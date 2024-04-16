{-# LANGUAGE TemplateHaskell #-}

module Parser.Monad where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.Except
import Control.Monad.Logic
import Control.Monad.Reader
import Control.Monad.State
import Data.Map.Strict          qualified as M
import Data.Vector              qualified as V
import Parser.Types

data ParserState t = ParserState
    { _stxStack :: SyntaxStack
    , _tokens   :: [t]
    , _bindPow  :: BindingPower
    }

mkParserState :: [t] -> ParserState t
mkParserState toks = ParserState
    { _stxStack = V.empty
    , _tokens = toks
    , _bindPow = 0
    }

makeLenses ''ParserState

type ParserInnerM t = ReaderT (ParserTable t) (Except String)

runParserInnerM :: ParserTable t -> ParserInnerM t a -> Either String a
runParserInnerM tbl = runExcept . flip runReaderT tbl

type Parser t = ParserState t -> ParserInnerM t (ParserState t)

type ParserM t a = StateT (ParserState t) (ParserInnerM t) a

runParserM :: ParserTable t -> Parser t -> [t] -> Either String (ParserState t)
runParserM tbl p toks = runParserInnerM tbl (p (mkParserState toks))

type LeadingParser t = Parser t
type TrailingParser t = Parser t

data ParserTable t = ParserTable
    { leadingParsers  :: M.Map t [LeadingParser t]
    , trailingParsers :: M.Map t [TrailingParser t]
    }

emptyParserTable :: ParserTable t
emptyParserTable = ParserTable M.empty M.empty

type ParserLogic t = LogicT (ParserInnerM t)

nextToken :: ParserM t t
nextToken = do
    toks <- use tokens
    case toks of
        x : xs -> tokens .= xs >> return x
        []     -> throwError "eof"

nextToken_ :: ParserM t ()
nextToken_ = void nextToken

peekToken :: ParserM t t
peekToken = do
    toks <- use tokens
    case toks of
        x : _ -> return x
        []    -> throwError "eof"

matchToken :: (t -> Bool) -> ParserM t ()
matchToken p = do
    tok <- nextToken
    if p tok
        then return ()
        else throwError "tokens unmatched"
