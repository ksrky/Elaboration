{-# LANGUAGE OverloadedRecordDot #-}

module Parse
    ( OpParser(..)
    , OpTable(..)
    , runParserM
    , parse
    , test
    ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map.Strict      qualified as M

newtype Name = Name String
    deriving (Eq, Show)

type BindingPower = Int

data OpParser t = OpParser
    { name     :: Name
    , symbols  :: [t]
    , bindPows :: [BindingPower]
    }

type LeadingOpParser = OpParser
type TrailingOpParser = OpParser

data OpTable t = OpTable
    { leadingOps  :: M.Map t [LeadingOpParser t]
    , trailingOps :: M.Map t [TrailingOpParser t]
    }

type ParserM t = ReaderT (OpTable t) (StateT [t] (Except String))

type ParserFn t = OpParser t -> ParserM t Syntax

runParserM :: ParserM t a -> OpTable t -> [t] -> IO a
runParserM m table toks = case runExcept (evalStateT (runReaderT m table) toks) of
    Left msg  -> putStrLn msg >> error "unexpected"
    Right res -> return res

class (Show t, Ord t) => Token t where
    tokenString :: t -> String
    tokenString = show

instance Token String where
    tokenString = id

nextToken :: ParserM t t
nextToken = do
    toks <- get
    case toks of
        x : xs -> put xs >> return x
        []     -> throwError "eof"

nextToken_ :: ParserM t ()
nextToken_ = void nextToken

peekToken :: ParserM t t
peekToken = do
    toks <- get
    case toks of
        x : _ -> return x
        []    -> throwError "eof"

matchToken :: (t -> Bool) -> ParserM t ()
matchToken p = do
    tok <- nextToken
    if p tok
        then return ()
        else throwError "tokens unmatched"

getLeadingOpParsers :: Ord t => t -> ParserM t [LeadingOpParser t]
getLeadingOpParsers tok = do
    lops <- asks leadingOps
    return $ concat $ M.lookup tok lops

getTrailingOpParsers :: Ord t => t -> ParserM t [TrailingOpParser t]
getTrailingOpParsers tok = do
    tops <- asks trailingOps
    return $ concat $ M.lookup tok tops

data Syntax = Node Name [Syntax] | Atom String
    deriving (Eq, Show)

mkAtom :: Token t => t -> Syntax
mkAtom inp = Atom $ tokenString inp

parseOpRest :: Token t => BindingPower -> [t] -> [BindingPower] -> ParserM t [Syntax]
parseOpRest curbp _ (bp : _) | bp < curbp = throwError "lower bp"
parseOpRest _ [] (bp : _) = do
    stx <- parseLeading bp
    return [stx]
parseOpRest curbp (sym : syms) (bp : bps) = do
    stx <- parseLeading bp
    matchToken (sym ==)
    (stx :) <$> parseOpRest curbp syms bps
parseOpRest _ _ [] = return []

tryParsers :: forall t. Syntax -> [OpParser t] -> ParserFn t -> ParserM t Syntax
tryParsers def parsers parserFn = go parsers
  where
    go :: [OpParser t] -> ParserM t Syntax
    go [] = return def
    go (parser : rest) = do
        toks <- get
        parserFn parser `catchError` (\_ -> put toks >> go rest)

parseLeading :: Token t => BindingPower -> ParserM t Syntax
parseLeading curbp = do
    tok <- nextToken
    parsers <- getLeadingOpParsers tok
    lhs <- case parsers of
        [] -> return $ mkAtom tok
        _ -> tryParsers (mkAtom tok) parsers $ \parser -> do
            stxs <- parseOpRest curbp (tail parser.symbols) parser.bindPows
            return $ Node parser.name stxs
    parseTrailing curbp lhs

parseTrailing :: Token t => BindingPower -> Syntax -> ParserM t Syntax
parseTrailing curbp lhs = do
    tok <- peekToken
    parsers <- getTrailingOpParsers tok
    case parsers of
        [] -> return lhs
        _ -> tryParsers (mkAtom tok) parsers $ \parser -> do
            when (head parser.bindPows < curbp) $ throwError "lower bp"
            nextToken_
            stxs <- parseOpRest curbp (tail parser.symbols) (tail parser.bindPows)
            let lhs' = Node parser.name (lhs : stxs)
            parseTrailing curbp lhs'
    `catchError` (\_ -> return lhs)

parse :: Token t => ParserM t Syntax
parse = parseLeading 0

sampleOpTable :: OpTable String
sampleOpTable = OpTable
    { leadingOps = M.fromList
        [ ("-", [OpParser{name = Name "Minus", symbols = ["-"], bindPows = [75]}])
        ]
    , trailingOps = M.fromList
        [ ("+", [OpParser{name = Name "Add", symbols = ["+"], bindPows = [65, 66]}])
        , ("-", [OpParser{name = Name "Sub", symbols = ["-"], bindPows = [65, 66]}])
        , ("*", [OpParser{name = Name "Mul", symbols = ["*"], bindPows = [70, 71]}])
        ]
    }

test :: IO Syntax
test = runParserM parse sampleOpTable ["1", "+", "2", "*", "-", "3"]
