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

type BindingPower = Int

data OpParser t = OpParser
    { name     :: String
    , symbols  :: [t]
    , bindPows :: [BindingPower]
    }

type LeadingOpParser = OpParser
type TrailingOpParser = OpParser

data OpTable t = OpTable
    { leadingOps  :: M.Map t (LeadingOpParser t)
    , trailingOps :: M.Map t (TrailingOpParser t)
    }

type ParserM t = ReaderT (OpTable t) (StateT [t] (Except String))

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

getLeadingOpParser :: Ord t => t -> ParserM t (Maybe (LeadingOpParser t))
getLeadingOpParser tok = do
    lops <- asks leadingOps
    return $ M.lookup tok lops

getTrailingOpParser :: Ord t => t -> ParserM t (Maybe (LeadingOpParser t))
getTrailingOpParser tok = do
    lops <- asks trailingOps
    return $ M.lookup tok lops

data Syntax = Node String [Syntax] | Atom String | Missing deriving (Eq, Show)

parseAtom :: Token t => t -> ParserM t Syntax
parseAtom inp = return $ Atom $ tokenString inp

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

parseLeading :: Token t => BindingPower -> ParserM t Syntax
parseLeading curbp = do
    tok <- nextToken
    mb_parser <- getLeadingOpParser tok
    lhs <- case mb_parser of
        Nothing -> parseAtom tok
        Just parser -> do
            stxs <- parseOpRest curbp (tail parser.symbols) parser.bindPows
            return $ Node parser.name stxs
        `catchError` (\_ -> parseAtom tok) -- TODO: multimap, backtrack
    parseTrailing curbp lhs

parseTrailing :: Token t => BindingPower -> Syntax -> ParserM t Syntax
parseTrailing curbp lhs = do
    tok <- peekToken
    mb_parser <- getTrailingOpParser tok
    case mb_parser of
        Nothing     -> return lhs
        Just parser -> do
            when (head parser.bindPows < curbp) $ throwError "lower bp"
            nextToken_
            stxs <- parseOpRest curbp (tail parser.symbols) (tail parser.bindPows)
            let lhs' = Node parser.name (lhs : stxs)
            parseTrailing curbp lhs'
    `catchError` (\_ -> return lhs) -- TODO: multimap, backtrack

parse :: Token t => ParserM t Syntax
parse = parseLeading 0

sampleOpTable :: OpTable String
sampleOpTable = OpTable
    { leadingOps   = M.fromList
        [ ("-", OpParser{name = "Minus", symbols = ["-"], bindPows = [75]})
        ]
    , trailingOps = M.fromList
        [ ("+", OpParser{name="Add", symbols=["+"], bindPows = [65, 66]})
        , ("-", OpParser{name="Sub", symbols=["-"], bindPows = [65, 66]})
        , ("*", OpParser{name="Mul", symbols=["*"], bindPows = [70, 71]})
        ]
    }

test :: IO Syntax
test = runParserM parse sampleOpTable ["1", "+", "2", "+", "3"]
