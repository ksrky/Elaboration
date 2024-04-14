{-# LANGUAGE OverloadedRecordDot #-}

module Parser.Expr
    ( OpParser(..)
    , OpTable(..)
    , runParserM
    , parse
    , test
    ) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Logic
import Control.Monad.Reader
import Control.Monad.State
import Data.List            qualified as L
import Data.Map.Strict      qualified as M

-- | Parser name, which can be a node label of the syntax tree.
newtype Name = Name String
    deriving (Eq, Show)

-- | Each expression expected by the operator has a binding power.
type BindingPower = Int

-- | Operator parser has a name, a list of symbols, and a list of binding powers.
data OpParser t = OpParser
    { name     :: Name
    , symbols  :: [t]
    , bindPows :: [BindingPower]
    }
    deriving (Show)

-- | Leading operator parser expects an operator on the first token.
-- For example, a prefix operator, if-then-else, etc.
type LeadingOpParser = OpParser

-- | Trailing operator parser expects an operator on the second or later token.
-- For example, infix operators, postfix operators, array's subscript, etc.
type TrailingOpParser = OpParser

-- | Operator table distinguishes between leading and trailing operator parsers.
data OpTable t = OpTable
    { leadingOps  :: M.Map t [LeadingOpParser t]
    , trailingOps :: M.Map t [TrailingOpParser t]
    }

-- * Monads

-- | Parser logic monad.
type ParserLogicM t m = StateT [t] (LogicT m)

runParserLogicM :: Applicative m => ParserLogicM t m a -> [t] -> m [(a, [t])]
runParserLogicM m toks = observeAllT (runStateT m toks)

liftParserLogicM :: Monad m => m a -> ParserLogicM t m a
liftParserLogicM = lift . lift

-- | Parser monad has operator table, token stream, and error handling.
type ParserM t = ParserLogicM t (ReaderT (OpTable t) (Except String))

runParserM :: Token t => ParserM t a -> [t] -> OpTable t -> IO a
runParserM m toks table = case runExcept (runReaderT (runParserLogicM m toks) table) of
    Left msg             -> error msg
    Right []             -> error "no result"
    Right [(res, [])]    -> return res
    Right [(_, tok : _)] -> error $ "parse error at " ++ show tok
    Right _              -> error "multiple results"

type ParserFn t a = OpParser t -> ParserM t a

-- * Token

class (Show t, Ord t) => Token t where
    tokenString :: t -> String
    tokenString = show

instance Token String where
    tokenString = id

-- ** Operation on token stream

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

-- ** Parser utilities

-- | Extracts the longest result from 'ParserLogicM'.
longestMatch :: ParserM t a -> ParserM t a
longestMatch m = do
    toks <- get
    xtoks <- liftParserLogicM $ runParserLogicM m toks
    when (null xtoks) $ throwError "no parsers"
    let (x, toks') = L.minimumBy (\(_, ts1) (_, ts2) -> compare (length ts1) (length ts2)) xtoks
    put toks'
    return x

-- | Tries mutiple parsers in 'ParserLogicM'.
tryParsers :: [OpParser t] -> ParserFn t a -> ParserM t a
tryParsers parsers parserFn = do
    parser <- foldr ((<|>) . pure) empty parsers
    parserFn parser `catchError` const empty

getLeadingOpParsers :: Token t => t -> ParserM t [LeadingOpParser t]
getLeadingOpParsers tok = do
    lops <- asks leadingOps
    return $ concat $ M.lookup tok lops

getTrailingOpParsers :: Token t => t -> ParserM t [TrailingOpParser t]
getTrailingOpParsers tok = do
    tops <- asks trailingOps
    return $ concat $ M.lookup tok tops

-- * Syntax

-- | Generalized AST.
data Syntax
    = -- | @Name@ corresponds to a data constructor of the AST
      -- and @[Syntax]@ is its field.
      Node Name [Syntax]
    | -- | Identifier or literal.
      Atom String
    deriving (Eq)

instance Show Syntax where
    show (Node (Name name) stxs) = name ++ " [" ++ L.intercalate ", " (map show stxs) ++ "]"
    show (Atom str)              = show str

-- | Construct an atom from a token.
mkAtom :: Token t => t -> Syntax
mkAtom = Atom . tokenString

-- ** Parser combinators

parseRestOps :: Token t => [t] -> [BindingPower] -> ParserM t [Syntax]
parseRestOps [] bps = mapM parseLeading bps
parseRestOps syms [] = do
    mapM_ (matchToken  . (==)) syms
    return []
parseRestOps (sym : syms) (bp : bps) = do
    stx <- parseLeading bp
    matchToken (sym ==)
    (stx :) <$> parseRestOps syms bps

parseLeading :: Token t => BindingPower -> ParserM t Syntax
parseLeading bp = do
    tok <- nextToken
    parsers <- getLeadingOpParsers tok
    lhs <- do
        longestMatch $ tryParsers parsers $ \parser -> do
            stxs <- parseRestOps (tail parser.symbols) parser.bindPows
            return $ Node parser.name stxs
        `catchError` (\_ -> return $ mkAtom tok)
    parseTrailing bp lhs

parseTrailing :: Token t => BindingPower -> Syntax -> ParserM t Syntax
parseTrailing bp lhs = do
    tok <- peekToken
    parsers <- getTrailingOpParsers tok
    longestMatch $ tryParsers parsers $ \parser -> do
        when (head parser.bindPows < bp) $ throwError "lower bp"
        nextToken_
        stxs <- parseRestOps (tail parser.symbols) (tail parser.bindPows)
        let lhs' = Node parser.name (lhs : stxs)
        parseTrailing bp lhs'
    `catchError` (\_ -> return lhs)

parse :: Token t => ParserM t Syntax
parse = parseLeading 0

-- * Samples

sampleOpTable :: OpTable String
sampleOpTable = OpTable
    { leadingOps = M.fromList
        [ ("-",  [OpParser{name = Name "Minus", symbols = ["-"], bindPows = [75]}])
        , ("if", [ OpParser{name = Name "IfThenElse", symbols = ["if", "then", "else"], bindPows = [30, 30, 30]}
                 , OpParser{name = Name "IfThen", symbols = ["if", "then"], bindPows = [30, 30]}
                 ])
        , ("(",  [ OpParser{name = Name "Paren", symbols = ["(", ")"], bindPows = [0]}
                 , OpParser{name = Name "Unit", symbols = ["(", ")"], bindPows = []}])
        ]
    , trailingOps = M.fromList
        [ (",",  [OpParser{name = Name "Tuple", symbols = [","], bindPows = [11, 10]}])
        , ("||", [OpParser{name = Name "Or", symbols = ["||"], bindPows = [30, 31]}])
        , ("&&", [OpParser{name = Name "And", symbols = ["&&"], bindPows = [35, 36]}])
        , ("==", [OpParser{name = Name "Eq", symbols = ["=="], bindPows = [50, 50]}])
        , ("+",  [OpParser{name = Name "Add", symbols = ["+"], bindPows = [65, 66]}])
        , ("-",  [OpParser{name = Name "Sub", symbols = ["-"], bindPows = [65, 66]}])
        , ("*",  [OpParser{name = Name "Mul", symbols = ["*"], bindPows = [70, 71]}])
        , ("[",  [OpParser{name = Name "Subscript", symbols = ["[", "]"], bindPows = [100, 0]}])
        ]
    }

test :: IO Syntax
test = runParserM parse [] sampleOpTable

-- ["1", "+", "2", "*", "-", "3"]
-- ["(", "1", "+", "2", ")", "*", "3"]
-- ["if", "1", "==", "2", "then", "3", "+", "4", "else", "5"]
-- ["1", "*", "(", "x", "+", "y", ")"]
-- ["x", "+", "a", "[", "2", "]"]
-- ["if", "1", "==", "2", "then", "if", "3", "then", "4", "else", "5"]
-- ["if", "1", "==", "2", "then", "3", "else", "if", "4", "then", "5", "else", "6"]
-- ["x", ",", "y", ",", "z"]
-- ["1", "-", "-", "2"]
-- ["(", ")"]
