module Parser.Pratt where

import Control.Applicative
import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.Except
import Control.Monad.Logic
import Control.Monad.Reader.Class
import Control.Monad.State
import Data.Either
import Data.List                  qualified as L
import Data.Map.Strict            qualified as M
import Data.Vector                qualified as V
import Debug.Trace
import Parser.Monad
import Parser.Types

pushSyntax :: Syntax -> ParserM t ()
pushSyntax stx = stxStack %= V.cons stx

popSyntax :: ParserM t Syntax
popSyntax = do
    stx <- use $ stxStack.to V.head
    stxStack %= V.tail
    return stx

getLeadingParsers :: Token t => t -> ParserM t [LeadingParser t]
getLeadingParsers tok = do
    lps <- asks leadingParsers
    return $ concat $ M.lookup tok lps

getTrailingParsers :: Token t => t -> ParserM t [TrailingParser t]
getTrailingParsers tok = do
    tps <- asks trailingParsers
    return $ concat $ M.lookup tok tps

longestMatch :: ParserLogic t -> ParserM t ()
longestMatch m = do
    let parsers = observeAll m
    when (null parsers) $ throwError "no parsers"
    st <- get
    sts <- lift $ sequence $ parsers ?? st
    let st' = head $ L.sortOn (\s -> length (s ^. tokens)) sts
    put st'

tryParsers :: [Parser t] -> ParserLogic t
tryParsers = foldr ((<|>) . pure) empty

mkAtom :: Token t => t -> ParserM t ()
mkAtom = pushSyntax . Atom . tokenString

mkNode :: Name -> Int -> ParserM t ()
mkNode name = mkNode' []
  where
    mkNode' :: [Syntax] -> Int -> ParserM t ()
    mkNode' stxs n
        | n == 0 = pushSyntax $ Node name stxs
        | otherwise = do
            stx <- popSyntax
            mkNode' (stx : stxs) (n - 1)

parseLeading :: Token t => ParserM t ()
parseLeading = do
    tok <- nextToken
    parsers <- getLeadingParsers tok
    do
        longestMatch $ tryParsers parsers
        `catchError` (\_ -> mkAtom tok)
    parseTrailing

parseTrailing :: Token t => ParserM t ()
parseTrailing = do
    tok <- peekToken
    parsers <- getTrailingParsers tok
    longestMatch $ tryParsers parsers
    `catchError` (\e -> do
        toks <- use tokens
        stxs <- use stxStack
        traceShowM (e, toks, stxs)
        return ())

parse :: Token t => Parser t
parse = execStateT parseLeading

parseOpExps :: Token t => [Oper t] -> ParserM t ()
parseOpExps oes = forM_ oes $ \case
    Operator tok -> matchToken (tok ==)
    Operand bp -> bindPow .= bp >> parseLeading

insertParser :: Token t => (t, Parser t) -> M.Map t [Parser t] -> M.Map t [Parser t]
insertParser (k, p) tbl =
    case M.lookup k tbl of
        Nothing  -> M.insert k [p] tbl
        Just ps' -> M.insert k (p : ps') tbl

type EitherParser t = Either (M.Map t [Parser t] -> M.Map t [Parser t]) (M.Map t [Parser t] -> M.Map t [Parser t])

insertMixfixOp :: Token t => MixfixOp t -> EitherParser t
insertMixfixOp MixfixOp{name, opers = opers@(Operator tok0 : _)} = do
    let arity = length $ filter (\case Operand _ -> True; _ -> False) opers
        parser = execStateT $ do
            bp <- use bindPow
            parseOpExps opers
            bindPow .= bp
            mkNode name arity
    Left $ insertParser (tok0, parser)
insertMixfixOp MixfixOp{name, opers = opers@(Operand bp0 : Operator tok1 : opers')} = do
    let arity = length $ filter (\case Operand _ -> True; _ -> False) opers
        parser = execStateT $ do
            bp <- use bindPow
            when (bp0 < bp) $ throwError "lower bp"
            nextToken_
            parseOpExps opers'
            bindPow .= bp
            mkNode name arity
            parseTrailing
    Right$ insertParser (tok1, parser)
insertMixfixOp _ = error "invalid mixfix op"

insertMixfixOps :: Token t => [MixfixOp t] -> ParserTable t -> ParserTable t
insertMixfixOps mixfixOps tbl = do
    let (lps, tps) = partitionEithers $ map insertMixfixOp mixfixOps
    ParserTable
        { leadingParsers = foldr ($) (leadingParsers tbl) lps
        , trailingParsers = foldr ($) (trailingParsers tbl) tps
        }

{-
groupParsers :: forall t. Token t => [(t, Parser t)] -> [(t, [Parser t])]
groupParsers = go . L.sortOn fst
  where
    go :: [(t, Parser t)] -> [(t, [Parser t])]
    go [] = []
    go ((k, v) : xs) = (k, v : map snd ys) : go zs
        where (ys, zs) = span ((k ==) . fst) xs
-}
