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
mkNode name baseSz = mkNode' []
  where
    mkNode' :: [Syntax] -> ParserM t ()
    mkNode' stxs = do
        sz <- use $ tokens.to length
        if sz == baseSz then pushSyntax $ Node name stxs
        else do
            stx <- popSyntax
            mkNode' (stx : stxs)

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
    `catchError` (\_ -> return ())

parse :: Token t => Parser t
parse = execStateT parseLeading

parseOpExps :: Token t => [OpExp t] -> ParserM t ()
parseOpExps oes = forM_ oes $ \case
    Op tok -> matchToken (tok ==)
    Exp bp -> bindPow .= bp >> parseLeading

insertParser :: Token t => (t, Parser t) -> M.Map t [Parser t] -> M.Map t [Parser t]
insertParser (k, p) tbl =
    case M.lookup k tbl of
        Nothing  -> M.insert k [p] tbl
        Just ps' -> M.insert k (p : ps') tbl

type EitherParser t = Either (M.Map t [Parser t] -> M.Map t [Parser t]) (M.Map t [Parser t] -> M.Map t [Parser t])

insertMixfixOp :: Token t => MixfixOp t -> EitherParser t
insertMixfixOp MixfixOp{name, opExps = oes@(Op op0 : _)} = do
    let parser = execStateT $ do
            baseSz <- use $ tokens.to length
            parseOpExps oes
            mkNode name baseSz
    Left $ insertParser (op0, parser)
insertMixfixOp MixfixOp{name, opExps = Exp bp0 : Op op1 : oes} = do
    let parser = execStateT $ do
            bp <- use bindPow
            when (bp0 < bp) $ throwError "lower bp"
            nextToken_
            baseSz <- use $ tokens.to length
            parseOpExps oes
            mkNode name (baseSz - 1)
            parseTrailing
    Right$ insertParser (op1, parser)
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
