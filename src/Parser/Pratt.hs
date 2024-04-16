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

longestMatch :: [Parser t] -> ParserM t ()
longestMatch parsers = do
    st <- get
    sts <- lift $ observeAllT $ tryParsers parsers st
    when (null sts) $ throwError "no match parsers"
    let st' = head $ L.sortOn (^. tokens.to length) sts
    put st'

tryParsers :: [Parser t] -> ParserState t -> ParserLogic t (ParserState t)
tryParsers parsers st = foldr (\p -> (lift (p st) `catchError` const empty <|>)) empty parsers

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
    longestMatch parsers `catchError` (\_ -> mkAtom tok)
    parseTrailing

parseTrailing :: Token t => ParserM t ()
parseTrailing = do
    tok <- peekToken
    parsers <- getTrailingParsers tok
    longestMatch parsers
    `catchError` (\_ -> return ())

parse :: Token t => Parser t
parse = execStateT parseLeading

parseOpExps :: Token t => [Oper t] -> ParserM t ()
parseOpExps oes = forM_ oes $ \case
    Operator tok -> matchToken (tok ==)
    Operand bp -> bindPow .= bp >> parseLeading

insertParser :: Token t => (t, Parser t) -> M.Map t [Parser t] -> M.Map t [Parser t]
insertParser (k, p) tbl = case M.lookup k tbl of
    Nothing  -> M.insert k [p] tbl
    Just ps' -> M.insert k (p : ps') tbl

insertMixfixOp :: Token t => MixfixOp t -> Either (t, Parser t) (t, Parser t)
insertMixfixOp MixfixOp{name, opers = Operator tok0 : opers} = do
    let arity = length $ filter (\case Operand _ -> True; _ -> False) opers
        parser = execStateT $ do
            bp <- use bindPow
            parseOpExps opers
            bindPow .= bp
            mkNode name arity
    Left (tok0, parser)
insertMixfixOp MixfixOp{name, opers = Operand bp0 : Operator tok1 : opers} = do
    let arity = 1 + length (filter (\case Operand _ -> True; _ -> False) opers)
        parser = execStateT $ do
            bp <- use bindPow
            when (bp0 < bp) $ throwError "lower bp"
            nextToken_
            parseOpExps opers
            bindPow .= bp
            mkNode name arity
            parseTrailing
    Right (tok1, parser)
insertMixfixOp _ = error "invalid mixfix op"

initParserTable :: Token t => [MixfixOp t] -> ParserTable t
initParserTable mixfixOps = do
    let (lps, tps) = partitionEithers $ map insertMixfixOp mixfixOps
    ParserTable
        { leadingParsers = M.fromList $ groupParsers lps
        , trailingParsers = M.fromList $ groupParsers tps
        }

groupParsers :: forall t. Token t => [(t, Parser t)] -> [(t, [Parser t])]
groupParsers = group . L.sortOn fst
  where
    group :: [(t, Parser t)] -> [(t, [Parser t])]
    group [] = []
    group ((k, v) : xs) = (k, v : map snd ys) : group zs
        where (ys, zs) = span ((k ==) . fst) xs
