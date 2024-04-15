{-# LANGUAGE QuasiQuotes #-}

module Parser.Sample where

import Control.Lens.Operators
import Data.Vector            qualified as V
import Parser.Monad
import Parser.Pratt
import Parser.TH
import Parser.Types

mixfixOps :: [MixfixOp String]
mixfixOps =
    [ MixfixOp (Name "Neg") [Op "-", Exp 75]
    , MixfixOp (Name "IfThenElse") [Op "if", Exp 30, Op "then", Exp 30, Op "else", Exp 30]
    , MixfixOp (Name "IfThen") [Op "if", Exp 30, Op "then", Exp 30]
    , MixfixOp (Name "Paren") [Op "(", Exp 0, Op ")"]
    , MixfixOp (Name "Unit") [Op "(", Op ")"]
    , MixfixOp (Name "Tuple") [Op "(", Exp 11, Op ",", Exp 10, Op ")"]
    , MixfixOp (Name "Or") [Exp 30, Op "||", Exp 31]
    , MixfixOp (Name "And") [Exp 35, Op "&&", Exp 36]
    , MixfixOp (Name "Eq") [Exp 50, Op "==", Exp 50]
    , MixfixOp (Name "Add") [Exp 65, Op "+", Exp 66]
    , MixfixOp (Name "Sub") [Exp 65, Op "-", Exp 66]
    , MixfixOp (Name "Mul") [Exp 70, Op "*", Exp 71]
    , MixfixOp (Name "Subscript") [Exp 100, Op "[", Exp 0, Op "]"]
    ]

sample :: IO ()
sample = do
    let _ = [syntax| :65 "+" :70 |] :: [OpExp String]
    let parserTable = insertMixfixOps mixfixOps emptyParserTable
    case runParserM parserTable parse ["1", "+", "2", "*", "3"] of
        Left err -> putStrLn err
        Right s  -> print $ V.head $ s ^. stxStack
