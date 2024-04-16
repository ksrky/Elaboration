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
    [ MixfixOp (Name "Neg") [Operator "-", Operand 75]
    , MixfixOp (Name "IfThenElse") [Operator "if", Operand 30, Operator "then", Operand 30, Operator "else", Operand 30]
    , MixfixOp (Name "IfThen") [Operator "if", Operand 30, Operator "then", Operand 30]
    , MixfixOp (Name "Paren") [Operator "(", Operand 0, Operator ")"]
    , MixfixOp (Name "Unit") [Operator "(", Operator ")"]
    , MixfixOp (Name "Tuple") [Operand 11, Operator ",", Operand 10]
    , MixfixOp (Name "Or") [Operand 30, Operator "||", Operand 31]
    , MixfixOp (Name "And") [Operand 35, Operator "&&", Operand 36]
    , MixfixOp (Name "Eq") [Operand 50, Operator "==", Operand 50]
    , MixfixOp (Name "Add") [Operand 65, Operator "+", Operand 66]
    , MixfixOp (Name "Sub") [Operand 65, Operator "-", Operand 66]
    , MixfixOp (Name "Mul") [Operand 70, Operator "*", Operand 71]
    , MixfixOp (Name "Subscript") [Operand 100, Operator "[", Operand 0, Operator "]"]
    ]

sample :: IO ()
sample = do
    let _ = [syntax| :65 "+" :70 |] :: [Oper String]
    let parserTable = initParserTable mixfixOps
    case runParserM parserTable parse ["f", "x"] of
        Left err -> putStrLn err
        Right s  -> print $ V.head $ s ^. stxStack
