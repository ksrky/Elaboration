module Parser.TH (syntax) where

import Control.Applicative
import Data.Functor
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Parser.Types
import Text.ParserCombinators.ReadP

-- [syntax| :65 "+" :70]

pInt :: ReadP Int
pInt = munch1 (`elem` ['0'..'9']) <&> read

pOperator :: ReadP (Oper String)
pOperator = do
    _ <- char '"'
    op <- munch1 (/= '"')
    _ <- char '"'
    return $ Operator op

pOperand :: ReadP (Oper String)
pOperand = do
    _ <- char ':'
    Operand <$> pInt

pOpExp :: ReadP (Oper String)
pOpExp = pOperator +++ pOperand

ppOpExps :: ReadP [Oper String]
ppOpExps = some (skipSpaces >> pOpExp) <* eof

parseSyntaxDecl :: String -> [Oper String]
parseSyntaxDecl s = case readP_to_S ppOpExps s of
    [(oes, "")] -> oes
    _           -> fail "parse error"

syntax :: QuasiQuoter
syntax = QuasiQuoter
    { quoteExp  = lift . parseSyntaxDecl
    , quotePat  = error "Usage as a parttern is not supported"
    , quoteType = error "Usage as a type is not supported"
    , quoteDec  = error "Usage as a declaration is not supported" }
