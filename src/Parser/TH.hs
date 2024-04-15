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

pOp :: ReadP (OpExp String)
pOp = do
    _ <- char '"'
    op <- munch1 (/= '"')
    _ <- char '"'
    return $ Op op

pExp :: ReadP (OpExp String)
pExp = do
    _ <- char ':'
    Exp <$> pInt

pOpExp :: ReadP (OpExp String)
pOpExp = pOp +++ pExp

ppOpExps :: ReadP [OpExp String]
ppOpExps = some (skipSpaces >> pOpExp) <* eof

parseSyntaxDecl :: String -> [OpExp String]
parseSyntaxDecl s = case readP_to_S ppOpExps s of
    [(oes, "")] -> oes
    _           -> fail "parse error"

syntax :: QuasiQuoter
syntax = QuasiQuoter
    { quoteExp  = lift . parseSyntaxDecl
    , quotePat  = error "Usage as a parttern is not supported"
    , quoteType = error "Usage as a type is not supported"
    , quoteDec  = error "Usage as a declaration is not supported" }
