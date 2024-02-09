{-# LANGUAGE OverloadedStrings #-}

module Lib.Parser (pRaw) where

import Control.Applicative        hiding (many)
import Data.Char
import Data.Function
import Data.Int
import Data.Ord
import Data.Text                  hiding (empty)
import Data.Void
import Lib.Raw
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

mkSrcPos :: SourcePos -> SrcPos
mkSrcPos (SourcePos _ l c) = (unPos l, unPos c)

sc :: Parser ()
sc = L.space
        space1
        (L.skipLineComment "//")
        (L.skipBlockComment "/-" "-/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

charL :: Char -> Parser Char
charL = lexeme . char

stringL :: Text -> Parser Text
stringL = lexeme . string

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

ifPrec :: Int -> Int -> Parser a -> Parser a
ifPrec p1 p2 p = if p1 <= p2 then p else empty

pName :: Parser Name
pName = lexeme ((:) <$> letterChar <*> many alphaNumChar) <?> "Name"

withSrcPos :: Parser Raw -> Parser Raw
withSrcPos p = RSrcPos <$> (mkSrcPos <$> getSourcePos) <*> p

pRVar :: Parser Raw
pRVar = RVar <$> pName <?> "RVar"

pRLam :: Parser Raw
pRLam = RLam <$> (charL 'λ' *> pName <* charL '→') <*> pRaw 0 <?> "RLam"

pRApp :: Parser Raw
pRApp = RApp <$> pRaw 2 <*> pRaw 1 <?> "RApp"

pRU :: Parser Raw
pRU = RU <$ charL 'U' <?> "RU"

pRPi :: Parser Raw
pRPi = RPi <$> parens (pName <* charL ':') <*> pRaw 1 <* char '→' <*> pRaw 1 <?> "RPi"

pRLet :: Parser Raw
pRLet = RLet <$> (stringL "let" *> pName <* char ':')
    <*> pRaw 1 <*> (stringL "=" *> pRaw 0 <* stringL "in") <*> pRaw 0 <?> "RLet"

pRHole :: Parser Raw
pRHole = RHole <$ charL '_' <?> "RHole"

pAtom :: Parser Raw
pAtom = pRU <|> pRHole <|> try pRVar <|> try (parens (pRaw 0))

pRaw :: Int -> Parser Raw
pRaw p = (lexeme . withSrcPos) (
    try (ifPrec p 2 pRApp)
    <|> ifPrec p 0 pRLet
    <|> try (ifPrec p 1 pRPi))
    <|> pAtom
    <|> pRLam
    <?> "RRaw"
