{-# LANGUAGE OverloadedStrings #-}

module Lib.Parser (pRaw) where

import Control.Applicative        hiding (many, some)
import Control.Monad
import Data.Char
import Data.Either
import Data.Function
import Data.Int
import Data.List                  qualified as List
import Data.Ord
import Data.Text                  hiding (empty)
import Data.Tuple
import Data.Void
import Lib.Common
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
ifPrec p1 p2 p = if p1 < p2 then p else empty

pName :: Parser Name
pName = lexeme ((:) <$> letterChar <*> many alphaNumChar) <?> "Name"

withSrcPos :: Parser Raw -> Parser Raw
withSrcPos p = RSrcPos <$> (mkSrcPos <$> getSourcePos) <*> p

pRVar :: Parser Raw
pRVar = RVar <$> pName <?> "RVar"

pLamBinder :: Parser (Name, ArgInfo)
pLamBinder =
    try ((,Right Expl) <$> pName)
    <|> try ((,Right Impl) <$> (charL '{' *> pName <* charL '}'))
    <|> (do
        _ <- charL '{'
        x <- pName
        _ <- charL '='
        y <- pName
        _ <- charL '}'
        pure (y, Left x))
    <?> "LamBinder"

pRLam :: Parser Raw
pRLam = do
    _ <- charL 'λ'
    xs <- some pLamBinder
    _ <- charL '→'
    r <- pRaw 0
    return (List.foldr (uncurry RLam) r xs) <?> "RLam"

pArg :: Parser (Raw, ArgInfo)
pArg =
    try ((,Right Expl) <$> pRaw 8)
    <|> try ((,Right Impl) <$> (charL '{' *> pRaw 0 <* charL '}'))
    <|> (do
        _ <- charL '{'
        x <- pName
        _ <- charL '='
        r <- pRaw 0
        _ <- charL '}'
        pure (r, Left x))
    <?> "Arg"

pRApp :: Parser Raw
pRApp = do
    f <- pRaw 9
    (a, i) <- pArg
    return (RApp f a i) <?> "RApp"

pRU :: Parser Raw
pRU = RU <$ charL 'U' <?> "RU"

pRPi :: Parser Raw
pRPi =
    try (RPi <$> parens (pName <* charL ':') <*> pure Expl
        <*> pRaw 3 <* char '→' <*> pRaw 2)
    <|> (RPi <$> (charL '{' *> pName <* charL ':') <*> pure Impl
        <*> pRaw 3 <* char '→' <*> pRaw 2 <* charL '}')
     <?> "RPi"

pRLet :: Parser Raw
pRLet = RLet <$> (stringL "let" *> pName <* char ':')
    <*> pRaw 2 <*> (stringL "=" *> pRaw 0 <* stringL "in") <*> pRaw 0 <?> "RLet"

pRHole :: Parser Raw
pRHole = RHole <$ charL '_' <?> "RHole"

pRaw :: Int -> Parser Raw
pRaw p = (lexeme . withSrcPos) (
    try (ifPrec p 9 pRApp)
    <|> try (ifPrec p 3 pRPi))
    <|> ifPrec p 2 pRLam
    <|> ifPrec p 1 pRLet
    <|> pRU
    <|> pRHole
    <|> try pRVar
    <|> try (parens (pRaw 0))
    <?> "Raw"
