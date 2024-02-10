{-# LANGUAGE OverloadedStrings #-}

module Lib.Parser (pRaw, parseProg) where

import Control.Applicative
import Control.Exception.Safe     hiding (try)
import Control.Monad
import Data.Char
import Data.Either
import Data.Function
import Data.List                  qualified as List
import Data.Text                  hiding (empty)
import Data.Tuple
import Data.Void
import Lib.Common
import Lib.Raw
import Text.Megaparsec            hiding (many, some)
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

pName :: Parser Name
pName = do
    x <- lexeme ((:) <$> letterChar <*> many alphaNumChar) <?> "Name"
    when (x `List.elem` ["let", "in", "λ", "U"]) empty
    return x

withSrcPos :: Parser Raw -> Parser Raw
withSrcPos p = RSrcPos <$> (mkSrcPos <$> getSourcePos) <*> p

pRVar :: Parser Raw
pRVar = RVar <$> pName <?> "RVar"

pLamBinder :: Parser (Name, ArgInfo)
pLamBinder =
    (,Right Expl) <$> pName
    <|> try (do
        _ <- charL '{'
        x <- pName
        _ <- charL '='
        y <- pName
        _ <- charL '}'
        pure (y, Left x))
    <|> (,Right Impl) <$> (charL '{' *> pName <* charL '}')
    <?> "LamBinder"

pRLam :: Parser Raw
pRLam = do
    _ <- charL 'λ'
    xs <- some pLamBinder
    _ <- charL '→'
    r <- pRaw
    return (List.foldr (uncurry RLam) r xs) <?> "RLam"

pArg :: Parser (Raw, ArgInfo)
pArg =
    (,Right Expl) <$> pAtom
    <|> try ((,Right Impl) <$> (charL '{' *> pRaw <* charL '}'))
    <|> (do
        _ <- charL '{'
        x <- pName
        _ <- charL '='
        r <- pRaw
        _ <- charL '}'
        pure (r, Left x))
    <?> "Arg"

pRApp :: Parser Raw
pRApp = do
    f <- pAtom
    as <- some pArg
    return (List.foldl (uncurry . RApp) f as) <?> "RApp"

pRU :: Parser Raw
pRU = RU <$ charL 'U' <?> "RU"

pPiBinder :: Parser ((Name, Raw), Icit)
pPiBinder =
    (,Expl) <$> ((,) <$> (charL '(' *> pName <* charL ':') <*> pRaw <* charL ')')
    <|> (,Impl) <$> ((,) <$> (charL '{' *> pName <* charL ':') <*> pRaw <* charL '}')
    <?> "PiBinder"

pRPi :: Parser Raw
pRPi = do
    ((x, a), i) <- pPiBinder
    _ <- charL '→'
    b <- pRaw
    return (RPi x i a b)  <?> "RPi"

pRLet :: Parser Raw
pRLet = RLet <$> (stringL "let" *> pName) <*> (charL ':' *> pRaw)
    <*> (stringL "=" *> pRaw) <*> (stringL "in" *> pRaw) <?> "RLet"

pRHole :: Parser Raw
pRHole = RHole <$ charL '_' <?> "RHole"

pAtom :: Parser Raw
pAtom = pRU <|> pRVar <|> pRHole <|> parens pRaw

pRaw :: Parser Raw
pRaw = (lexeme . withSrcPos) (
    try pRLet
    <|> try pRLam
    <|> try pRApp
    <|> try pRPi
    <|> try pAtom)
    <?> "Raw"

parseProg :: MonadThrow m => Text -> m Raw
parseProg inp = case runParser (pRaw <* eof) "main" inp of
    Left e  -> throwString $ errorBundlePretty e
    Right r -> return r
