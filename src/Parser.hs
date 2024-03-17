{-# LANGUAGE OverloadedStrings #-}

module Parser (parseProg) where

import           Common
import           Control.Applicative        hiding (many, some)
import           Control.Exception.Safe     hiding (try)
import           Control.Monad
import           Data.Text                  (Text)
import           Data.Void
import           Raw
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

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
    when (x `elem` ["let", "in", "λ", "U"]) empty
    return x

pSrcPos :: Parser SrcPos
pSrcPos = mkSrcPos <$> getSourcePos <?> "SrcPos"

withSrcPos :: Parser Raw -> Parser Raw
withSrcPos parser = SrcPos <$> pSrcPos <*> parser

pVar :: Parser Raw
pVar = Var <$> pName <?> "Var"

pLamBinder :: Parser Name
pLamBinder =
    pName
    <?> "LamBinder"

pLam :: Parser Raw
pLam = do
    _ <- charL 'λ'
    xs <- some pLamBinder
    _ <- charL '→'
    r <- pRaw
    return (foldr Lam r xs)
    <?> "Lam"

pArg :: Parser Raw
pArg =
    pAtom
    <?> "Arg"

pApp :: Parser Raw
pApp = do
    f <- pAtom
    as <- some pArg
    return (foldl App f as) <?> "RApp"

pU :: Parser Raw
pU = U <$ charL 'U' <?> "U"

pPiBinder :: Parser (Name, Raw)
pPiBinder =
    ((,) <$> (charL '(' *> pName <* charL ':') <*> pRaw <* charL ')')
    <?> "PiBinder"

pPi :: Parser Raw
pPi =  do
    (x, a) <- pPiBinder
    _ <- charL '→'
    b <- pRaw
    return (Pi x a b)  <?> "RPi"

pLet :: Parser Raw
pLet = Let <$> (stringL "let" *> pName) <*> (charL ':' *> pRaw)
    <*> (stringL "=" *> pRaw) <*> (stringL "in" *> pRaw) <?> "RLet"

pAtom :: Parser Raw
pAtom = pU <|> pVar <|> parens pRaw

pRaw :: Parser Raw
pRaw = (lexeme . withSrcPos) (
    try pLet
    <|> try pLam
    <|> try pApp
    <|> pPi
    <|> pAtom)
    <?> "RRaw"

parseProg :: MonadThrow m => Text -> m Raw
parseProg inp = case runParser (pRaw <* eof) "main" inp of
    Left e  -> throwString $ errorBundlePretty e
    Right r -> return r
