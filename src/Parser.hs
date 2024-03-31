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

withSrcPos :: Parser Raw -> Parser Raw
withSrcPos p = SrcPos . mkSrcPos <$> getSourcePos <*> p

pVar :: Parser Raw
pVar = Var <$> pName <?> "Var"

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

pLam :: Parser Raw
pLam = do
    _ <- charL 'λ'
    xs <- some pLamBinder
    _ <- charL '→'
    r <- pRaw
    return (foldr (uncurry Lam) r xs) <?> "Lam"

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

pApp :: Parser Raw
pApp = do
    f <- pAtom
    as <- some pArg
    return (foldl (uncurry . App) f as) <?> "App"

pU :: Parser Raw
pU = U <$ charL 'U' <?> "U"

pPiBinder :: Parser ((Name, Raw), Icit)
pPiBinder =
    (,Expl) <$> ((,) <$> (charL '(' *> pName <* charL ':') <*> pRaw <* charL ')')
    <|> (,Impl) <$> ((,) <$> (charL '{' *> pName <* charL ':') <*> pRaw <* charL '}')
    <?> "PiBinder"

pPi :: Parser Raw
pPi = do
    ((x, a), i) <- pPiBinder
    _ <- charL '→'
    b <- pRaw
    return (Pi x i a b)  <?> "Pi"

pLet :: Parser Raw
pLet = Let <$> (stringL "let" *> pName) <*> (charL ':' *> pRaw)
    <*> (stringL "=" *> pRaw) <*> (stringL "in" *> pRaw) <?> "Let"

pHole :: Parser Raw
pHole = Hole <$ charL '_' <?> "Hole"

pAtom :: Parser Raw
pAtom = pU <|> pVar <|> pHole <|> parens pRaw

pRaw :: Parser Raw
pRaw = (lexeme . withSrcPos) (
    try pLet
    <|> try pLam
    <|> try pApp
    <|> try pPi
    <|> try pAtom)
    <?> "Raw"

parseProg :: MonadThrow m => Text -> m Raw
parseProg inp = case runParser (pRaw <* eof) "main" inp of
    Left e  -> throwString $ errorBundlePretty e
    Right r -> return r
