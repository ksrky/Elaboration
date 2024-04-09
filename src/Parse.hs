{-# LANGUAGE TemplateHaskell #-}

module Parse where

import Control.Monad.Except
import Control.Monad.State

type ParserM t a = StateT [t] (Except String) a

type BindingPower = Int

next :: ParserM t t
next = do
    toks <- get
    case toks of
        x : xs -> put xs >> return x
        []     -> throwError "no more tokens"

next_ :: ParserM t ()
next_ = void next

peek :: ParserM t t
peek = do
    toks <- get
    case toks of
        x : _ -> return x
        []    -> throwError "nothing to peek"

getPrefixBP :: String -> ParserM t BindingPower
getPrefixBP = undefined

getInfixBP :: String -> ParserM t (BindingPower, BindingPower)
getInfixBP = undefined

getPostfixBP :: String -> ParserM t BindingPower
getPostfixBP = undefined

data Token = TokAtom String | TokOp String | TokEof deriving (Eq, Show)

data Syntax = Node String [Syntax] | Atom String deriving (Eq, Show)

parsePrefix :: BindingPower -> ParserM Token Syntax
parsePrefix bp = do
    tok <- next
    lhs <- case tok of
        TokAtom x -> return $ Atom x
        TokOp op -> do
            rbp <- getPrefixBP op
            rhs <- parsePrefix rbp
            return $ Node op [rhs]
        TokEof -> throwError "bad token"
    parseInfix bp lhs

parseInfix :: BindingPower -> Syntax -> ParserM Token Syntax
parseInfix bp lhs = do
    tok <- peek
    case tok of
        TokAtom{} -> throwError "bad token"
        TokOp op -> do
            lhs' <- parsePostfix bp lhs op
            (lbp, rbp) <- getInfixBP op
            if lbp < bp then return lhs'
            else do
                next_
                rhs <- parsePrefix rbp
                return $ Node op [lhs', rhs]
        TokEof -> return lhs

parsePostfix :: BindingPower -> Syntax -> String -> ParserM Token Syntax
parsePostfix bp lhs op = do
    lbp <- getPostfixBP op
    if lbp < bp then return lhs
    else do
        next_
        return $ Node op [lhs]

