module ParserSpec (spec) where

import Common
import Data.Text  (Text)
import Parser
import Raw
import Test.Hspec

stripParseProg :: Text -> IO Raw
stripParseProg = fmap stripSrcPos . parseProg

spec :: Spec
spec = do
  describe "parse" $ do
    it "x" $
      stripParseProg "x" `shouldReturn` Var "x"
    it "λx → x" $
      stripParseProg "λx → x" `shouldReturn` Lam "x" (Right Expl) (Var "x")
    it "λ{x} → x" $
      stripParseProg "λ{x} → x" `shouldReturn` Lam "x" (Right Impl) (Var "x")
    it "λ{l = x} → x" $
      stripParseProg "λ{l = x} → x" `shouldReturn` Lam "x" (Left "l") (Var "x")
    it "x y" $
      stripParseProg "x y" `shouldReturn` App (Var "x") (Var "y") (Right Expl)
    it "x {y}" $
      stripParseProg "x {y}" `shouldReturn` App (Var "x") (Var "y") (Right Impl)
    it "x {l = y}" $
      stripParseProg "x {l = y}" `shouldReturn` App (Var "x") (Var "y") (Left "l")
    it "U" $
      stripParseProg "U" `shouldReturn` U
    it "(x : A) → B" $
      stripParseProg "(x : A) → B" `shouldReturn` Pi "x" Expl (Var "A") (Var "B")
    it "{x : A} → B" $
      stripParseProg "{x : A} → B" `shouldReturn` Pi "x" Impl (Var "A") (Var "B")
    it "let x : A = t; u" $
      stripParseProg "let x : A = t; u" `shouldReturn` Let "x" (Var "A") (Var "t") (Var "u")
    it "_" $
      stripParseProg "_" `shouldReturn` Hole
