module ParserSpec (spec) where

import           Data.Text  (Text)
import           Parser
import           Raw
import           Test.Hspec

stripParseProg :: Text -> IO Raw
stripParseProg = fmap stripSrcPos . parseProg

spec :: Spec
spec = do
  describe "parse" $ do
    it "x" $
      stripParseProg "x" `shouldReturn` Var "x"
    it "λx → x" $
      stripParseProg "λx → x" `shouldReturn` Lam "x" (Var "x")
    it "x y" $
      stripParseProg "x y" `shouldReturn` App (Var "x") (Var "y")
    it "(x : A) → B" $
      stripParseProg "(x : A) → B" `shouldReturn` Pi "x" (Var "A") (Var "B")
    it "let x : A = t in u" $
      stripParseProg "let x : A = t in u" `shouldReturn` Let "x" (Var "A") (Var "t") (Var "u")
    it "U" $
      stripParseProg "U" `shouldReturn` U
