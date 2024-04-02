module Lib.ParserSpec (spec) where

import Data.Either
import Data.Function
import Data.Functor
import Data.Text
import Lib.Common
import Lib.Parser
import Lib.Raw
import System.IO
import Test.Hspec

stripParseProg :: Text -> IO Raw
stripParseProg = fmap stripSrcPos . parseProg

spec :: Spec
spec = do
  describe "parse" $ do
    it "x" $
      stripParseProg "x" `shouldReturn` RVar "x"
    it "λx → x" $
      stripParseProg "λx → x" `shouldReturn` RLam "x" (Right Expl) (RVar "x")
    it "λ{x} → x" $
      stripParseProg "λ{x} → x" `shouldReturn` RLam "x" (Right Impl) (RVar "x")
    it "λ{l = x} → x" $
      stripParseProg "λ{l = x} → x" `shouldReturn` RLam "x" (Left "l") (RVar "x")
    it "x y" $
      stripParseProg "x y" `shouldReturn` RApp (RVar "x") (RVar "y") (Right Expl)
    it "x {y}" $
      stripParseProg "x {y}" `shouldReturn` RApp (RVar "x") (RVar "y") (Right Impl)
    it "x {l = y}" $
      stripParseProg "x {l = y}" `shouldReturn` RApp (RVar "x") (RVar "y") (Left "l")
    it "U" $
      stripParseProg "U" `shouldReturn` RU
    it "(x : A) → B" $
      stripParseProg "(x : A) → B" `shouldReturn` RPi "x" Expl (RVar "A") (RVar "B")
    it "{x : A} → B" $
      stripParseProg "{x : A} → B" `shouldReturn` RPi "x" Impl (RVar "A") (RVar "B")
    it "let x : A = t in u" $
      stripParseProg "let x : A = t in u" `shouldReturn` RLet "x" (RVar "A") (RVar "t") (RVar "u")
    it "_" $
      stripParseProg "_" `shouldReturn` RHole


