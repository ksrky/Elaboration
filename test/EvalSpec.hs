module EvalSpec (spec) where

import           Eval
import           Syntax
import           Test.Hspec
import           Value
import qualified Value.Env  as Env

spec :: Spec
spec = do
    describe "eval" $ do
        it "λx → x" $
            evalClosedTerm (Lam "x" (Var 0)) `shouldBe` VLam "x" (Closure Env.empty (Var 0))
        it "(λx → x) U" $
            evalClosedTerm (App (Lam "x" (Var 0)) U) `shouldBe` VU
