module EvalSpec (spec) where

import           Common
import           Eval
import           Syntax
import           Test.Hspec
import           Value
import qualified Value.Env  as Env

spec :: Spec
spec = do
    describe "eval" $ do
        it "λx → x" $
            evalClosedTerm (Lam "x" Expl (Var 0)) `shouldReturn` VLam "x" Expl (Closure Env.empty (Var 0))
        it "(λx → x) U" $
            evalClosedTerm (App (Lam "x" Expl (Var 0)) U Expl) `shouldReturn` VU
