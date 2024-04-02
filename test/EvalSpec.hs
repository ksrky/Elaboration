module EvalSpec (spec) where

import Common
import Eval
import Syntax
import Test.Hspec
import Value
import Value.Env  qualified as Env

spec :: Spec
spec = do
    describe "eval" $ do
        it "λx → x" $
            evalClosedTerm (Lam "x" Expl (Var 0)) `shouldReturn` VLam "x" Expl (Closure Env.empty (Var 0))
        it "(λx → x) U" $
            evalClosedTerm (App (Lam "x" Expl (Var 0)) U Expl) `shouldReturn` VU
        it "let id : {A : U} → A → A = λx → x; id U" $
            evalClosedTerm (Let "id" (Pi "A" Impl U (Arrow (Var 1) (Var 1))) (Lam "x" Expl (Var 0)) (App (Var 0) U Expl)) `shouldReturn` VU
