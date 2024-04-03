module ElabSpec (spec) where

import Common
import Elab
import Raw qualified
import Syntax
import Test.Hspec
import Value
import Value.Env  as Env

spec :: Spec
spec = do
    describe "eval" $ do
        it "λx → x" $
            (runElabM (infer (Raw.Lam "x" (Right Expl) (Raw.Var "x"))) =<< initElabCtx)
                `shouldReturn` (Lam "x" Expl (Var 0), VPi "x" Impl VU (Closure (Env.fromList [VVar 0]) (Arrow (Var 1) (Var 1))))
