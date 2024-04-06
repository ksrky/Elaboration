module ElabSpec (spec) where

import Common
import Elab
import Raw qualified
import Syntax
import Test.Hspec

elab :: Raw.Raw -> IO Term
elab e = do
    (t, _) <- runElabM (infer e) =<< initElabCtx
    return t

spec :: Spec
spec = do
    describe "eval" $ do
        it "λ{a} → λx → x" $
            elab (Raw.Lam "a" (Right Impl) (Raw.Lam "x" (Right Expl) (Raw.Var "x")))
                `shouldReturn` Lam "a" Impl (Lam "x" Expl (Var 0))
