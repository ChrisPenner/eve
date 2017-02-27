module ReflexSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "Reflex" $ do
    it "runs tests" $
      True `shouldBe` True
