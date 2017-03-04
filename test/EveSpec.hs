module EveSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "Eve" $ do
    it "runs tests" $
      True `shouldBe` True
