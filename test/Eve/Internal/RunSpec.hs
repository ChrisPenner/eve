module Eve.Internal.RunSpec where

import Test.Hspec.Core.Spec (SpecM)
import Test.Hspec
import Fixtures
import Eve
import Eve.Internal.Actions
import Control.Lens

exiter :: (AppT TestState IO () -> IO ()) -> IO ()
exiter dispatch = dispatch exit

spec :: Spec
spec = do
  describe "Running App" $ do
    it "Initializes" $
      fst (noIOTest (store .= "new" >> use store)) `shouldBe` "new"

    asyncResult <- ioTest (asyncActionProvider (\d -> d (store .= "new" >> exit)))
    it "Collects Async Actions" $
      (asyncResult ^. store) `shouldBe` "new"
