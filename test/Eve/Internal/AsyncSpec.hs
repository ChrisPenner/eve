module Eve.Internal.AsyncSpec where

import Test.Hspec

import Fixtures
import Control.Lens
import Eve

asyncActionsTest :: App ()
asyncActionsTest = do
  addListener (const exit :: CustomEvent -> App ())
  asyncActionProvider (\d -> d exit)
  store .= "new"

spec :: Spec
spec = do
  describe "asyncActionProvider" $ do
    asyncState <- ioTest (asyncActionProvider (\d -> d (store .= "new" >> exit)))
    it "Eventually Runs Provided Actions" $
      (asyncState ^. store) `shouldBe` "new"
  describe "dispatchActionAsync" $ do
    asyncActionsResult <- ioTest asyncActionsTest
    it "Eventually Runs Provided Actions" $
      (asyncActionsResult ^. store) `shouldBe` "new"
