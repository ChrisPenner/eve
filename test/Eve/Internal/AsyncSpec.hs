module Eve.Internal.AsyncSpec where

import Test.Hspec

import Fixtures
import Control.Lens
import Eve.Internal.Run
import Eve.Internal.Listeners
import Eve.Internal.App
import Eve.Internal.Async

asyncActionsTest :: App ()
asyncActionsTest = do
  addListener (const exit :: CustomEvent -> App ())
  asyncActionProvider (\d -> d exit)
  store .= "new"

spec :: Spec
spec = do
  describe "asyncActionProvider" $ do
    asyncState <- runIO $ eve (asyncActionProvider (\d -> d (store .= "new" >> exit)))
    it "Eventually Runs Provided Actions" $
      (asyncState ^. store) `shouldBe` "new"
  describe "dispatchActionAsync" $ do
    asyncState <- runIO $ eve (asyncActionProvider (\d -> d (store .= "new" >> exit)))
    asyncActionsResult <- runIO $ eve asyncActionsTest
    it "Eventually Runs Provided Actions" $
      (asyncActionsResult ^. store) `shouldBe` "new"
