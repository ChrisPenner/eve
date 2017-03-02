module Reflex.Internal.AsyncSpec where

import Test.Hspec

import Fixtures
import Control.Lens
import Reflex.Internal.Run
import Reflex.Internal.Listeners
import Reflex.Internal.App
import Reflex.Internal.Async

asyncActionsTest :: App ()
asyncActionsTest = do
  addListener (const exit :: CustomEvent -> App ())
  asyncActionProvider (\d -> d exit)
  store .= "new"

spec :: Spec
spec = do
  describe "asyncActionProvider" $ do
    asyncState <- runIO $ reflex (asyncActionProvider (\d -> d (store .= "new" >> exit)))
    it "Eventually Runs Provided Actions" $
      (asyncState ^. store) `shouldBe` "new"
  describe "dispatchActionAsync" $ do
    asyncState <- runIO $ reflex (asyncActionProvider (\d -> d (store .= "new" >> exit)))
    asyncActionsResult <- runIO $ reflex asyncActionsTest
    it "Eventually Runs Provided Actions" $
      (asyncActionsResult ^. store) `shouldBe` "new"
