module Reflex.Internal.RunSpec where

import Test.Hspec.Core.Spec (SpecM)
import Test.Hspec
import Fixtures
import Reflex.Internal.Run
import Reflex.Internal.Extensions
import Reflex.Internal.Async
import Reflex.Internal.App
import Control.Lens

exiter :: (App () -> IO ()) -> IO ()
exiter dispatch = dispatch exit

runWithInit :: App () -> SpecM a BaseState
runWithInit next = runIO $ reflex (next >> asyncActionProvider exiter)

spec :: Spec
spec = do
  describe "Running App" $ do
    exitState <- runWithInit (return ())
    it "Exits" $
      (exitState ^. ext) `shouldBe` Exiting True

    initializeState <- runWithInit (store .= "new")
    it "Initializes" $
      (initializeState ^. store) `shouldBe` "new"

    asyncState <- runIO $ reflex (asyncActionProvider (\d -> d (store .= "new" >> exit)))
    it "Collects Async Actions" $
      (asyncState ^. store) `shouldBe` "new"
