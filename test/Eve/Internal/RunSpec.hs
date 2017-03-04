module Eve.Internal.RunSpec where

import Test.Hspec.Core.Spec (SpecM)
import Test.Hspec
import Fixtures
import Eve.Internal.Run
import Eve.Internal.Extensions
import Eve.Internal.Async
import Eve.Internal.App
import Control.Lens

exiter :: (App () -> IO ()) -> IO ()
exiter dispatch = dispatch exit

runWithInit :: App () -> SpecM a BaseState
runWithInit next = runIO $ eve (next >> asyncActionProvider exiter)

spec :: Spec
spec = do
  describe "Running App" $ do
    exitState <- runWithInit (return ())
    it "Exits" $
      (exitState ^. ext) `shouldBe` Exiting True

    initializeState <- runWithInit (store .= "new")
    it "Initializes" $
      (initializeState ^. store) `shouldBe` "new"

    asyncState <- runIO $ eve (asyncActionProvider (\d -> d (store .= "new" >> exit)))
    it "Collects Async Actions" $
      (asyncState ^. store) `shouldBe` "new"
