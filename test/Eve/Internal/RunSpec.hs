module Eve.Internal.RunSpec where

import Test.Hspec.Core.Spec (SpecM)
import Test.Hspec
import Fixtures
import Eve
import Eve.Internal.Testing
import Control.Lens

exiter :: (App () -> IO ()) -> IO ()
exiter dispatch = dispatch exit

runWithInit :: App () -> SpecM a AppState
runWithInit next = runIO $ eve (next >> asyncActionProvider exiter)

spec :: Spec
spec = do
  describe "Running App" $ do
    initializeState <- runWithInit (store .= "new")
    it "Initializes" $
      (initializeState ^. store) `shouldBe` "new"

    asyncState <- runIO $ eve (asyncActionProvider (\d -> d (store .= "new" >> exit)))
    it "Collects Async Actions" $
      (asyncState ^. store) `shouldBe` "new"
