module Reflex.Internal.RunSpec where

import Test.Hspec.Core.Spec (SpecM)
import Test.Hspec
import Fixtures
import Reflex.Internal.Run
import Reflex.Internal.Extensions
import Reflex.Internal.Async
import Reflex.Internal.BaseMonad
import Control.Lens
import Data.Default

data Store =
  Store String
  deriving (Show, Eq)
instance Default Store where
  def = Store "default"

exiter :: (BaseMonad () -> IO ()) -> IO ()
exiter dispatch = dispatch exit

runWithInit :: BaseMonad () -> SpecM a BaseState
runWithInit next = runIO $ reflex (next >> asyncActionProvider exiter)

spec :: Spec
spec = do
  describe "Running App" $ do
    exitState <- runWithInit (return ())
    it "Exits" $
      (exitState ^. ext) `shouldBe` (Exiting True)

    initializeState <- runWithInit (ext .= Store "new")
    it "Initializes" $
      (initializeState ^. ext) `shouldBe` (Store "new")

    asyncState <- runIO $ reflex (asyncActionProvider (\d -> d (ext .= Store "new" >> exit)))
    it "Collects Async Actions" $
      (asyncState ^. ext) `shouldBe` (Store "new")
