{-# language TemplateHaskell #-}
module Eve.Internal.StatesSpec where

import Test.Hspec
import Fixtures
import Eve

import Control.Lens
import Data.Default

data SimpleState = SimpleState
  { _myString :: String
  }
makeLenses ''SimpleState

instance Default SimpleState where
  def = SimpleState "default"

myStringStateLens :: HasStates s => Lens' s String
myStringStateLens = makeStateLens myString

makeStateLensTest :: AppT AppState Identity String
makeStateLensTest = do
  myStringStateLens .= "new"
  use (stateLens.myString)

spec :: Spec
spec = do
  describe "makeStateLens" $ do
    it "creates a lens which accesses state from States" $
      let (makeStateLensResult, _) = noIOTest makeStateLensTest
       in makeStateLensResult `shouldBe` "new"
