{-# language TemplateHaskell #-}
module Eve.Internal.ActionsSpec where

import Test.Hspec

import Fixtures
import Eve
import Eve.Internal.AppState

import Control.Lens
import Control.Monad.State
import Data.Default

appendEx  :: Monad m => ActionT AppState String m ()
appendEx  = modify (++ "!!")

runAppTest :: Monad m => ActionT AppState String m String
runAppTest = do
  put "new"
  runApp $ runAction appendEx
  get

data SimpleState = SimpleState
  { _myInt :: Int
  , _myString :: String
  }
makeLenses ''SimpleState

instance Default SimpleState where
  def = SimpleState 0 "default"

alterSimpleState :: ActionT AppState SimpleState Identity String
alterSimpleState = do
  myString .= "Hello!"
  use myString

monoidTest :: ActionT AppState SimpleState Identity (String, [Int])
monoidTest = do
  result <- one `mappend` two
  str <- use myString
  return (str, result)
  where
    one = myString .= "first" >> return [1]
    two = myString <>= "second" >> return [2]

spec :: Spec
spec = do
  describe "Exiting" $ do
    it "Exits" $
      let (didExit, _) = noIOTest (exit >> isExiting)
       in didExit `shouldBe` True

  describe "runAction" $ do
    it "runs lifted actions to zoomed monad" $
      let (runActionResult, _) = noIOTest $ runAction (put "new" >> appendEx) >> runAction get
       in runActionResult `shouldBe` "new!!"

  describe "runActionOver" $ do
    it "runs lifted actions to zoomed monad" $
      let (runActionOverResult, _) = noIOTest $ runActionOver stateLens (put "new" >> appendEx) >> runActionOver stateLens get
       in runActionOverResult `shouldBe` "new!!"

    it "runs over traversals" $
      let (traversalResult, _) = noIOTest $ runActionOver stateLens (put $ Just "new") >> runActionOver (stateLens._Just) (appendEx >> get)
       in traversalResult `shouldBe` "new!!"

  describe "runApp" $ do
    it "runs lifted actions to zoomed monad" $
      let (runAppResult, _) = noIOTest (runAction runAppTest :: AppT AppState Identity String)
       in runAppResult `shouldBe` "new!!"

  describe "Can run actions over non-HasStates states" $ do
    it "compiles" $
      let (alterationResult, _) = noIOTest (runAction alterSimpleState :: AppT AppState Identity String)
       in alterationResult `shouldBe` "Hello!"

  describe "is a Monoid" $ do
    it "combines results and effects" $
      let (monoidResult, _) = noIOTest (runAction monoidTest :: AppT AppState Identity (String, [Int]))
       in monoidResult `shouldBe` ("firstsecond", [1, 2])
