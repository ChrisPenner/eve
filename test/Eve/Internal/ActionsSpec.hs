module Eve.Internal.ActionsSpec where

import Test.Hspec

import Fixtures
import Eve
import Eve.Internal.Actions

import Control.Lens
import Control.Monad.State

appendEx  :: Monad m => ActionT TestState String m ()
appendEx  = modify (++ "!!")

liftActionTest :: Monad m => ActionT TestState String m String
liftActionTest = do
  put "new"
  liftAction $ runAction stateLens appendEx
  get

spec :: Spec
spec = do
  describe "Exiting" $ do
    it "Exits" $
      let (didExit, _) = noIOTest (exit >> isExiting)
       in didExit `shouldBe` True

  describe "runAction " $ do
    it "runs lifted actions to zoomed monad" $
      let (runActionResult, _) = noIOTest $ runAction stateLens (put "new" >> appendEx) >> runAction stateLens get
       in runActionResult `shouldBe` "new!!"

    it "runs over traversals" $
      let (traversalResult, _) = noIOTest $ runAction stateLens (put $ Just "new") >> runAction (stateLens._Just) (appendEx >> get)
       in traversalResult `shouldBe` "new!!"

  describe "liftAction" $ do
    it "runs lifted actions to zoomed monad" $
      let (liftActionResult, _) = noIOTest (runAction stateLens liftActionTest :: AppT TestState Identity String)
       in liftActionResult `shouldBe` "new!!"

