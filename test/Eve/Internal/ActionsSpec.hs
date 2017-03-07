module Eve.Internal.ActionsSpec where

import Test.Hspec

import Fixtures
import Eve
import Eve.Internal.AppState

import Control.Lens
import Control.Monad.State

appendEx  :: Monad m => ActionT AppState String m ()
appendEx  = modify (++ "!!")

liftAppTest :: Monad m => ActionT AppState String m String
liftAppTest = do
  put "new"
  liftApp $ runAction stateLens appendEx
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

  describe "liftApp" $ do
    it "runs lifted actions to zoomed monad" $
      let (liftAppResult, _) = noIOTest (runAction stateLens liftAppTest :: AppT AppState Identity String)
       in liftAppResult `shouldBe` "new!!"

