module Eve.Internal.ActionsSpec where

import Test.Hspec

import Fixtures
import Eve
import Eve.Internal.Testing
import Eve.Internal.Actions

import Control.Lens
import Control.Monad.State

appendEx  :: Action String ()
appendEx  = modify (++ "!!")

spec :: Spec
spec = do
  describe "Exiting" $ do
    exiting <- runIO . noEventTest $ exit >> isExiting
    it "Exits" $
      exiting `shouldBe` True

  describe "runAction " $ do
    runActionResult <- runIO . noEventTest $ runAction ext (put "new" >> appendEx) >> runAction ext get
    it "runs lifted actions to zoomed monad" $
      runActionResult `shouldBe` "new!!"

    traversalResult <- runIO . noEventTest $ runAction ext (put $ Just "new") >> runAction (ext._Just) (appendEx >> get)
    it "runs over traversals" $
      traversalResult `shouldBe` "new!!"

  describe "liftAction" $ do
    liftActionResult <- runIO . noEventTest . runAction ext $ do
      put "new" 
      liftAction $ runAction ext appendEx
      get
    it "runs lifted actions to zoomed monad" $
      liftActionResult `shouldBe` "new!!"

