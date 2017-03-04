{-# language ScopedTypeVariables #-}
module Eve.Internal.ListenersSpec where

import Test.Hspec

import Fixtures
import Eve.Internal.Run
import Eve.Internal.Listeners
import Eve.Internal.Extensions
import Eve.Internal.App

import Control.Monad.State
import Control.Lens

testAction :: State ExtState () -> ExtState
testAction = flip execState emptyExts

basicAction :: State ExtState ()
basicAction = do
  addListener (const (store .= "new") :: CustomEvent -> State ExtState ())
  dispatchEvent_ CustomEvent

delayedExit :: App ()
delayedExit = do
  addListener (const exit :: CustomEvent -> App ())
  dispatchEventAsync (return CustomEvent)
  store .= "new"

removeListenersTest :: State ExtState ()
removeListenersTest = do
  listId <- addListener (const (store .= "new") :: CustomEvent -> State ExtState ())
  removeListener listId
  dispatchEvent_ CustomEvent

asyncEventsTest :: App ()
asyncEventsTest = do
  addListener (const exit :: CustomEvent -> App ())
  asyncEventProvider (\d -> d CustomEvent)
  store .= "new"

data OtherEvent = OtherEvent
multiAsyncEventsTest :: App ()
multiAsyncEventsTest = do
  addListener (const exit :: CustomEvent -> App ())
  addListener (const (store .= "new") :: OtherEvent -> App ())
  asyncEventProvider (\d -> d CustomEvent >> d OtherEvent)

spec :: Spec
spec = do
  describe "dispatchEvent/addListener" $
    it "Triggers Listeners" $ (testAction basicAction ^. store) `shouldBe` "new"
  describe "dispatchEventAsync" $ do
    delayedExitState <- runIO $ eve delayedExit
    it "Triggers Listeners Eventually" $ (delayedExitState ^. store) `shouldBe` "new"
  describe "removeListener" $
    it "Removes Listeners" $ (testAction removeListenersTest ^. store) `shouldBe` "default"
  describe "asyncEventProvider" $ do
    asyncEventsResult <- runIO $ eve asyncEventsTest
    it "Provides events eventually" $ (asyncEventsResult ^. store) `shouldBe` "new"
    multiAsyncEventsResult <- runIO $ eve multiAsyncEventsTest
    it "Can provide different event types" $ (multiAsyncEventsResult ^. store) `shouldBe` "new"
