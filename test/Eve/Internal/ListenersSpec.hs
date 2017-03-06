{-# language ScopedTypeVariables #-}
module Eve.Internal.ListenersSpec where

import Test.Hspec

import Fixtures
import Eve

import Control.Monad.State
import Control.Lens

basicAction :: AppT TestState Identity String
basicAction = do
  addListener (const (store .= "new") :: CustomEvent -> AppT TestState Identity ())
  dispatchEvent_ CustomEvent
  use store

delayedExit :: AppT TestState IO ()
delayedExit = do
  addListener (const exit :: CustomEvent -> AppT TestState IO ())
  dispatchEventAsync (return CustomEvent)
  store .= "new"

removeListenersTest :: AppT TestState Identity String
removeListenersTest = do
  listId <- addListener (const (store .= "new") :: CustomEvent -> AppT TestState Identity ())
  removeListener listId
  dispatchEvent_ CustomEvent
  use store

asyncEventsTest :: AppT TestState IO ()
asyncEventsTest = do
  addListener (const exit :: CustomEvent -> AppT TestState IO ())
  asyncEventProvider (\d -> d CustomEvent)
  store .= "new"

data OtherEvent = OtherEvent
multiAsyncEventsTest :: AppT TestState IO ()
multiAsyncEventsTest = do
  addListener (const exit :: CustomEvent -> AppT TestState IO ())
  addListener (const (store .= "new") :: OtherEvent -> AppT TestState IO ())
  asyncEventProvider (\d -> d CustomEvent >> d OtherEvent)

spec :: Spec
spec = do
  describe "dispatchEvent/addListener" $
    it "Triggers Listeners" $ fst (noIOTest basicAction) `shouldBe` "new"
  describe "dispatchEventAsync" $ do
    delayedExitState <- ioTest delayedExit
    it "Triggers Listeners Eventually" $ (delayedExitState ^. store) `shouldBe` "new"
  describe "removeListener" $
    it "Removes Listeners" $ fst (noIOTest removeListenersTest) `shouldBe` "default"
  describe "asyncEventProvider" $ do
    asyncEventsResult <- ioTest asyncEventsTest
    it "Provides events eventually" $ (asyncEventsResult ^. store) `shouldBe` "new"
    multiAsyncEventsResult <- ioTest multiAsyncEventsTest
    it "Can provide different event types" $ (multiAsyncEventsResult ^. store) `shouldBe` "new"
