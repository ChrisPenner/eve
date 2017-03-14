{-# language ScopedTypeVariables #-}
module Eve.Internal.ListenersSpec where

import Test.Hspec

import Fixtures
import Eve

import Control.Monad.State
import Control.Lens

basicAction :: AppT AppState Identity String
basicAction = do
  addListener (const (store .= "new") :: CustomEvent -> AppT AppState Identity ())
  dispatchEvent_ CustomEvent
  use store

delayedExit :: App ()
delayedExit = do
  addListener (const exit :: CustomEvent -> App ())
  dispatchEventAsync (return CustomEvent)
  store .= "new"

removeListenersTest :: AppT AppState Identity String
removeListenersTest = do
  listId <- addListener (const (store .= "new") :: CustomEvent -> AppT AppState Identity ())
  removeListener listId
  dispatchEvent_ CustomEvent
  use store

asyncEventsTest :: App ()
asyncEventsTest = do
  addListener (const exit :: CustomEvent -> App ())
  asyncEventProvider (\d -> d CustomEvent)
  store .= "new"

localEventsTest :: Action NestedStates ()
localEventsTest = do
  addLocalListener (const (nestedString .= "new") :: CustomEvent -> Action NestedStates ())
  dispatchEvent CustomEvent

data OtherEvent = OtherEvent
multiAsyncEventsTest :: App ()
multiAsyncEventsTest = do
  addListener (const exit :: CustomEvent -> App ())
  addListener (const (store .= "new") :: OtherEvent -> App ())
  asyncEventProvider (\d -> d OtherEvent >> d CustomEvent)

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

--   describe "removeListener" $
--     it "Removes Listeners" $ fst (noIOTest removeListenersTest) `shouldBe` "default"
