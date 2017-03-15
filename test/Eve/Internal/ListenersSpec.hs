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

localStateIsolationTest :: ActionT AppState NestedStates Identity String
localStateIsolationTest = do
  addLocalListener (const (nestedString .= "new") :: CustomEvent -> ActionT AppState NestedStates Identity ())
  dispatchEvent_ CustomEvent
  use nestedString

globalStateIsolationTest :: AppT AppState Identity String
globalStateIsolationTest = do
  addListener (const (store .= "new") :: CustomEvent -> AppT AppState Identity ())
  runActionOver nestedStates $ do
    dispatchLocalEvent_ CustomEvent
  use store

listenerPassThroughTest :: AppT AppState Identity String
listenerPassThroughTest = do
  let nestedAction :: ActionT AppState NestedStates Identity ()
      nestedAction = do
        addListener (const (store .= "new") :: CustomEvent -> AppT AppState Identity ())
        dispatchEvent CustomEvent
  runActionOver nestedStates nestedAction
  use store


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

  describe "local events" $ do
    it "local state is isolated from global events" $ fst (noIOTest $ runAction localStateIsolationTest) `shouldBe` "default"
    it "global state is isolated from local events" $ fst (noIOTest globalStateIsolationTest) `shouldBe` "default"
    it "global event actions pass through from local state" $ fst (noIOTest listenerPassThroughTest) `shouldBe` "new"

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
