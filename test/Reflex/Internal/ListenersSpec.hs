{-# language ScopedTypeVariables #-}
module Reflex.Internal.ListenersSpec where

import Test.Hspec

import Fixtures
import Reflex.Internal.Run
import Reflex.Internal.Listeners
import Reflex.Internal.Extensions
import Reflex.Internal.BaseMonad

import Control.Monad.State
import Control.Lens

testAction :: State ExtState () -> ExtState
testAction = flip execState emptyExts

basicAction :: State ExtState ()
basicAction = do
  addListener (const (store .= "new") :: CustomEvent -> State ExtState ())
  dispatchEvent_ CustomEvent

delayedExit :: BaseMonad ()
delayedExit = do
  addListener (const exit :: CustomEvent -> BaseMonad ())
  dispatchEventAsync (return CustomEvent)
  store .= "new"

removeListenersTest :: State ExtState ()
removeListenersTest = do
  listId <- addListener (const (store .= "new") :: CustomEvent -> State ExtState ())
  removeListener listId
  dispatchEvent_ CustomEvent

asyncEventsTest :: BaseMonad ()
asyncEventsTest = do
  addListener (const exit :: CustomEvent -> BaseMonad ())
  asyncEventProvider (\d -> d CustomEvent)
  store .= "new"

data OtherEvent = OtherEvent
multiAsyncEventsTest :: BaseMonad ()
multiAsyncEventsTest = do
  addListener (const exit :: CustomEvent -> BaseMonad ())
  addListener (const (store .= "new") :: OtherEvent -> BaseMonad ())
  asyncEventProvider (\d -> d CustomEvent >> d OtherEvent)

spec :: Spec
spec = do
  describe "dispatchEvent/addListener" $
    it "Triggers Listeners" $ (testAction basicAction ^. store) `shouldBe` "new"
  describe "dispatchEventAsync" $ do
    delayedExitState <- runIO $ reflex delayedExit
    it "Triggers Listeners Eventually" $ (delayedExitState ^. store) `shouldBe` "new"
  describe "removeListener" $
    it "Removes Listeners" $ (testAction removeListenersTest ^. store) `shouldBe` "default"
  describe "asyncEventProvider" $ do
    asyncEventsResult <- runIO $ reflex asyncEventsTest
    it "Provides events eventually" $ (asyncEventsResult ^. store) `shouldBe` "new"
    multiAsyncEventsResult <- runIO $ reflex multiAsyncEventsTest
    it "Can provide different event types" $ (multiAsyncEventsResult ^. store) `shouldBe` "new"
