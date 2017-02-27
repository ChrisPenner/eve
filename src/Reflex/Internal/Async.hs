module Reflex.Internal.Async
  ( HasAsyncQueue(..)
  , dispatchActionAsync
  , asyncActionProvider
  ) where

import Reflex.Internal.Action

import Control.Monad
import Control.Lens

import Pipes
import Pipes.Concurrent

class HasAsyncQueue s where
  asyncQueue :: Lens' s (Output (Action s ()))

dispatchActionAsync :: HasAsyncQueue s => IO (Action s ()) -> Action s ()
dispatchActionAsync asyncAction = do
  queue <- use asyncQueue
  let effect = (liftIO asyncAction >>= yield) >-> toOutput queue
  liftIO . void . forkIO $ runEffect effect >> performGC

type ActionDispatcher s = Action s () -> IO ()
asyncActionProvider :: HasAsyncQueue s => (ActionDispatcher s -> IO ()) -> Action s ()
asyncActionProvider provider = do
  queue <- use asyncQueue
  let dispatcher action =
        let effect = yield action >-> toOutput queue
        in void . forkIO $ runEffect effect >> performGC
  liftIO . void . forkIO $ provider dispatcher
