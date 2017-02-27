{-# LANGUAGE MultiParamTypeClasses #-}

module Reflex.Internal.Async
  ( HasAsyncQueue(..)
  , dispatchActionAsync
  , asyncActionProvider
  ) where

import Control.Monad
import Control.Monad.State
import Control.Lens

import Pipes
import Pipes.Concurrent

class (MonadState s m) =>
      HasAsyncQueue m s  where
  asyncQueue :: Lens' s (Output (m ()))

dispatchActionAsync
  :: (Monad m, MonadIO m, MonadState s m, HasAsyncQueue m s)
  => IO (m ()) -> m ()
dispatchActionAsync asyncAction = do
  queue <- use asyncQueue
  let effect = (liftIO asyncAction >>= yield) >-> toOutput queue
  liftIO . void . forkIO $ runEffect effect >> performGC

type ActionDispatcher m = m () -> IO ()

asyncActionProvider
  :: (MonadIO m, HasAsyncQueue m s)
  => (ActionDispatcher m -> IO ()) -> m ()
asyncActionProvider provider = do
  queue <- use asyncQueue
  let dispatcher action =
        let effect = yield action >-> toOutput queue
        in void . forkIO $ runEffect effect >> performGC
  liftIO . void . forkIO $ provider dispatcher
