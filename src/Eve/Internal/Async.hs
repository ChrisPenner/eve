{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language ExistentialQuantification #-}
{-# language ScopedTypeVariables #-}

module Eve.Internal.Async
  ( dispatchActionAsync
  , asyncActionProvider
  ) where

import Eve.Internal.Actions
import Eve.Internal.States

import Control.Monad
import Control.Monad.State
import Control.Lens
import Data.Typeable

import Pipes
import Pipes.Concurrent

dispatchActionAsync
  :: (MonadIO m, HasStates base, Typeable m, Typeable base) => IO (AppT base m ()) -> ActionT base zoomed m ()
dispatchActionAsync asyncAction = liftAction $ do
  mQueue <- use asyncQueue
  case mQueue of
    Nothing -> return ()
    Just queue -> do
      let effect = (liftIO asyncAction >>= yield) >-> toOutput queue
      liftIO . void . forkIO $ runEffect effect >> performGC

asyncActionProvider :: (MonadIO m, HasStates base, Typeable m, Typeable base) => ((AppT base m () -> IO ()) -> IO ()) -> ActionT base zoomed m ()
asyncActionProvider provider = liftAction $ do
  mQueue <- use asyncQueue
  case mQueue of
    Nothing -> return ()
    Just queue -> do
      let dispatcher action =
            let effect = yield action >-> toOutput queue
            in void . forkIO $ runEffect effect >> performGC
      liftIO . void . forkIO $ provider dispatcher
