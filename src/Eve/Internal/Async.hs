{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language ExistentialQuantification #-}
{-# language ScopedTypeVariables #-}

module Eve.Internal.Async
  ( dispatchActionAsync
  , asyncActionProvider
  ) where

import Eve.Internal.Actions

import Control.Monad
import Control.Monad.State
import Control.Lens

import Pipes
import Pipes.Concurrent

dispatchActionAsync
  :: IO (App ()) -> App ()
dispatchActionAsync asyncAction = do
  queue <- use asyncQueue
  let effect = (liftIO asyncAction >>= yield) >-> toOutput queue
  liftIO . void . forkIO $ runEffect effect >> performGC

asyncActionProvider :: ((App () -> IO ()) -> IO ()) -> App ()
asyncActionProvider provider = do
  queue <- use asyncQueue
  let dispatcher action =
        let effect = yield action >-> toOutput queue
        in void . forkIO $ runEffect effect >> performGC
  liftIO . void . forkIO $ provider dispatcher
