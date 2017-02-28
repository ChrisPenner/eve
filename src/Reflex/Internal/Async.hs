{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language ExistentialQuantification #-}
{-# language ScopedTypeVariables #-}

module Reflex.Internal.Async
  ( dispatchActionAsync
  , asyncActionProvider
  ) where

import Reflex.Internal.BaseMonad

import Control.Monad
import Control.Monad.State
import Control.Lens

import Pipes
import Pipes.Concurrent

dispatchActionAsync
  :: IO (BaseMonad ()) -> BaseMonad ()
dispatchActionAsync asyncAction = do
  queue <- use asyncQueue
  let effect = (liftIO asyncAction >>= yield) >-> toOutput queue
  liftIO . void . forkIO $ runEffect effect >> performGC

asyncActionProvider :: ((BaseMonad () -> IO ()) -> IO ()) -> BaseMonad ()
asyncActionProvider provider = do
  queue <- use asyncQueue
  let dispatcher action =
        let effect = yield action >-> toOutput queue
        in void . forkIO $ runEffect effect >> performGC
  liftIO . void . forkIO $ provider dispatcher
