{-# language GeneralizedNewtypeDeriving #-}
{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language TemplateHaskell #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language ScopedTypeVariables #-}
module Eve.Internal.Actions
  ( Action(..)
  , ActionF(..)
  , App
  , execApp

  , AppState(..)
  , asyncQueue

  , liftAction
  , runAction

  , exit
  , isExiting
  , Exiting(..)
  ) where

import Eve.Internal.Extensions
import Data.Default

import Control.Monad.State
import Control.Monad.Trans.Free
import Control.Lens

import Pipes.Concurrent

type App a = Action AppState a
data AppState = AppState
  { _baseExts :: Exts
  , _asyncQueue :: Output (App ())
  }

newtype ActionF next =
  LiftAction (StateT AppState IO next)
  deriving (Functor, Applicative)

newtype Action zoomed a = Action
  { getAction :: FreeT ActionF (StateT zoomed IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadState zoomed, MonadFree ActionF)
makeLenses ''AppState

instance HasExts AppState where
  exts = baseExts

instance HasEvents AppState where

unLift :: FreeT ActionF (StateT AppState IO) a -> StateT AppState IO a
unLift m = do
  step <- runFreeT m
  case step of
    Pure a -> return a
    Free (LiftAction next) -> next >>= unLift

liftAction :: Action AppState a -> Action zoomed a
liftAction = liftF .  LiftAction . unLift . getAction

execApp :: AppState -> Action AppState a -> IO a
execApp appState = flip evalStateT appState . unLift . getAction

type instance Zoomed (Action s) = Zoomed (FreeT ActionF (StateT s IO))
instance Zoom (Action s) (Action t) s t where
  zoom l (Action action) = Action $ zoom l action

runAction :: Zoom m n s t => LensLike' (Zoomed m c) t s -> m c -> n c
runAction = zoom

newtype Exiting =
  Exiting Bool
  deriving (Show, Eq)

instance Default Exiting where
  def = Exiting False

exit :: App ()
exit = ext .= Exiting True

isExiting :: App Bool
isExiting = do
  Exiting b <- use ext
  return b
