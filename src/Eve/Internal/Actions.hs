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

import Eve.Internal.States
import Data.Default

import Control.Monad.State
import Control.Monad.Trans.Free
import Control.Lens

import Pipes.Concurrent

type App a = Action AppState AppState a
data AppState = AppState
  { _baseStates :: States
  , _asyncQueue :: Output (App ())
  }

newtype ActionF base next =
  LiftAction (StateT base IO next)
  deriving (Functor, Applicative)

newtype Action base zoomed a = Action
  { getAction :: FreeT (ActionF base) (StateT zoomed IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadState zoomed, MonadFree (ActionF base))
makeLenses ''AppState

instance HasStates AppState where
  states = baseStates

instance HasEvents AppState where

unLift :: FreeT (ActionF base) (StateT base IO) a -> StateT base IO a
unLift m = do
  step <- runFreeT m
  case step of
    Pure a -> return a
    Free (LiftAction next) -> next >>= unLift

liftAction :: Action base base a -> Action base zoomed a
liftAction = liftF .  LiftAction . unLift . getAction

execApp :: base -> Action base base a -> IO a
execApp appState = flip evalStateT appState . unLift . getAction

type instance Zoomed (Action base zoomed) = Zoomed (FreeT (ActionF base) (StateT zoomed IO))
instance Zoom (Action base s) (Action base t) s t where
  zoom l (Action action) = Action $ zoom l action

runAction :: Zoom m n s t => LensLike' (Zoomed m c) t s -> m c -> n c
runAction = zoom

newtype Exiting =
  Exiting Bool
  deriving (Show, Eq)

instance Default Exiting where
  def = Exiting False

exit :: App ()
exit = stateLens .= Exiting True

isExiting :: App Bool
isExiting = do
  Exiting b <- use stateLens
  return b
