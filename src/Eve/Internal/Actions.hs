{-# language GeneralizedNewtypeDeriving #-}
{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language TemplateHaskell #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
module Eve.Internal.Actions
  ( Action
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

type App a = ActionT AppState AppState IO a
type CustomAction base zoomed a = ActionT base zoomed IO a
type Action zoomed a = CustomAction AppState zoomed a

data AppState = AppState
  { _baseStates :: States
  , _asyncQueue :: Output (App ())
  }

newtype ActionF base m next =
  LiftAction (StateT base m next)
  deriving (Functor, Applicative)

newtype ActionT base zoomed m a = ActionT
  { getAction :: FreeT (ActionF base m) (StateT zoomed m) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadState zoomed)
makeLenses ''AppState

instance Monad n => MonadFree (ActionF base n) (ActionT base zoomed n) where
  -- m :: StateT base n (ActionT base zoomed n a)
  wrap (LiftAction act) = join . ActionT . wrap . fmap return . LiftAction $ act


instance MonadTrans (ActionT base zoomed) where
  lift = ActionT . lift . lift

instance HasStates AppState where
  states = baseStates

instance HasEvents AppState where

unLift :: Monad m => FreeT (ActionF base m) (StateT base m) a -> StateT base m a
unLift m = do
  step <- runFreeT m
  case step of
    Pure a -> return a
    Free (LiftAction next) -> next >>= unLift

liftAction :: Monad m => ActionT base base m a -> ActionT base zoomed m a
liftAction = liftF .  LiftAction . unLift . getAction

execApp :: Monad m => base -> ActionT base base m a -> m a
execApp appState = flip evalStateT appState . unLift . getAction

type instance Zoomed (ActionT base zoomed m) = Zoomed (FreeT (ActionF base m) (StateT zoomed m))
instance Monad m => Zoom (ActionT base s m) (ActionT base t m) s t where
  zoom l (ActionT action) = ActionT $ zoom l action

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
