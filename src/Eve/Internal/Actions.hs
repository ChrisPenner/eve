{-# language GeneralizedNewtypeDeriving #-}
{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
module Eve.Internal.Actions
  ( ActionF(..)
  , ActionT(..)
  , AppT
  , runApp
  , evalApp
  , execApp

  , liftAction
  , runAction

  , exit
  , isExiting
  , asyncQueue
  , Exiting(..)
  ) where

import Eve.Internal.States

import Control.Monad.State
import Control.Monad.Trans.Free
import Control.Lens
import Data.Default
import Data.Typeable

import Pipes.Concurrent

type AppT s m a = ActionT s s m a

newtype ActionF base m next =
  LiftAction (StateT base m next)
  deriving (Functor, Applicative)

newtype ActionT base zoomed m a = ActionT
  { getAction :: FreeT (ActionF base m) (StateT zoomed m) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadState zoomed)

instance Monad n => MonadFree (ActionF base n) (ActionT base zoomed n) where
  wrap (LiftAction act) = join . ActionT . liftF . LiftAction $ act

instance MonadTrans (ActionT base zoomed) where
  lift = ActionT . lift . lift

unLift :: Monad m => FreeT (ActionF base m) (StateT base m) a -> StateT base m a
unLift m = do
  step <- runFreeT m
  case step of
    Pure a -> return a
    Free (LiftAction next) -> next >>= unLift

liftAction :: Monad m => AppT base m a -> ActionT base zoomed m a
liftAction = liftF .  LiftAction . unLift . getAction

runApp :: Monad m => base -> AppT base m a -> m (a, base)
runApp baseState = flip runStateT baseState . unLift . getAction

evalApp :: Monad m => base -> AppT base m a -> m a
evalApp baseState = fmap fst . runApp baseState

execApp :: Monad m => base -> AppT base m a -> m base
execApp baseState = fmap snd . runApp baseState

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

exit :: (Monad m, HasStates s) => ActionT s zoomed m ()
exit = liftAction $ stateLens .= Exiting True

isExiting :: (Monad m, HasStates s) => ActionT s zoomed m Bool
isExiting = liftAction $ do
  Exiting b <- use stateLens
  return b

newtype AsyncQueue base m = AsyncQueue
  { _asyncQueue' :: Maybe (Output (AppT base m ()))
  } deriving Typeable
makeLenses ''AsyncQueue

instance Show (AsyncQueue base m) where
  show _ = "Async Queue"

instance Default (AsyncQueue base m) where
  def = AsyncQueue Nothing

asyncQueue :: (HasStates s, Typeable m, Typeable base) => Lens' s (Maybe (Output (AppT base m ())))
asyncQueue = stateLens.asyncQueue'
