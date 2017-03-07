{-# language GeneralizedNewtypeDeriving #-}
{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language ScopedTypeVariables #-}
module Eve.Internal.Actions
  ( AppF(..)
  , ActionT(..)
  , AppT

  , runApp
  , evalApp
  , execApp

  , liftApp
  , runAction
  ) where

import Control.Monad.State
import Control.Monad.Trans.Free
import Control.Lens

-- | An 'App' has the same base and zoomed values.
type AppT s m a = ActionT s s m a

-- | A Free Functor for storing lifted App actions.
newtype AppF base m next =
  LiftApp (StateT base m next)
  deriving (Functor, Applicative)

-- | Base Action type. Allows paramaterization over application state, zoomed state
-- and underlying monad.
newtype ActionT base zoomed m a = ActionT
  { getAction :: FreeT (AppF base m) (StateT zoomed m) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadState zoomed)

instance Monad n => MonadFree (AppF base n) (ActionT base zoomed n) where
  wrap (LiftApp act) = join . ActionT . liftF . LiftApp $ act

instance MonadTrans (ActionT base zoomed) where
  lift = ActionT . lift . lift

-- | Helper method to run FreeTs.
unLift :: Monad m => FreeT (AppF base m) (StateT base m) a -> StateT base m a
unLift m = do
  step <- runFreeT m
  case step of
    Pure a -> return a
    Free (LiftApp next) -> next >>= unLift

-- | Allows 'zoom'ing 'Action's.
type instance Zoomed (ActionT base zoomed m) = Zoomed (FreeT (AppF base m) (StateT zoomed m))
instance Monad m => Zoom (ActionT base s m) (ActionT base t m) s t where
  zoom l (ActionT action) = ActionT $ zoom l action

-- | Given a 'Lens' or 'Traversal' or something similar from "Control.Lens"
-- which focuses the state (t) of an 'Action' from a base state (s),
-- this will convert @Action t a -> Action s a@.
--
-- Given a lens @HasStates s => Lens' s t@ it can also convert @Action t a -> App a@
runAction :: Zoom m n s t => LensLike' (Zoomed m c) t s -> m c -> n c
runAction = zoom

-- | Allows you to run an 'App' or 'AppM' inside of an 'Action' or 'ActionM'
liftApp :: Monad m => AppT base m a -> ActionT base zoomed m a
liftApp = liftF .  LiftApp . unLift . getAction

-- | Runs an application and returns the value and state.
runApp :: Monad m => base -> AppT base m a -> m (a, base)
runApp baseState = flip runStateT baseState . unLift . getAction

-- | Runs an application and returns the resulting value.
evalApp :: Monad m => base -> AppT base m a -> m a
evalApp baseState = fmap fst . runApp baseState

-- | Runs an application and returns the resulting state.
execApp :: Monad m => base -> AppT base m a -> m base
execApp baseState = fmap snd . runApp baseState
