{-# language GeneralizedNewtypeDeriving #-}
{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language ScopedTypeVariables #-}
module Eve.Internal.Actions
  ( AppF(..)
  , ActionT(..)
  , AppT

  , runEve
  , evalEve
  , execEve

  , runApp
  , runAction
  , runActionOver
  ) where

import Eve.Internal.States
import Control.Monad.State
import Control.Monad.Trans.Free
import Control.Lens
import Data.Typeable
import Data.Default
import Data.Semigroup

-- | An 'App' has the same base and zoomed values.
type AppT s m a = ActionT s s m a

-- | A Free Functor for storing lifted App actions.
newtype AppF base m next =
  RunApp (StateT base m next)
  deriving (Functor, Applicative)

-- | Base Action type. Allows paramaterization over application state, zoomed state
-- and underlying monad.
newtype ActionT base zoomed m a = ActionT
  { getAction :: FreeT (AppF base m) (StateT zoomed m) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadState zoomed)

instance (Semigroup a, Monad m) => Semigroup (ActionT base zoomed m a) where
  a <> b = do
    a' <- a
    b' <- b
    return $ a' <> b'

instance (Monoid a, Monad m) => Monoid (ActionT base zoomed m a) where
  mempty = return mempty
  mappend = (<>)

instance Monad n => MonadFree (AppF base n) (ActionT base zoomed n) where
  wrap (RunApp act) = join . ActionT . liftF . RunApp $ act

instance MonadTrans (ActionT base zoomed) where
  lift = ActionT . lift . lift

-- | Helper method to run FreeTs.
unLift :: Monad m => FreeT (AppF base m) (StateT base m) a -> StateT base m a
unLift m = do
  step <- runFreeT m
  case step of
    Pure a -> return a
    Free (RunApp next) -> next >>= unLift

-- | Allows 'zoom'ing 'Action's.
type instance Zoomed (ActionT base zoomed m) = Zoomed (FreeT (AppF base m) (StateT zoomed m))
instance Monad m => Zoom (ActionT base s m) (ActionT base t m) s t where
  zoom l (ActionT action) = ActionT $ zoom l action

-- | This runs an @'Action' MyState a@ over the MyState which is
-- stored in the currently focused state and returns the result.
-- Use 'runActionOver' if you'd like to specify a particular @MyState@
-- which is accessed by a 'Lens' or 'Traversal'.
runAction :: (HasStates t, Functor (Zoomed m c), Default s, Typeable s, Zoom m n s t) => m c -> n c
runAction = zoom stateLens

-- | Given a 'Lens' or 'Traversal' or LensLike from "Control.Lens"
-- which focuses the state (t) of an 'Action' from a base state (s),
-- this will convert @Action t a -> Action s a@ so that it may be run
-- in an @Action s a@
runActionOver :: Zoom m n s t => LensLike' (Zoomed m c) t s -> m c -> n c
runActionOver = zoom

-- | Allows you to run an 'App' inside of an 'Action'
runApp :: Monad m => AppT base m a -> ActionT base zoomed m a
runApp = liftF .  RunApp . unLift . getAction

-- | Runs an application and returns the value and state.
runEve :: Monad m => base -> AppT base m a -> m (a, base)
runEve baseState = flip runStateT baseState . unLift . getAction

-- | Runs an application and returns the resulting value.
evalEve :: Monad m => base -> AppT base m a -> m a
evalEve baseState = fmap fst . runEve baseState

-- | Runs an application and returns the resulting state.
execEve :: Monad m => base -> AppT base m a -> m base
execEve baseState = fmap snd . runEve baseState
