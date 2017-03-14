{-# language TemplateHaskell #-}
module Eve.Internal.AppState
  ( AppState(..)
  , App
  , Action
  , ActionM
  , AppM

  , exit
  , isExiting

  , asyncQueue
  ) where

import Eve.Internal.Actions
import Eve.Internal.States
import Control.Lens
import Data.Default
import Data.Typeable
import Control.Concurrent.Chan

-- | A basic default state which underlies 'App' Contains only a map of 'States'.
data AppState = AppState
  { _baseStates :: States
  }
makeLenses ''AppState

instance Default AppState where
  def = AppState mempty

instance HasStates AppState where
  states = baseStates

instance HasEvents AppState where

newtype AsyncQueue base m = AsyncQueue
  { _asyncQueue' :: Maybe (Chan (AppT base m ()))
  } deriving Typeable
makeLenses ''AsyncQueue

instance Default (AsyncQueue base m) where
  def = AsyncQueue Nothing

-- | Accesses a queue for dispatching async actions.
asyncQueue :: (HasStates s, Typeable m, Typeable base) => Lens' s (Maybe (Chan (AppT base m ())))
asyncQueue = stateLens.asyncQueue'

newtype Exiting =
  Exiting Bool

instance Default Exiting where
  def = Exiting False

-- | Tells the application to quit. This triggers 'onExit' listeners
-- following the current event loop.
exit :: (Monad m, HasStates s) => ActionT s zoomed m ()
exit = liftApp $ stateLens .= Exiting True

-- | Checks whether we're in the process of exiting.
isExiting :: (Monad m, HasStates s) => ActionT s zoomed m Bool
isExiting = liftApp $ do
  Exiting b <- use stateLens
  return b


-- | An App is a base level monad which operates over your main application
-- state. You may call 'runAction' inside an app to run 'Action's over other states.
-- need to specify your own custom base state.
type App a = AppT AppState IO a

-- | An Action is a monad over some zoomed in state, they are run inside 'App' using
-- 'runAction'. For example an Action which operates over a String somewhere in your app state
-- would be written as:
--
-- alterString :: Action String ()
type Action state a = ActionT AppState state IO a

-- | A more general version of 'App' which lets you specify the underlying monad.
type AppM m a = AppT AppState m a

-- | A more general version of 'Action' which lets you to specify the underlying monad.
type ActionM s m a = ActionT AppState s m a
