module Eve.Internal.Run
  ( eve
  , eve_
  ) where

import Eve.Internal.Actions
import Eve.Internal.Listeners
import Eve.Internal.Events
import Eve.Internal.States()
import Eve.Internal.AppState

import Control.Monad
import Control.Monad.State
import Control.Lens
import Data.Default
import Data.Maybe
import Data.Typeable

import Pipes
import Pipes.Concurrent
import qualified Pipes.Parse as PP

-- | This runs your application. It accepts an initialization block (which
-- is the same as any other 'App' or 'Action' block, which
-- registers event listeners and event providers. Note that nothing in this
-- block should use 'dispatchEvent' since it is possible that not all listeners
-- have yet been registered. You can use the 'afterInit' trigger to dispatch
-- any events you'd like to run at start-up.
--
-- It is polymorphic in the Monad it operates over, so you may use it with any 
-- custom base monad which implements 'MonadIO'.
--
-- If you don't need this functionality; the easiest way to get started is to simply
-- cally it like so:
--
-- > import Eve
-- >
-- > initialize = App ()
-- > initialize = do
-- >   addListener ...
-- >   ...
-- >
-- > startApp :: IO ()
-- > startApp = eve_ initialize
eve :: (MonadIO m, Typeable m) => AppT AppState m () -> m AppState
eve initialize = do
  (output, input) <- liftIO $ spawn unbounded
  execApp (def & asyncQueue .~ Just output) $ do
    initialize
    dispatchEvent_ Init
    dispatchEvent_ AfterInit
    eventLoop $ fromInput input
    dispatchEvent_ Exit

-- | 'eve' with '()' as its return value.
eve_ :: (MonadIO m, Typeable m) => AppT AppState m () -> m ()
eve_ = void . eve

-- | This is the main event loop, it runs recursively forever until something
-- sets the exit status. It runs the pre-event listeners, then checks if any
-- async events have finished, then runs the post event listeners and repeats.
eventLoop :: (MonadIO m, HasEvents base, Typeable m) => Producer (AppT base m ()) IO () -> AppT base m ()
eventLoop producer = do
  dispatchEvent_ BeforeEvent
  (mAction, nextProducer) <- liftIO $ PP.runStateT PP.draw producer
  fromMaybe (return ()) mAction
  dispatchEvent_ AfterEvent
  shouldExit <- isExiting
  unless shouldExit $ eventLoop nextProducer
