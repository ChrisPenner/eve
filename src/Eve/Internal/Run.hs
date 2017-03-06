module Eve.Internal.Run
  ( eve
  , eveT
  ) where

import Eve.Internal.Actions
import Eve.Internal.AppState
import Eve.Internal.Listeners
import Eve.Internal.Events
import Eve.Internal.States()

import Control.Monad
import Control.Monad.State
import Control.Lens
import Data.Default
import Data.Maybe
import Data.Typeable

import Pipes
import Pipes.Concurrent
import qualified Pipes.Parse as PP

eve :: App () -> IO ()
eve = void . eveT def

eveT :: (MonadIO m, Typeable m, HasEvents base) => base -> AppT base m () -> m base
eveT startState initialize = do
  (output, input) <- liftIO $ spawn unbounded
  execApp (startState & asyncQueue .~ Just output) $ do
    initialize
    dispatchEvent_ Init
    dispatchEvent_ AfterInit
    eventLoop $ fromInput input
    dispatchEvent_ Exit
    get

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
