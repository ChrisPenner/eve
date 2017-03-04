{-# LANGUAGE ScopedTypeVariables #-}

module Eve.Internal.Run
  ( eve
  ) where

import Eve.Internal.App
import Eve.Internal.Listeners
import Eve.Internal.Events

import Control.Monad.State
import Data.Maybe

import Pipes
import Pipes.Concurrent
import qualified Pipes.Parse as PP

eve :: App () -> IO AppState
eve initialize = do
  (output :: Output (App ()), input :: Input (App ())) <- spawn unbounded
  execApp (AppState mempty output) $ do
    initialize
    dispatchEvent_ Init
    dispatchEvent_ AfterInit
    eventLoop $ fromInput input
    dispatchEvent_ Exit

-- | This is the main event loop, it runs recursively forever until something
-- sets the exit status. It runs the pre-event listeners, then checks if any
-- async events have finished, then runs the post event listeners and repeats.
eventLoop :: Producer (App ()) IO () -> App ()
eventLoop producer = do
  dispatchEvent_ BeforeEvent
  (mAction, nextProducer) <- liftIO $ PP.runStateT PP.draw producer
  fromMaybe (return ()) mAction
  dispatchEvent_ AfterEvent
  shouldExit <- isExiting
  unless shouldExit $ eventLoop nextProducer
