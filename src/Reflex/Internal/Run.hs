{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Internal.Run
  ( reflex
  ) where

import Reflex.Internal.App
import Reflex.Internal.Listeners
import Reflex.Internal.Events

import Control.Monad.State
import Data.Maybe

import Pipes
import Pipes.Concurrent
import qualified Pipes.Parse as PP

reflex :: App () -> IO AppState
reflex initialize = do
  (output :: Output (App ()), input :: Input (App ())) <- spawn unbounded
  flip execStateT (AppState mempty output) . runApp $ do
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
