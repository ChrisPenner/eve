{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Internal.Run
  ( reflex
  ) where

import Reflex.Internal.BaseMonad
import Reflex.Internal.Listeners
import Reflex.Internal.Events

import Control.Monad.State
import Data.Maybe

import Pipes
import Pipes.Concurrent
import qualified Pipes.Parse as PP

reflex :: BaseMonad () -> IO BaseState
reflex initialize = do
  (output :: Output (BaseMonad ()), input :: Input (BaseMonad ())) <- spawn unbounded
  flip execStateT (BaseState mempty output) . runBaseMonad $ do
    initialize
    dispatchEvent_ Init
    dispatchEvent_ AfterInit
    eventLoop $ fromInput input
    dispatchEvent_ Exit

-- | This is the main event loop, it runs recursively forever until something
-- sets the exit status. It runs the pre-event listeners, then checks if any
-- async events have finished, then runs the post event listeners and repeats.
eventLoop :: Producer (BaseMonad ()) IO () -> BaseMonad ()
eventLoop producer = do
  dispatchEvent_ AfterEvent
  (mAction, nextProducer) <- liftIO $ PP.runStateT PP.draw producer
  fromMaybe (return ()) mAction
  shouldExit <- isExiting
  unless shouldExit $ eventLoop nextProducer
