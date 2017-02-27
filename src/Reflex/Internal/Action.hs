{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Reflex.Internal.Action
  ( Action(..)
  ) where

import Reflex.Internal.Extensions

import Control.Lens
import Control.Monad.State
import Data.Default

data ActionState = ActionState
  { _actionExt :: Exts
  }

makeClassy ''ActionState

instance Default ActionState where
  def = ActionState def

instance HasExts ActionState where
  exts = actionExt

newtype Action s a = Action
  { getAction :: StateT s IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadState s)
-- instance Zoom (Action a) (Action s) a s where
--   zoom l (Action m) = Action (zoom l m)
-- type instance Zoomed (Action s) = Zoomed (StateT s IO)
-- runAnyAction :: (HasExts dst, Typeable src, Default src) => Action src a -> Action dst a
-- runAnyAction = Action . zoom ext . getAction
-- runOver :: Lens' dst src -> Action src a -> Action dst a
-- runOver l = Action . zoom l . getAction
-- runOverThis :: thing -> Action thing a -> Action any (a, thing)
-- runOverThis s = liftIO . flip runStateT s . getAction
