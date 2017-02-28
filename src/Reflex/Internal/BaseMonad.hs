{-# language TemplateHaskell #-}
{-# language GeneralizedNewtypeDeriving #-}
module Reflex.Internal.BaseMonad
  ( BaseMonad(..)
  , BaseState(..)
  , exit
  , isExiting
  , Exiting(..)
  , asyncQueue
  ) where

import Reflex.Internal.Extensions

import Control.Monad.State
import Control.Lens

import Data.Default
import Pipes.Concurrent

newtype BaseMonad a = BaseMonad
  { runBaseMonad :: StateT BaseState IO a
  } deriving (Functor, Applicative, Monad, MonadState BaseState, MonadIO)

data BaseState = BaseState
  { _baseExts :: Exts
  , _asyncQueue :: Output (BaseMonad ())
  }
makeLenses ''BaseState

instance HasExts BaseState where
  exts = baseExts

instance HasEvents BaseState where

newtype Exiting =
  Exiting Bool
  deriving (Show, Eq)

instance Default Exiting where
  def = Exiting False

exit :: BaseMonad ()
exit = ext .= Exiting True

isExiting :: BaseMonad Bool
isExiting = do
  Exiting b <- use ext
  return b

