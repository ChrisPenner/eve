{-# language TemplateHaskell #-}
module Eve.Internal.AppState
  ( AppState(..)
  , App
  , Action
  ) where

import Eve.Internal.Actions
import Eve.Internal.States
import Control.Lens
import Data.Default

data AppState = AppState
  { _baseStates :: States
  }
makeLenses ''AppState

instance Default AppState where
  def = AppState mempty

instance HasStates AppState where
  states = baseStates

instance HasEvents AppState where

type App a = AppT AppState IO a
type Action s a = ActionT AppState s IO a
