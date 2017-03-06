{-# language TemplateHaskell #-}
module Fixtures
  ( TestState(..)
  , store
  , CustomEvent(..)
  , emptyStates
  ) where

import Eve.Internal.States
import Eve.Internal.Listeners

import Control.Lens
import Data.Default

data TestState = TestState
  { _testStates :: States
  }
makeLenses ''TestState

instance HasStates TestState where
  states = testStates

instance HasEvents TestState

emptyStates = TestState mempty

data Store = Store
  {_payload :: String
  } deriving (Show, Eq)
makeLenses ''Store

store :: HasStates s => Lens' s String
store = stateLens.payload

instance Default Store where
  def = Store "default"

data CustomEvent = CustomEvent
