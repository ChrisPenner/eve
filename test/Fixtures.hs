{-# language TemplateHaskell #-}
module Fixtures
  ( store
  , CustomEvent(..)
  , noIOTest
  , ioTest
  , NestedStates
  , nestedString
  ) where

import Eve.Testing
import Eve.Internal.States
import Eve.Internal.Listeners
import Eve.Internal.Actions
import Eve.Internal.AppState
import Eve.Internal.Run

import Control.Lens
import Data.Default

import Test.Hspec.Core.Spec (SpecM)
import Test.Hspec

data NestedStates = NestedStates
  { _nestedStates :: States
  , _nestedString :: String
  }
makeLenses ''NestedStates

instance Default NestedStates where
  def = NestedStates mempty "default"

instance HasStates NestedStates where
  states = nestedStates

instance HasEvents NestedStates

data Store = Store
  {_payload :: String
  }
makeLenses ''Store

store :: HasStates s => Lens' s String
store = stateLens.payload

instance Default Store where
  def = Store "default"

data CustomEvent = CustomEvent

ioTest :: App () -> SpecM m AppState
ioTest = runIO . eve
