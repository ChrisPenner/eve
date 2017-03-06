{-# language TemplateHaskell #-}
module Fixtures
  ( TestState(..)
  , store
  , CustomEvent(..)
  , noIOTest
  , ioTest
  ) where

import Eve.Internal.States
import Eve.Internal.Listeners
import Eve.Internal.Actions
import Eve.Internal.Run

import Control.Lens
import Data.Default

import Test.Hspec.Core.Spec (SpecM)
import Test.Hspec

data TestState = TestState
  { _testStates :: States
  }
makeLenses ''TestState

instance HasStates TestState where
  states = testStates

instance HasEvents TestState

emptyState = TestState mempty

data Store = Store
  {_payload :: String
  } deriving (Show, Eq)
makeLenses ''Store

store :: HasStates s => Lens' s String
store = stateLens.payload

instance Default Store where
  def = Store "default"

data CustomEvent = CustomEvent

noIOTest :: AppT TestState Identity a -> (a, TestState)
noIOTest = runIdentity . runApp emptyState

ioTest :: AppT TestState IO () -> SpecM m TestState
ioTest = runIO . eveT emptyState
