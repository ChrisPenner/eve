{-# language TemplateHaskell #-}
module Fixtures
  ( store
  , CustomEvent(..)
  , noIOTest
  , ioTest
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

data Store = Store
  {_payload :: String
  } deriving (Show, Eq)
makeLenses ''Store

store :: HasStates s => Lens' s String
store = stateLens.payload

instance Default Store where
  def = Store "default"

data CustomEvent = CustomEvent

ioTest :: App () -> SpecM m AppState
ioTest = runIO . eve
