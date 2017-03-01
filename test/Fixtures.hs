{-# language TemplateHaskell #-}
module Fixtures
  ( ExtState(..)
  , store
  , CustomEvent(..)
  , emptyExts
  ) where

import Reflex.Internal.Extensions
import Reflex.Internal.Listeners

import Control.Lens
import Data.Default

data ExtState = ExtState
  { _testExts :: Exts
  }

makeLenses ''ExtState

instance HasExts ExtState where
  exts = testExts

instance HasEvents ExtState

emptyExts = ExtState mempty

data Store = Store 
  {_payload :: String
  } deriving (Show, Eq)
makeLenses ''Store

store :: HasExts s => Lens' s String
store = ext.payload

instance Default Store where
  def = Store "default"

data CustomEvent = CustomEvent
