{-# language TemplateHaskell #-}
module Fixtures
  ( ExtState(..)
  ) where

import Reflex.Internal.Extensions
import Reflex.Internal.Listeners

import Control.Lens

data ExtState = ExtState
  { _testExts :: Exts
  }

makeLenses ''ExtState

instance HasExts ExtState where
  exts = testExts

instance HasEvents ExtState
