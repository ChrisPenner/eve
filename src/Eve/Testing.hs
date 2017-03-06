module Eve.Testing
  ( noIOTest
  ) where

import Eve.Internal.Actions
import Eve.Internal.Run
import Eve.Internal.AppState

import Control.Monad.Identity
import Data.Default

noIOTest :: AppT AppState Identity a -> (a, AppState)
noIOTest = runIdentity . runApp def
