module Eve.Internal.Testing
  ( AppState(..)
  , Exiting(..)
  , noEventTest
  ) where

import Eve
import Eve.Internal.Actions
import Eve.Internal.Events
import Pipes.Concurrent

noEventTest :: App a -> IO a
noEventTest test = do
  (output, input) <- spawn unbounded
  execApp (AppState mempty output) $ test
