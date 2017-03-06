module Eve.Internal.Testing
  ( noEventTest
  ) where

import Eve
import Eve.Internal.Actions
import Pipes.Concurrent

noEventTest :: App a -> IO a
noEventTest test = do
  (output, _) <- spawn unbounded
  execApp (AppState mempty output) test
