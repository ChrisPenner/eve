module Eve.Internal.Run
  ( eve
  , eve_
  ) where

import Eve.Internal.Actions
import Eve.Internal.Listeners
import Eve.Internal.Events
import Eve.Internal.States()
import Eve.Internal.AppState

import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.State
import Control.Lens
import Data.Default
import Data.Typeable

-- | This runs your application like 'eve_',
-- It is polymorphic in the Monad it operates over, so you may use it with any
-- custom base monad which implements 'MonadIO'. Upon termination of the app it
-- returns the final 'AppState'.
eve :: (MonadIO m, Typeable m) => AppT AppState m () -> m AppState
eve initialize = do
  chan <- liftIO newChan
  execEve (def & asyncQueue .~ Just chan) $ do
    initialize
    dispatchEvent_ Init
    dispatchEvent_ AfterInit
    eventLoop chan
    dispatchEvent_ Exit

-- | This runs your application. It accepts an initialization block (which
-- is the same as any other 'App' or 'Action' block, which
-- registers event listeners and event providers. Note that nothing in this
-- block should use 'dispatchEvent' since it is possible that not all listeners
-- have yet been registered. You can use the 'afterInit' trigger to dispatch
-- any events you'd like to run at start-up.
-- Here's a simple example:
--
-- > import Eve
-- >
-- > initialize = App ()
-- > initialize = do
-- >   addListener_ myListener
-- >   asyncEventProvider myProvider
-- >
-- > startApp :: IO ()
-- > startApp = eve_ initialize
eve_ :: App () -> IO ()
eve_ = void . eve

-- | This is the main event loop, it runs recursively forever until something
-- sets the exit status. It runs the pre-event listeners, then checks if any
-- async events have finished, then runs the post event listeners and repeats.
eventLoop :: (MonadIO m, HasEvents base, Typeable m) => Chan (AppT base m ()) -> AppT base m ()
eventLoop chan = do
  dispatchEvent_ BeforeEvent
  join . liftIO $ readChan chan
  dispatchEvent_ AfterEvent
  shouldExit <- isExiting
  unless shouldExit $ eventLoop chan
