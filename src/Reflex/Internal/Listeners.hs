{-# language GADTs #-}
{-# language ExistentialQuantification #-}
{-# language ScopedTypeVariables #-}
{-# language RankNTypes #-}

module Reflex.Internal.Listeners
  ( HasEvents
  , dispatchEvent
  , dispatchEventAsync
  , addListener
  , removeListener
  , asyncEventProvider
  ) where

import Reflex.Internal.Action
import Reflex.Internal.Extensions
import Reflex.Internal.Async

import Control.Lens
import Data.Default
import Data.Typeable
import Data.Maybe
import qualified Data.Map as M

-- | A typeclass to ensure people don't dispatch events to states which shouldn't
--   accept them.
class (Typeable s, HasExts s) => HasEvents s where

dispatchEvent :: forall result eventType s. (HasEvents s, Monoid result, Typeable eventType, Typeable result) => eventType -> Action s result
dispatchEvent evt = do
  LocalListeners _ listeners <- use localListeners
  results <- traverse ($ evt) (matchingListeners listeners :: [eventType -> Action s result])
  return (mconcat results :: result)

addListener :: forall result eventType s. (HasEvents s, Typeable eventType, Typeable result, Monoid result) => (eventType -> Action s result) -> Action s ListenerId
addListener lFunc = do
  LocalListeners nextListenerId listeners <- use localListeners
  let (listener, listenerId, eventType) = mkListener nextListenerId lFunc
      newListeners = M.insertWith mappend eventType [listener] listeners
  localListeners .= LocalListeners (nextListenerId + 1) newListeners
  return listenerId
    where
      mkListener :: forall event r. (Typeable event, Typeable r, Monoid r) => Int -> (event -> Action s r) -> (Listener, ListenerId, TypeRep)
      mkListener n listenerFunc =
        let list = Listener (typeOf listenerFunc) listId listenerFunc
            listId = ListenerId n (typeRep (Proxy :: Proxy event))
            prox = typeRep (Proxy :: Proxy event)
          in (list, listId, prox)

removeListener :: HasEvents s => ListenerId -> Action s ()
removeListener listenerId@(ListenerId _ eventType) =
  localListeners %= remover
    where
      remover (LocalListeners nextListenerId listeners) =
        let newListeners = listeners & at eventType._Just %~ filter (notMatch listenerId)
        in LocalListeners nextListenerId newListeners
      notMatch idA (Listener _ idB _) = idA /= idB


-- | This function takes an IO which results in some Event, it runs the IO
-- asynchronously and dispatches the event
dispatchEventAsync :: (HasEvents s, HasAsyncQueue s, Typeable event) => IO event -> Action s ()
dispatchEventAsync ioEvent = dispatchActionAsync $ dispatchEvent <$> ioEvent

-- | A wrapper around event listeners so they can be stored in 'Listeners'.
data Listener where
  Listener :: (Typeable eventType, Typeable result, Monoid result, HasExts s, Typeable s) => TypeRep -> ListenerId -> (eventType -> Action s result) -> Listener

instance Show Listener where
  show (Listener rep (ListenerId n _) _) = "<Listener #" ++ show n ++ ", " ++ show rep ++ ">"

-- | An opaque reverence to a specific registered event-listener.
-- A ListenerId is used only to remove listeners later with 'Rasa.Internal.Listeners.removeListener'.
data ListenerId =
  ListenerId Int TypeRep
  deriving Show

instance Eq ListenerId where
  ListenerId a _ == ListenerId b _ = a == b

-- | A map of Event types to a list of listeners for that event
type Listeners = M.Map TypeRep [Listener]

-- | Store the listeners in extensions
data LocalListeners =
  LocalListeners Int Listeners
  deriving Show

instance Default LocalListeners where
  def = LocalListeners 0 M.empty

localListeners :: HasExts s => Lens' s LocalListeners
localListeners = ext

-- | This extracts all event listeners from a map of listeners which match the type of the provided event.
matchingListeners :: forall s eventType result. (HasEvents s, Typeable s, Typeable eventType, Typeable result) => Listeners -> [eventType -> Action s result]
matchingListeners listeners = catMaybes $ (getListener :: Listener -> Maybe (eventType -> Action s result)) <$> (listeners^.at (typeRep (Proxy :: Proxy eventType))._Just)

-- | Extract the listener function from eventType listener
getListener :: Typeable expected => Listener -> Maybe expected
getListener (Listener _ _ x) = cast x


-- | This is a type alias to make defining your functions for use with 'asyncEventProvider' easier;
-- It represents the function your event provider function will be passed to allow dispatching
-- events. Using this type requires the @RankNTypes@ language pragma.
type Dispatcher = forall event. Typeable event => event -> IO ()


-- | This allows long-running IO processes to provide Events to Rasa asyncronously.
--
-- Don't let the type signature confuse you; it's much simpler than it seems.
--
-- Let's break it down:
--
-- Using the 'Dispatcher' type with asyncEventProvider requires the @RankNTypes@ language pragma.
--
-- This type as a whole represents a function which accepts a 'Dispatcher' and returns an 'IO';
-- the dispatcher itself accepts data of ANY 'Typeable' type and emits it as an event (see "Rasa.Internal.Events").
--
-- When you call 'asyncEventProvider' you pass it a function which accepts a @dispatch@ function as an argument
-- and then calls it with various events within the resulting 'IO'.
--
-- Note that asyncEventProvider calls forkIO internally, so there's no need to do that yourself.
--
-- Here's an example which fires a @Timer@ event every second.
--
-- > {-# language RankNTypes #-}
-- > data Timer = Timer
-- > myTimer :: Dispatcher -> IO ()
-- > myTimer dispatch = forever $ dispatch Timer >> threadDelay 1000000
-- >
-- > myAction :: Action s ()
-- > myAction = onInit $ asyncEventProvider myTimer
asyncEventProvider :: forall s. (HasEvents s, HasAsyncQueue s) => (Dispatcher -> IO ()) -> Action s ()
asyncEventProvider asyncEventProv =
  asyncActionProvider $ eventsToActions asyncEventProv
    where
      eventsToActions :: (Dispatcher -> IO ()) -> (Action s () -> IO ()) -> IO ()
      eventsToActions aEventProv dispatcher = aEventProv (dispatcher . dispatchEvent)
