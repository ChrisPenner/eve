{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Reflex.Internal.Listeners
  ( HasEvents
  , dispatchEvent
  , dispatchEvent_
  , dispatchEventAsync
  , addListener
  , addListener_
  , removeListener
  , asyncEventProvider
  ) where

import Reflex.Internal.Extensions
import Reflex.Internal.Async
import Reflex.Internal.BaseMonad

import Control.Monad.State
import Control.Lens

import Data.Default
import Data.Typeable
import Data.Maybe
import qualified Data.Map as M

dispatchEvent
  :: forall result eventType m s.
     (MonadState s m
     ,HasEvents s
     ,Monoid result
     ,Typeable m
     ,Typeable eventType
     ,Typeable result)
  => eventType -> m result
dispatchEvent evt = do
  LocalListeners _ listeners <- use localListeners
  results <-
    traverse ($ evt) (matchingListeners listeners :: [eventType -> m result])
  return (mconcat results :: result)

dispatchEvent_
  :: forall eventType m s.
     (MonadState s m
     ,HasEvents s
     ,Typeable m
     ,Typeable eventType)
  => eventType -> m ()
dispatchEvent_ = dispatchEvent


addListener
  :: forall result eventType m s.
     (MonadState s m
     ,HasEvents s
     ,Typeable m
     ,Typeable eventType
     ,Typeable result
     ,Monoid result)
  => (eventType -> m result) -> m ListenerId
addListener lFunc = do
  LocalListeners nextListenerId listeners <- use localListeners
  let (listener, listenerId, eventType) = mkListener nextListenerId lFunc
      newListeners = M.insertWith mappend eventType [listener] listeners
  localListeners .= LocalListeners (nextListenerId + 1) newListeners
  return listenerId
  where
    mkListener
      :: forall event r.
         (Typeable event, Typeable r, Monoid r)
      => Int -> (event -> m r) -> (Listener, ListenerId, TypeRep)
    mkListener n listenerFunc =
      let list = Listener (typeOf listenerFunc) listId listenerFunc
          listId = ListenerId n (typeRep (Proxy :: Proxy event))
          prox = typeRep (Proxy :: Proxy event)
      in (list, listId, prox)

addListener_
  :: forall result eventType m s.
     (MonadState s m
     ,HasEvents s
     ,Typeable m
     ,Typeable eventType
     ,Typeable result
     ,Monoid result)
  => (eventType -> m result) -> m ()
addListener_ = void . addListener

removeListener
  :: (MonadState s m, HasEvents s)
  => ListenerId -> m ()
removeListener listenerId@(ListenerId _ eventType) = localListeners %= remover
  where
    remover (LocalListeners nextListenerId listeners) =
      let newListeners =
            listeners & at eventType . _Just %~ filter (notMatch listenerId)
      in LocalListeners nextListenerId newListeners
    notMatch idA (Listener _ idB _) = idA /= idB

-- | This function takes an IO which results in some Event, it runs the IO
-- asynchronously and dispatches the event
dispatchEventAsync
  :: (Typeable event)
  => IO event -> BaseMonad ()
dispatchEventAsync ioEvent = dispatchActionAsync $ dispatchEvent <$> ioEvent

-- | A wrapper around event listeners so they can be stored in 'Listeners'.
data Listener where
        Listener ::
            (MonadState s m, Typeable m, Typeable eventType, Typeable result,
             Monoid result, HasExts s) =>
            TypeRep -> ListenerId -> (eventType -> m result) -> Listener

instance Show Listener where
  show (Listener rep (ListenerId n _) _) =
    "<Listener #" ++ show n ++ ", " ++ show rep ++ ">"

-- | An opaque reverence to a specific registered event-listener.
-- A ListenerId is used only to remove listeners later with 'Rasa.Internal.Listeners.removeListener'.
data ListenerId =
  ListenerId Int
             TypeRep
  deriving (Show)

instance Eq ListenerId where
  ListenerId a _ == ListenerId b _ = a == b

-- | A map of Event types to a list of listeners for that event
type Listeners = M.Map TypeRep [Listener]

-- | Store the listeners in extensions
data LocalListeners =
  LocalListeners Int
                 Listeners
  deriving (Show)

instance Default LocalListeners where
  def = LocalListeners 0 M.empty

localListeners
  :: HasExts s
  => Lens' s LocalListeners
localListeners = ext

-- | This extracts all event listeners from a map of listeners which match the type of the provided event.
matchingListeners
  :: forall m s eventType result.
     (MonadState s m
     ,HasEvents s
     ,Typeable m
     ,Typeable eventType
     ,Typeable result)
  => Listeners -> [eventType -> m result]
matchingListeners listeners =
  catMaybes $ (getListener :: Listener -> Maybe (eventType -> m result)) <$>
  (listeners ^. at (typeRep (Proxy :: Proxy eventType)) . _Just)

-- | Extract the listener function from eventType listener
getListener
  :: Typeable expected
  => Listener -> Maybe expected
getListener (Listener _ _ x) = cast x

-- | This is a type alias to make defining your functions for use with 'asyncEventProvider' easier;
-- It represents the function your event provider function will be passed to allow dispatching
-- events. Using this type requires the @RankNTypes@ language pragma.
type Dispatcher = forall event. Typeable event =>
                                event -> IO ()

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
asyncEventProvider
  :: (Dispatcher -> IO ()) -> BaseMonad ()
asyncEventProvider asyncEventProv = asyncActionProvider $ eventsToActions asyncEventProv
  where
    eventsToActions :: (Dispatcher -> IO ()) -> (BaseMonad () -> IO ()) -> IO ()
    eventsToActions aEventProv dispatcher =
      aEventProv (dispatcher . dispatchEvent)
