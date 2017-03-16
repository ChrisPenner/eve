{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Eve.Internal.Listeners
  ( HasEvents
  , dispatchEvent
  , dispatchEvent_
  , dispatchLocalEvent
  , dispatchLocalEvent_
  , dispatchEventAsync

  , addListener
  , addListener_
  , addLocalListener
  , addLocalListener_

  , removeListener
  , removeLocalListener

  , asyncEventProvider

  , afterInit
  , beforeEvent
  , beforeEvent_
  , afterEvent
  , afterEvent_
  , onExit

  , Listener
  , ListenerId
  , EventDispatcher
  ) where

import Eve.Internal.States
import Eve.Internal.Async
import Eve.Internal.Actions
import Eve.Internal.Events

import Control.Monad.State
import Control.Lens

import Data.Default
import Data.Typeable
import Data.Maybe
import qualified Data.Map as M

-- | Registers an action to be performed directly following the Initialization phase.
--
-- At this point any listeners in the initialization block have run, so you may 'dispatchEvent's here.
afterInit :: forall base m a. (Monad m, HasEvents base, Typeable m) => AppT base m a -> AppT base m ()
afterInit action = addListener_ (const (void action) :: AfterInit -> AppT base m ())

-- | Registers an action to be performed BEFORE each async event is processed phase.
beforeEvent :: forall base zoomed m a. (Monad m, HasEvents base, Typeable m) => AppT base m a -> ActionT base zoomed m ListenerId
beforeEvent action = addListener (const (void action) :: BeforeEvent -> AppT base m ())

beforeEvent_ :: (Monad m, HasEvents base, Typeable m) => AppT base m a -> ActionT base zoomed m ()
beforeEvent_ = void . beforeEvent

-- | Registers an action to be performed AFTER each event phase.
afterEvent :: forall base zoomed m a. (Monad m, HasEvents base, Typeable m) => AppT base m a -> ActionT base zoomed m ListenerId
afterEvent action = addListener (const (void action) :: AfterEvent -> AppT base m ())

afterEvent_ :: (Monad m, HasEvents base, Typeable m) => AppT base m a -> ActionT base zoomed m ()
afterEvent_ = void . afterEvent

-- | Registers an action to be run before shutdown. Any asynchronous combinators used in this block will NOT be run.
onExit :: forall base zoomed m a. (HasEvents base, Typeable m, Monad m) => AppT base m a -> ActionT base zoomed m ()
onExit action = addListener_ (const (void action) :: Exit -> AppT base m ())

-- | A local version of 'dispatchEvent'.
-- The local version dispatches the event in the context of the current 'Action',
-- If you don't know what this means, you probably want 'dispatchEvent' instead
dispatchLocalEvent
  :: forall result eventType m s.
     (MonadState s m
     ,HasEvents s
     ,Monoid result
     ,Typeable m
     ,Typeable eventType
     ,Typeable result)
  => eventType -> m result
dispatchLocalEvent evt = do
  LocalListeners _ listeners <- use localListeners
  results <-
    traverse ($ evt) (matchingListeners listeners :: [eventType -> m result])
  return (mconcat results :: result)

dispatchLocalEvent_
  :: forall eventType m s.
     (MonadState s m
     ,HasEvents s
     ,Typeable m
     ,Typeable eventType)
  => eventType -> m ()
dispatchLocalEvent_ = dispatchLocalEvent

-- | Runs any listeners registered for the provided event with the provided event;
--
-- You can also 'query' listeners and receive a ('Monoid'al) result.
--
-- > data RequestNames = GetFirstName | GetLastName
-- > provideName1, provideName2 :: RequestNames -> App [String]
-- > provideName1 GetFirstNames = return ["Bob"]
-- > provideName1 GetLastNames = return ["Smith"]
-- > provideName2 GetFirstNames = return ["Sally"]
-- > provideName2 GetLastNames = return ["Jenkins"]
-- >
-- > -- Note that if we registered an action of type 'GetFirstName -> ()' it would NOT
-- > -- be run in response to the following 'dispatchEvent', since it's type doesn't match.
-- >
-- > greetNames :: App [String]
-- > greetNames = do
-- >   addListener_ provideName1
-- >   addListener_ provideName2
-- >   firstNames <- dispatchEvent GetFirstName
-- >   lastNames <- dispatchEvent GetLastName
-- >   liftIO $ print firstNames
-- >   -- ["Bob", "Sally"]
-- >   liftIO $ print lastNames
-- >   -- ["Smith", "Jenkins"]
dispatchEvent
  :: forall result eventType m base zoomed.
     (HasEvents base
     ,Monoid result
     ,Monad m
     ,Typeable m
     ,Typeable eventType
     ,Typeable result)
    => eventType -> ActionT base zoomed m result
dispatchEvent evt = runApp $ dispatchLocalEvent evt

dispatchEvent_
  :: forall eventType m base zoomed.
     (HasEvents base
     ,Monad m
     ,Typeable m
     ,Typeable eventType)
    => eventType -> ActionT base zoomed m ()
dispatchEvent_ = dispatchEvent

-- | The local version of 'addListener'. It will register a listener within an 'Action's local event
-- context. If you don't know what this means you probably want 'addListener' instead.
addLocalListener
  :: forall result eventType m s.
     (MonadState s m
     ,HasEvents s
     ,Typeable m
     ,Typeable eventType
     ,Typeable result
     ,Monoid result)
  => (eventType -> m result) -> m ListenerId
addLocalListener lFunc = do
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

addLocalListener_
  :: forall result eventType m s.
     (MonadState s m
     ,HasEvents s
     ,Typeable m
     ,Typeable eventType
     ,Typeable result
     ,Monoid result)
  => (eventType -> m result) -> m ()
addLocalListener_ = void . addLocalListener

-- | Registers an 'Action' or 'App' to respond to an event.
--
-- For a given use: @addListener myListener@, @myListener@ might have the type @MyEvent -> App a@
-- it will register the function @myListener@ to be run in response to a @dispatchEvent (MyEvent eventInfo)@
-- and will be provided @(MyEvent eventInfo)@ as an argument.
--
-- This returns a 'ListenerId' which corresponds to the registered listener for use with 'removeListener'
addListener
  :: forall result eventType m base zoomed.
     (HasEvents base
     ,Monad m
     ,Typeable m
     ,Typeable eventType
     ,Typeable result
     ,Monoid result)
    => (eventType -> AppT base m result) -> ActionT base zoomed m ListenerId
addListener = runApp . addLocalListener

addListener_
  :: forall result eventType m base zoomed.
     (HasEvents base
     ,Monad m
     ,Typeable m
     ,Typeable eventType
     ,Typeable result
     ,Monoid result)
    => (eventType -> AppT base m result) -> ActionT base zoomed m ()
addListener_ = void . addListener

-- | The local version of 'removeListener'.
-- This removes a listener from an 'Action's event context. If you don't
-- know what this means you probably want 'removeListener' instead.
removeLocalListener
  :: (MonadState s m, HasEvents s)
  => ListenerId -> m ()
removeLocalListener listenerId@(ListenerId _ eventType) = localListeners %= remover
  where
    remover (LocalListeners nextListenerId listeners) =
      let newListeners =
            listeners & at eventType . _Just %~ filter (notMatch listenerId)
      in LocalListeners nextListenerId newListeners
    notMatch idA (Listener _ idB _) = idA /= idB

-- | Unregisters a listener referred to by the provided 'ListenerId'
removeListener
  :: (HasEvents base
     ,Monad m)
    => ListenerId -> ActionT base zoomed m ()
removeListener = runApp . removeLocalListener

-- | This function takes an IO which results in some event, it runs the IO
-- asynchronously, THEN dispatches the event. Note that only the
-- code which generates the event is asynchronous, not any responses to the event
-- itself.
dispatchEventAsync
  :: (Typeable event
     ,MonadIO m
     ,Typeable m
     ,HasEvents base
     ) => IO event -> ActionT base zoomed m ()
dispatchEventAsync ioEvent = dispatchActionAsync $ dispatchEvent <$> ioEvent

-- | A wrapper around event listeners so they can be stored in 'Listeners'.
data Listener where
        Listener ::
            (MonadState s m, Typeable m, Typeable eventType, Typeable result,
             Monoid result, HasStates s) =>
            TypeRep -> ListenerId -> (eventType -> m result) -> Listener

-- | An opaque reverence to a specific registered event-listener.
-- A ListenerId is used only to remove listeners later with 'removeListener'.
data ListenerId =
  ListenerId Int
             TypeRep

instance Eq ListenerId where
  ListenerId a _ == ListenerId b _ = a == b

-- | A map of event types to a list of listeners for that event
type Listeners = M.Map TypeRep [Listener]

-- | Store the listeners in the state-map
data LocalListeners =
  LocalListeners Int
                 Listeners

instance Default LocalListeners where
  def = LocalListeners 0 M.empty

localListeners
  :: HasStates s
  => Lens' s LocalListeners
localListeners = stateLens

-- | This extracts all event listeners from a map of listeners which match the type of the provided event.
matchingListeners
  :: forall m eventType result.
     (Typeable m
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
type EventDispatcher = forall event. Typeable event =>
                                event -> IO ()

-- | This allows long-running IO processes to provide Events to the application asyncronously.
--
-- Don't let the type signature confuse you; it's much simpler than it seems.
--
-- Let's break it down:
--
-- Using the 'EventDispatcher' type with asyncEventProvider requires the @RankNTypes@ language pragma.
--
-- This type as a whole represents a function which accepts an 'EventDispatcher' and returns an 'IO';
-- the dispatcher itself accepts data of ANY 'Typeable' type and emits it as an event.
--
-- When you call 'asyncEventProvider' you pass it a function which accepts a @dispatch@ function as an argument
-- and then calls it with various events within the resulting 'IO'.
--
-- Note that this function calls forkIO internally, so there's no need to do that yourself.
--
-- Here's an example which fires a @Timer@ event every second.
--
-- > {-# language RankNTypes #-}
-- > data Timer = Timer
-- > myTimer :: EventDispatcher -> IO ()
-- > myTimer dispatch = forever $ dispatch Timer >> threadDelay 1000000
-- >
-- > myInit :: App ()
-- > myInit = asyncEventProvider myTimer
asyncEventProvider
  :: (HasEvents base, MonadIO m, Typeable m) => (EventDispatcher -> IO ()) -> ActionT base zoomed m ()
asyncEventProvider asyncEventProv = asyncActionProvider $ eventsToActions asyncEventProv
  where
    eventsToActions :: (Monad m, HasEvents base, Typeable m) => (EventDispatcher -> IO ()) -> (AppT base m () -> IO ()) -> IO ()
    eventsToActions aEventProv dispatcher =
      aEventProv (dispatcher . dispatchEvent)
