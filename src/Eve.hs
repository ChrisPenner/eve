module Eve
  (
  -- * Running your App
  eve
  , eve_

  -- * Working with Actions
  , App
  , Action
  , AppT
  , ActionT
  , liftApp
  , runAction
  , runActionOver
  , exit

  -- * Dispatching Events
  , dispatchEvent
  , dispatchEvent_

  , dispatchLocalEvent
  , dispatchLocalEvent_

  , dispatchEventAsync
  , dispatchActionAsync

  -- * Event Listeners
  , addListener
  , addListener_

  , addLocalListener
  , addLocalListener_

  , removeListener
  , removeLocalListener

  , Listener
  , ListenerId

  -- * Asynchronous Helpers
  , asyncActionProvider
  , asyncEventProvider
  , Dispatcher

  -- * Built-in Event Listeners
  , afterInit
  , beforeEvent
  , beforeEvent_
  , afterEvent
  , afterEvent_
  , onExit

  -- * Working with State
  -- | All application-provided states are stored in the same
  -- Map; keyed by their 'Data.Typeable.TypeRep'. This means that if more than one state
  -- uses the same type then they'll conflict and overwrite each-other (this is less of a
  -- problem than you're probably thinking). This is easily solved by simply
  -- using a newtype around any types you haven't defined yourself.
  -- For example if your application stores a counter as an Int, wrap it in your own custom
  -- @Counter@ newtype when storing it. If you wish to store multiple copies of a given state
  -- simply store them in a list or map, then store that container as your state.
  --
  -- Because states are stored by their 'Data.Typeable.TypeRep', they must define an
  -- instance of 'Data.Typeable.Typeable', luckily GHC can derive this for you with
  -- @deriving Typeable@.
  --
  -- It is also required for all states to define an instance of
  -- 'Data.Default.Default', this is because accessing an extension which has not
  -- yet been stored will result in the default value.
  --
  -- If there's no default value that makes sense for your type, you can define
  -- a default of 'Data.Maybe.Nothing' and pattern-match on its value when you
  -- access it.
  --
  -- Stored states are accessed by using the `stateLens` lens, this lens is polymorphic
  -- and can return ANY type. GHC infers the needed type and the lens will retrieve the
  -- state that you want from the store of states. It seems a bit complicated, but it all
  -- works fine in practice.
  --
  -- To avoid confusion it's best to rename a version of `stateLens` with a more restrictive
  -- type for each different state type that you store. This helps prevent strange errors and
  -- makes your code much easier to read. For example:
  --
  -- > data MyState = MyState String
  -- > myState :: HasStates s => Lens' s MyState
  -- > myState = stateLens
  -- >
  -- > myAction = do
  -- >   MyState str <- use stateLens
  --
  -- If GHC has trouble inferring the type, rename it and restrict the type as above.
  , HasStates(..)
  , States
  , HasEvents
  , stateLens
  , makeStateLens
  , AppState
  ) where

import Eve.Internal.Run
import Eve.Internal.Actions
import Eve.Internal.Listeners
import Eve.Internal.Async
import Eve.Internal.AppState
import Eve.Internal.States
