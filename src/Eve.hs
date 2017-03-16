module Eve
  (
  -- | This documentation is split into parts based on complexity.
  -- For most applications you'll need only the Simple section. You'll
  -- find useful tools in the Advanced section once you've got a simple app
  -- up and running.

  -- * Simple
  -- | Eve allows you to build your applications incrementally, adding more
  -- complexity as you need it. For this reason, many of the types are more
  -- general than you'll likely need. This can be a bit confusing, but here's
  -- a few tips:
  --
  --    * Both 'Action' and 'App' unify with 'ActionT'. You may use them in place of
  --    'ActionT'. When in doubt, use 'App'.
  --    * When you see vague references to monads @m@ or @n@, you can use 'App' or 'Action' in its place.
  --    * Simple Apps assume that you use the provided 'AppState' and it is
  --      "baked in" to the 'Action' and 'App' types. Wherever you see @'HasStates' s@
  --      you can mentally replace @s@ with 'AppState'.

  -- ** Running your App
  eve_

  -- ** Working with Actions
  , App
  , Action
  , runApp
  , runAction
  , exit

  -- ** Dispatching Events
  , dispatchEvent
  , dispatchEvent_

  -- ** Event Listeners
  , addListener
  , addListener_

  , removeListener

  , Listener
  , ListenerId

  -- ** Asynchronous Helpers
  , asyncEventProvider
  , EventDispatcher

  -- ** Built-in Event Listeners
  , afterInit
  , beforeEvent
  , beforeEvent_
  , afterEvent
  , afterEvent_
  , onExit

  -- ** Working with State
  -- | All application-provided states are stored in the same
  -- Map; keyed by their 'Data.Typeable.TypeRep'. This means that if more than one state
  -- uses the same type then they'll conflict and overwrite each-other (this is less of a
  -- problem than you're probably thinking). This is easily solved by simply
  -- using a newtype around any types you haven't defined yourself.
  -- For example if your application stores a counter as an Int, wrap it in your own custom
  -- @Counter@ newtype when storing it. If you wish to store multiple copies of a given state
  -- simply store them in a list or map, then store that container as your state.
  --
  -- Because states are stored by their 'Data.Typeable.TypeRep', they must
  -- define an instance of 'Data.Typeable.Typeable', In most cases it's
  -- unnecessary, but GHC can derive this for you with @deriving Typeable@.
  --
  -- It is also required for all states to define an instance of
  -- 'Data.Default.Default', this is because accessing an extension which has not
  -- yet been stored will result in the default value.
  --
  -- If there's no default value that makes sense for your type, you can define
  -- a default of 'Data.Maybe.Nothing' and pattern-match on its value when you
  -- access it.
  --
  -- Here's an example of defining your own state:
  --
  -- > data SimpleState = SimpleState
  -- >   { _myString :: String
  -- >   }
  -- > makeLenses ''SimpleState
  -- >
  -- > instance Default SimpleState where
  -- >   def = SimpleState "default"
  , makeStateLens
  , AppState

  -- * Advanced
  -- | This section provides tools which become relevant when working on more
  -- complex apps. You can customize which states you operate over, embed events
  -- in nested states, and choose a custom base monad for the mtl stack.

  , eve
  -- ** Actions
  , AppT
  , ActionT
  , runActionOver

  -- ** States
  , HasStates(..)
  , States
  , stateLens

  -- ** Local Events

  -- | The local versions of the event functions are the same as the others ('dispatchEvent',
  -- 'addListener', 'removeListener') however they operate on a per-state basis.
  -- This means that if you define a custom state which implements 'HasEvents'
  -- then you may use these functions inside an `Action CustomState` to dispatch events
  -- to ONLY the listners within that specific instance of that state. Note that
  -- these listeners and events are distinct on the value level, not just the type level,
  -- so if you have multiple copies of CustomState in your app, they each have their
  -- own disjoint event listeners.
  , HasEvents
  , dispatchLocalEvent
  , dispatchLocalEvent_

  , addLocalListener
  , addLocalListener_

  , removeLocalListener

  -- ** Async
  , asyncActionProvider

  , dispatchEventAsync
  , dispatchActionAsync
  ) where

import Eve.Internal.Run
import Eve.Internal.Actions
import Eve.Internal.Listeners
import Eve.Internal.Async
import Eve.Internal.AppState
import Eve.Internal.States
