Eve
===

[![Join the chat at https://gitter.im/eve-framework/Lobby](https://badges.gitter.im/eve-framework/Lobby.svg)](https://gitter.im/eve-framework/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Hackage](https://img.shields.io/badge/hackage-latest-green.svg)](https://hackage.haskell.org/package/eve)

An extensible event-driven application framework in haskell for building embarassingly modular software.

Documentation
-------------
You can find hackage documentation for eve [HERE](https://hackage.haskell.org/package/eve)

Getting started
---------------
### [Building A Game in Eve](https://github.com/ChrisPenner/eve/blob/master/examples/tunnel-crawler/README.md)
[Here's](https://github.com/ChrisPenner/eve/blob/master/examples/tunnel-crawler/README.md) a guide which
walks you through building your first application in Eve from start to finish, it's quite thorough and it's a great
place to start!

If you have any issues (and I'm sure there'll be a few; it's a new project!)
please report them [here](https://github.com/ChrisPenner/eve/issues).

Core Principles
---------------

Eve's core principle is making it easy to build programs in a modular way.
There are two key concepts in Eve which you should be aware of:

- Events
- State

## Events
Eve provides many useful combinators for dispatching events and adding
listeners to events, events are a broad concept in Eve and can be triggered by
user-interaction, file-changes, even network sockets! Anything you can think of
really! Each time an event is fired, your app 'reacts' by running any
associated listeners on the given event. 

The functions you need to know are (with simplified types, see the real type in
the [hackage docs](https://hackage.haskell.org/package/eve/docs/Eve.html)):

- `dispatchEvent :: forall eventType result m. (Monad m, Monoid result) => eventType -> m result`
- `addListener :: forall eventType result m. (Monad m, Monoid result) (eventType -> m result) -> m ListenerId`

As I mention above, these types are simplified a bit (and yet they still look
complicated!). Actually, the types look so complex so that they're simpler to
use! The `forall` makes it so that you can call `dispatchEvent` with ANY
Typeable type and it will run the proper event listeners which were registered
by `addListener`; those listeners can alter app state, or even dispatch more
events! If the listeners return some (monoidal) value then the results from all
listeners are combined with `mappend` and are returned. That's pretty much it!

Here's a quick example for those who need to see some code:

```haskell
import Eve
import Data.Monoid

-- Define an event to listen for, in this case we don't even need any data alongside it.
data ComputeScore = ComputeScore

-- Define some computations which calculate some aspect of score.
-- We accept an argument of 'ComputeScore' to define what this is a listener for
scoreContributor1, scoreContributor2 :: ComputeScore -> App (Sum Int)
scoreContributor1 _ = do
  ... -- do some calculation over app state to determine one aspect of score
  return (Sum score)

scoreContributor2 _ = do
  ... -- Calculate some other aspect of the score
  return (Sum score)

-- In eve's initialization block we register the listeners, we could add these listeners anywhere
main :: IO ()
main = eve_ $ do
  ... -- other initialization (e.g. key listeners, etc.)
  addListener_ scoreContributor1
  addListener_ scoreContributor2

  -- This dispatches the triggering event and monoidally sums all the individual score components!
computeTotalScore :: App (Sum Int)
computeTotalScore = do
  Sum score <- dispatchEvent ComputeScore
  return score
```

## State

Next we see how Eve handles state. Eve seeks to be as extensible as possible so
it makes very few assumptions about the type of state that you (or your
extensions) plan to store. You can define a type of state yourself using `data`
and then provide actions which alter that state using a `MonadState` instance
(from mtl). Don't worry if you don't know what that means, here's a real quick
example which uses the combinators from the [lens
library](https://hackage.haskell.org/package/lens) to make a few simple state
changes.

```haskell
import Eve
import Control.Lens
data MyState = MyState
  { _myInt :: Int
  , _myString :: String
  }
makeLenses ''MyState

-- This alters some state and returns the old string for some reason.
doSomething :: Action MyState String
doSomething = do
  oldString <- use myString
  myString .= "Hi!"
  myInt += 1
  return oldString
```

So what does this gain us? Well now if we have a `MyState` somewhere in our app we
can run that Action on it! We can also register that Action as a listener for some
event!

Now for the interesting part; handling state for extensions. This is usually a bit
tricky since the types that an extension might use aren't known by you (the app author).
Eve takes care of this by providing an interface for extensions to store and keep track
of arbitrary types, while still allowing other extensions to run actions that it exports.
This is where the `HasStates` typeclass comes in; here's the honest to goodness implementation:

```haskell
class HasStates s  where
  states :: Lens' s States
```

If your state implements that typeclass, then extensions can store their own states inside it!
It's pretty easy to implement too, let's add it to our `MyState`.

```haskell
import Eve
import Control.Lens
data MyState = MyState
  { _myInt :: Int
  , _myString :: String
  , _myStates :: States
  }
makeLenses ''MyState

instance HasStates MyState where
    states = myStates
```

Done! We added a new field which has the type `States` which is exported by Eve.
Then we just took the **lens** created by `makeLenses` and used it in our instance.
That's it! Now extensions can store their own state inside `Action MyState` by
using the `stateLens`; check out the [hackage docs](https://hackage.haskell.org/package/eve/docs/Eve.html) 
on that for more info on how to do it!

Those are the basics, but you can do much more than that if you like!
Eve also lets you add listeners and dispatch events on an Object specific basis!
If you have a copy of some state (let's say a single instance of an Enemy in a game)
you can dispatch events over that enemy individually and any registered (Action Enemy)
callbacks will be run without affecting any other enemies! Check out `HasEvents`
to see how that works.

One last cool feature is that event listeners can return information! If your event
listener results in a return value that's a Monoid (like a list, or string for example)
you can collect the responses of all the listeners when you call `dispatchEvent`. This
is a great way for your application to 'ask' extensions about their state.

When designing applications in Eve; it's crucial to think about how the
state of you application will be stored, and how different components interact.
Eve works best when components are separated and communicate with each-other through
events. This is because it allows those who will eventually write extensions to your
application to 'hook' into those events to add functionality.

There are some definite Pros and Cons to Eve's approach:

### Pros

-   Implementing most core functionality using the event system your app remains extensible.
-   Flexibility & Adaptability; applications can be written in such a way that
    users can replace entire components with alternate versions.

### Cons

-   Module cross-dependencies makes the community infrastructure more fragile,
-   This architecture takes some getting used-to.

Contributing
============

Installation
------------

Eve uses Stack for reproducible builds.

1. Install [stack](http://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html)
3. Clone this repo and `cd` into the directory
4. Run `stack build`

Running Tests
-------------

- `stack test`

Contributions
-------------

Chatting about features is a key part of Eve's development; come join us in
the [Chat Room](https://gitter.im/eve-framework/Lobby) to discuss features or 
improvements!

Related Works
=============

- [Emulating traditional imperative event loops with functional paradigms](./paper/functional-event-loops.pdf)
