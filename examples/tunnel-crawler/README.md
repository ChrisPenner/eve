# Building your first App with Eve

Glad you could join us! This will be a quick walkthrough of the steps involved
in building a simple app. After completing this tutorial you should feel
comfortable building something of your own! Eve is moving fast, so this guide
is likely to go out of date from time to time, if you notice that
something seems a bit off (or doesn't compile); feel free to open an issue
[here](https://github.com/ChrisPenner/eve/issues), or hop into our [gitter
chat](https://gitter.im/eve-framework/Lobby)!

If you haven't read through Eve's README yet (slacker! :wink:) Eve is a
framework for designing reactive event-driven applications, with a specialty
towards *extensibility*! I've always found examples and tutorials to be the
most helpful when learning to use a new library or technology, so I figured I'd
combine the two; so here's my tutorial on building an example app in Eve!

Being that Eve is an **Event-Driven** application framework I want us to build
something which is responsive to user input, and maybe even has some form of 
time-reliant aspect. Games tend to have these properties, and people are always
complaining that Haskell doesn't have any game frameworks, so lets make a dungeon-crawler!
I don't have all day though, so let's simplify it a bit and make it a 1-dimensional
'Tunnel-Crawler' instead!

Here's what we're shooting for, we want a little game where you can move your
little tunnel crawler back and forth in his tunnel and collect coins which
spawn over time. It's sure to be a AAA success!

First things first, let's set up our app! We'll use stack for this since it 
provides reproducible builds and is relatively easy to work with.

Creating a new Eve Project
--------------------------

If you're comfortable setting up a new project using stack you can probably go
ahead and skip down to the 'Providing Events' section. If you're unfamiliar
with Stack you can check out a decent guide
[HERE](http://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html).
Go ahead and install Stack now, I'll wait! (Psst! I'd recommend
`brew update && brew install haskell-stack`). Make sure you run `stack setup`
before we get started.

Once you've got that all figured, open the command line and go to a folder
where you'd like to keep your project and run `stack new tunnel-crawler`. This
creates a new project and sets up an application template for us! Go ahead and
`cd` your way in there and let's get started. First we'll need to specify that
we want to use the `eve` package; do this by adding `eve` to the
`build-depends` list under the `library` section of `tunnel-crawler.cabal`. We'll go ahead
and add the other dependencies we'll need for the tutorial while we're at it, so your
build-depends should now look like this:
```yaml
  build-depends:       base >= 4.7 && < 5
                     , eve
                     , mtl
                     , data-default
                     , lens
                     , random
```

Eve is a newish package so we'll probably also need to add it to our `stack.yaml`
file. Go ahead and change the line that says `extra-deps` to `extra-deps: ["eve-0.1.3"]`

With that stack should be able to find Eve! Open up `app/Main.hs` in your favourite
text-editor and change main into `main = gameLoop`; we're going to do most of our work in 
`Lib.hs` so this is all we'll need to change in here. Save and close `Main.hs` and
open up `Lib.hs`. Let's do a trivial `gameLoop` just to make sure we have our project
set up properly before moving on to more difficult things. You can delete the contents
of `Lib.hs` and replace it with this instead:

```haskell
module Lib where

gameLoop :: IO ()
gameLoop = print "Hi!"
```

This doesn't have anything to do with Eve yet, but should help us make sure our
project is set up right. At this point open a command terminal, cd into your
`tunnel-crawler` directory and run
`stack build && stack exec tunnel-crawler-exe` (you may wish to alias this to
something, we'll be doing it a lot).

Fingers crossed that it worked out for you, you may need to do some trouble-shooting
to make sure stack is working for you, but once you get that all sorted out we'll
get started on actually using Eve!

Providing Events
--------------------

There are two functions which can be used to run an eve app; namely:

- `eve :: (MonadIO m, Typeable m) => AppT AppState m () -> m AppState`
- `eve_ :: (MonadIO m, Typeable m) => AppT AppState m () -> m ()`

The only difference being that `eve` returns the final application state and
`eve_` does not, we don't need the final state so we'll go with `eve_`.
For those who care about type-signatures, you'll notice that the signatures
are polymorphic over m, which allows you to use any base monad you like with Eve,
however that's more advanced than we'll be getting with this tutorial.

You can see that `eve_` takes an `AppT AppState m ()` whatever that means!
Actually, for simple apps we can use the type alias `App ()` which just serves
to hide any complexity we don't care about. `App` is a monad (if you're not
familiar with monads that's okay; a bit of familiarity goes a long way, but you
should still be able to follow along); There are two types of 'commands' or
'actions' that we care about with Eve, `App`s and `Action`s. They're almost
identical except that `Action`s can be made to run over specific states whereas
`App`s can only be used over the application's global state. This will all make
sense in a second I swear!  Let's come back to that in a bit.

Okay! So, we saw that `eve_` needs an `App` in order to run, the basic idea is
that you pass `eve_` an `App` which describes your application's setup.
To start off let's just use the simplest setup we can think of:

```haskell
module Lib where

import Eve

setup :: App ()
setup = return ()

gameLoop :: IO ()
gameLoop = eve_ setup
```

Brilliant! Our players will have so much fun with this! Okay let's run it (with
`stack build && stack exec tunnel-crawler-exe`)

```
tunnel-crawler-exe: thread blocked indefinitely in an MVar operation
```

Uh-Oh! Something went wrong here; I'll save you the trouble and tell you that
it's because eve is an event driven application, but we haven't given it any
events to work with! Basically, the event-loop froze waiting for something to
happen. This is why every eve application needs to provide either an
`asyncEventProvider` or an `asyncActionProvider` in its initialization block;
these serve to provide your application with events in response to some
external stimuli, whether it be a key-press, network event, or file-system
change!

Let's add a little routine which provides keypress events!

```haskell
{-# LANGUAGE RankNTypes #-}
module Lib where

import Eve
import Control.Monad

data KeyPress = KeyPress Char

-- This provides KeyPress events to our app
keypressProvider :: Dispatcher -> IO ()
keypressProvider dispatcher = forever $ do
  c <- getChar 
  dispatcher $ KeyPress c

setup :: App ()
setup = do
  -- We register the keypressProvider as an event provider here.
  asyncEventProvider keypressProvider

gameLoop :: IO ()
gameLoop = eve_ setup
```

Okay, so we've added a `keypressProvider`, it has the type
`Dispatcher -> IO ()` which seems a bit strange, how does it work?
Dispatcher is a type alias provided by Eve which resolves to:

```haskell
type Dispatcher = forall event. event -> IO ()
```

It means that our 'provider' needs to be a function which accepts a `Dispatcher` as its
first argument, and then we can just call that `dispatcher` function any time we have a 
new event and it'll make sure it gets where it needs to go. In this case we use `forever`
to continually `getChar`s from stdin and dispatch a `KeyPress` event for each one!
Note that we added the `RankNTypes` language pragma at the top of the file, we'll need
that any time we use the `Dispatcher` type.

If you were to run the app now you'd notice that it no longer crashes, but no matter what
you type nothing happens! That's because we're dispatching events, but there's no-one listening!
Let's add an event-listener and make something happen.

Listening for Events
--------------------

```haskell
{-# LANGUAGE RankNTypes #-}
module Lib where

import Eve
import Control.Monad
import Control.Monad.Trans

data KeyPress = KeyPress Char

keypressProvider :: Dispatcher -> IO ()
keypressProvider dispatcher = forever $ do
  c <- getChar
  dispatcher (KeyPress c)

echo :: KeyPress -> App ()
echo (KeyPress c) = liftIO (print $ "You pressed: " ++ [c])

setup :: App ()
setup = do
  asyncEventProvider keypressProvider
  addListener_ echo

gameLoop :: IO ()
gameLoop = eve_ setup
```

So we added the `echo` listener. A listener is any function which takes some Event and returns an
`App` or `Action`. The `echo` listener is simple, it responds to keypresses by printing out which
key you pressed! Since we're just using `getChar` it'll only work properly on printable characters,
but we'll worry about that later.

Notice how we use `liftIO` from `Control.Monad.Trans` to run `print` inside an `App`.
This is because `App` is a Monad Transformer; see [mtl](http://hackage.haskell.org/package/mtl) for
more details if you care, but all you really need to know is that `liftIO` lets you run `IO` inside
an `App` or `Action`. Now that we've set up `echo` we need to register it so it listens for keypress
events. Since we have `KeyPress` in the function signature for echo Eve can actually infer which
event it's listening for and we can just use `addListener` on it. In this case we use `addListener_`
which discards the `ListenerId`; that's okay for now since we don't plan on using `removeListener` on
it later.

It's been a while since we ran anything, so let's give it another go! Hopefully
it compiled for you nicely, but as you type characters it doesn't seem to be
printing our message. What gives? Well it turns out that stdin is buffered by
newlines by default, so you'll have to hit your return key to see all the
responses. Normally we'd probably use a library which handles all of this
terminal wonkiness for us, but for the sake of simplicity we'll do it
ourselves. Go ahead and alter `setup` to include this magic formula which tells
our terminal we don't want it to buffer. Also add the new `System.IO` import at
the top of your file.

```haskell
import System.IO
setup :: App ()
setup = do
  liftIO $ do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
  asyncEventProvider keypressProvider
  addListener_ echo
```

Okay, so the app should be a bit more responsive now! Congrats, you've built
your first event-driven app! Let's keep moving!

Storing State
-------------

If this is going to be a game then we'll need to keep track of what's going on!
If our keypresses are going to move our tunnel-crawler character then we should
store their position somewhere. Eve's way of storing state can be a little 
unintuitive at first, but it starts to make sense as your application gets
larger, especially if you allow people to write plugins for it.

We'll take a simple approach in this case where all of our game state is stored
in a single 'GameState' object.

```haskell
import Data.Default
data GameState = GameState Int

instance Default GameState where
  def = GameState 0
```

As you can see our GameState is pretty simple, it consists of just a single int
which we'll use to track our player's position in the tunnel. We've also added
the `Data.Default` import and have defined an instance of `Default` for our
GameState which works pretty much exactly how you'd expect. States which we
store in Eve require a `Default` instance because of how they're accessed; if
you're dealing with a state where it's tough to come up with a good default you
can wrap your type in a `Maybe` and use `Nothing` as your default, then
initialize your state in the setup block when you have more information.

Now let's use our keypress events to change our new player position state!
You can go ahead and delete `echo` and we'll write a new keypress handler
called (unoriginally) `handleKeypress`

```haskell

-- This action runs over a GameState,
-- We'll see how to write this MUCH cleaner using
-- 'lenses' soon.
updatePos :: Char -> Action GameState ()
updatePos 'a' = modify dec
  where
    dec (GameState pos) = GameState $ pos - 1
updatePos 'd' = modify inc
  where
    inc (GameState pos) = GameState $ pos + 1
updatePos _ = return ()

handleKeypress :: KeyPress -> App ()
handleKeypress (KeyPress c) = do
  runAction $ updatePos c
  GameState pos <- runAction get
  liftApp . liftIO $ print pos
```

Then we'll go ahead and remove the old `addListener_` call and replace it
with: `addListener_ handleKeypress`. 

Now you might be noticing that this is looking a bit clunky, it's great that we
can define whole `Action`s over a given state, but what if we want to access
multiple different states all interleaved together? It gets a bit tedious to
continually use `runAction` over these things. This is where the [lens
library](https://hackage.haskell.org/package/lens) comes in. Eve cooperates
nicely with different lenses and lens combinators. Let's rewrite our `handleKeypress`
and `updatePos` using lenses to clean it up a bit.

It's been a while since we've showed the whole file, here's a checkpoint for you:

```haskell
{-# LANGUAGE RankNTypes #-}
-- Need TemplateHaskell so makeLenses can generate lenses for us.
{-# LANGUAGE TemplateHaskell #-}
module Lib where

import Eve
import System.IO

-- New import! We're using lenses now!
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.Default

data KeyPress = KeyPress Char

-- We've turned GameState into a totally legit Haskell Record now,
-- note that _pos' has an underscore prefix so that 'makeLenses' will
-- generate a pos' lens for us.
data GameState = GameState
  { _pos' :: Int
  }
-- This uses Template Haskell, it may be a bit confusing if you haven't seen it before,
-- it comes from the Lens library. All you need to know is that this generates
-- code for us which provides a lens: pos' :: Lens' GameState Int
makeLenses ''GameState

instance Default GameState where
  def = GameState 0

-- Now that we have the pos' lens from GameState to our int, we can
-- use Eve's 'makeStateLens' utility to generate a lens for us which will
-- work inside 'Action's and 'App's!
pos :: HasStates s => Lens' s Int
pos = makeStateLens pos'

handleKeypress :: KeyPress -> App ()
handleKeypress (KeyPress c) = do
  -- We've refactored this code to use lenses, look how much cleaner it is,
  -- Almost like we're programming imperatively ;)
  case c of
    'a' -> pos -= 1
    'd' -> pos += 1
    _ -> return ()
  position <- use pos
  liftApp . liftIO $ print position

keypressProvider :: Dispatcher -> IO ()
keypressProvider dispatcher = forever $ do
  c <- getChar
  dispatcher (KeyPress c)

setup :: App ()
setup = do
  liftIO $ do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
  asyncEventProvider keypressProvider
  addListener_ handleKeypress

gameLoop :: IO ()
gameLoop = eve_ setup
```

Rendering to Screen
-------------------

Okay! Wow, we've been through a lot! Feel free to take a breather and
experiment a bit before moving on. I hope you're convinced that the benefits of
using lenses is worth the trouble it can take to learn them. We can rely on
utilities that Eve provides to avoid their complexities for the most part.

Okay! So we're changing our player's position, but we can't actually see anything!
Let's sort that out now! Let's add a hook to our app so that it renders the game's
state to screen each time something happens. Let's trigger a `Render` event
**after** each event that occurs! We can add something to our setup block for this!

We'll add the new Render event type:

```haskell
data Render = Render
```

Then trigger it after each event using the handy `afterEvent_` listener hook
provided by Eve.

```haskell
setup :: App ()
setup = do
  ...
  afterEvent_ $ dispatchEvent_ Render
```

You'll notice that we're dispatching an event within an event listener here!
We could trigger rendering directly in the afterEvent hook, but for better semantics
we'll trigger a Render event specifically. Let's write a rendering function and
set it up to listen for the Render event.

```haskell
-- A simple tunnel drawing
tunnel :: String
tunnel = replicate 20 '.'

-- Replace the char at a position with a new char
-- We'll use this to draw our player's position 
-- in the tunnel.
replaceAt :: Char -> Int -> String -> String
replaceAt _ _ [] = []
replaceAt c 0 (_:xs) = c:xs
replaceAt c n (x:xs) = x:replaceAt c (n-1) xs

-- This clears the current line then draws the tunnel.
render :: App ()
render = do
  liftIO $ putChar '\r'
  n <- use pos
  liftIO . putStr . replaceAt '$' n $ tunnel
```

We'll also remove the old code that prints our position from handleKeypress as
it's just getting in the way now. Here's what it looks like:

```haskell
handleKeypress :: KeyPress -> App ()
handleKeypress (KeyPress c) = do
  case c of
    'a' -> pos -= 1
    'd' -> pos += 1
    _ -> return ()
```

Okay! So the rendering code is set up, `render` will clear the current line
printing `\r` is an old terminal trick which returns the print-head on the terminal
to the beginning of the current line. Now we just have to set it up as a listener
to the Render event!

```haskell
setup :: App ()
setup = do
  ...
  -- Here's a way to register functions as listeners which
  -- don't use the event that triggers them.
  addListener_ (const render :: Render -> App ())
```

I'm showing off a new way to register a listener here, you'll notice that
our `render` function doesn't actually accept a `Render` event, which
is how we usually tell the type system what sort of event it's expecting.
In this case we can use `const` to create a function which takes (and ignores)
one argument, then returns the `App` we defined. We annotate the type of
`const render` in-line so that GHC knows which type we expect it to take (Render)
and so that it gets registered for the proper events!

You can use this, or the other method, whichever you prefer. But remember the type
annotation if you do this new one! You'll get a strange type error without it!

Compile and run that, and we've got a working game! We can see the player move back
and forth by typing 'a' and 'd'. we can leave the tunnel off either end if we want,
let's just call that a feature for now :wink:

If you're content to start making apps, then go for it! Otherwise, the guide
continues on to talk about time-related events and how extensions can tie into
Eve apps.

Checkpoint! Here's roughly what your code should look like:

```haskell
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib where

import Eve
import System.IO
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.Default

data Render = Render
data KeyPress = KeyPress Char

data GameState = GameState
  { _pos' :: Int
  }

makeLenses ''GameState

instance Default GameState where
  def = GameState 0

pos :: HasStates s => Lens' s Int
pos = makeStateLens pos'

handleKeypress :: KeyPress -> App ()
handleKeypress (KeyPress c) = do
  case c of
    'a' -> pos -= 1
    'd' -> pos += 1
    _ -> return ()

keypressProvider :: Dispatcher -> IO ()
keypressProvider dispatcher = forever $ do
  c <- getChar
  dispatcher (KeyPress c)

-- A simple tunnel drawing
tunnel :: String
tunnel = replicate 20 '.'

-- Replace the char at a position with a new char
replaceAt :: Char -> Int -> String -> String
replaceAt _ _ [] = []
replaceAt c 0 (_:xs) = c:xs
replaceAt c n (x:xs) = x:replaceAt c (n-1) xs

render :: App ()
render = do
  liftIO $ putChar '\r'
  n <- use pos
  liftIO . putStr . replaceAt '$' n $ tunnel

setup :: App ()
setup = do
  liftIO $ do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
  asyncEventProvider keypressProvider
  addListener_ handleKeypress
  afterEvent_ $ dispatchEvent_ Render
  addListener_ (const render :: Render -> App ())

gameLoop :: IO ()
gameLoop = eve_ setup

```

Time-based Events
-----------------
All the good old unix games (like rogue) only reacted when the player moved,
but using our fancy modern laptops we can probably handle a game where things
happen in our game over time. How are we going to do that? You guessed it: Events!

We'll add a simple timer which ticks every few seconds and causes some treasure
to appear in the tunnel. We've learned all the tools we'll need for this
already actually. The timer will have to run asyncronously and it will trigger a 
`Timer` event each time it fires. Let's give it a go!

We'll add our new Timer event:

```haskell
-- new import!
import Control.Concurrent

-- new timer event!
data Timer = Timer

-- This dispatches a Timer event every 3 seconds
-- for some reason threadDelay takes microseconds :/
timer :: Dispatcher -> IO ()
timer dispatch = forever $ do
  threadDelay 3000000
  dispatch Timer

setup :: App ()
setup = do
  ...
  asyncEventProvider timer
```

Okay, so `timer` is the brains of this operation. It looks pretty similar
to our `keypressProvider`, basically we run an IO which dispatches events whenever
it wants. Well that was quick! We've got a timer event firing every 3 seconds, let's
do something with it! How about we spawn some treasure for the player to collect!

```haskell
-- new import!
import System.Random

-- Let's update our game state so we can track where the treasures are
-- We'll just use a list of positions which will represent which spots
-- have treasure
data GameState = GameState
  { _pos' :: Int
  , _treasures' :: [Int]
  }

-- We need to add this to our Default instance too.
instance Default GameState where
  def = GameState 0 []

treasures :: HasStates s => Lens' s [Int]
treasures = makeStateLens treasures'

-- This generates a random position within our tunnel then adds
-- it to the list of treasures we've defined.
spawnTreasure :: App ()
spawnTreasure = do
  -- Generate a random number
  newTreasure <- liftIO $ randomRIO (1, 20)
  -- '%=' from the lens library runs a function over
  -- the focus of the lens, in this case we 
  -- prepend the new treasure position.
  treasures %= (newTreasure:)

-- We need to update our render method too:
render :: App ()
render = do
  liftIO $ putChar '\r'
  n <- use pos
  t <- use treasures
  liftIO . putStr . replaceAt '$' n . addTreasures t $ tunnel
    where
      addTreasures t tunnel = foldr (replaceAt '%') tunnel t

setup :: App ()
setup = do
  ...
  -- spawn a treasure whenever the timer goes off.
  addListener_ (const spawnTreasure :: Timer -> App ())
```

Okay, let's try that, give it a try!
Hopefully you'll see your tunnel slowly fill with treasure!
But wait! Darn! We can't collect any of it. Let's take care of that.

```haskell
-- new import
import Data.List

collectTreasure :: App ()
collectTreasure = do
  p <- use pos
  t <- use treasures
  when (p `elem` t) (treasures %= delete p)
```

For now we'll wire that in after each time we move.

```haskell
handleKeypress :: KeyPress -> App ()
handleKeypress (KeyPress c) = do
  case c of
    'a' -> pos -= 1
    'd' -> pos += 1
    _ -> return ()
  collectTreasure
```

Done deal! We can collect that treasure! We'll probably want to keep track of how
much treasure we've collected, we'll do that in the next section as we learn how
extensions tend to work in Eve.

Keeping score with extensions
-----------------------------
...
