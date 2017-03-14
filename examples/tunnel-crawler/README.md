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
ahead and skip down to the 'Smallest Eve App' section. If you're unfamiliar
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
`build-depends` list under the `library` section of `tunnel-crawler.cabal`. Eve
is a newish package so we'll probably also need to add it to our `stack.yaml`
file. Go ahead and change the line that says `extra-deps` to `extra-deps: ["eve-0.1.2"]`

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

The Smallest Eve App
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

```
module Lib where

import Eve

gameLoop :: IO ()
gameLoop = eve_ $ return ()
```


