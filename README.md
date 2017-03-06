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
### [Building an App in Eve](https://github.com/ChrisPenner/eve/blob/master/docs/Building-An-App.md)

\^ That guide will bring you through the process of making your first app!

If you have any issues (and I'm sure there'll be a few; it's a new project!)
please report them [here](https://github.com/ChrisPenner/rasa/issues).

Core Principles
---------------

Eve's core principle is making it easy to build programs in a modular way.
There are two key concepts in Eve which you should be aware of:

- Events
- State

Eve provides many useful combinators for dispatching events and adding
listeners to events, events are a broad concept in Eve and can be triggered by
user-interaction, file-changes, even network sockets! Anything you can think of
really! Each time an event is fired, your app 'reacts' by running any
associated listeners on the given event. This is quite similar to other event
systems so far; however Eve does a few things differently. This is where the
'State' concept comes in. When writing an App, or an extension for an App, in
Eve, you can specify a state object which you'd like Eve to keep track of for
you, you can run monadic actions over this state and do whatever you want with
it. You can even expose your state-changing combinators to other extensions to
allow them to change the state too! Another nifty thing is that events can be
dispatched on different levels; so for instance in the Rasa text editor which
is built using Eve, there's the notion of 'Global Events' and 'Buffer Events'.
A 'Buffer' is a State object they've defined, and they added the `HasEvents`
typeclass to it, which now allows them to register listeners and dispatch
events to a specific instance of a buffer! Trackable states (and therefore
state-level event listeners) can be nested several levels deep without issue.
Unlike most event systems, Eve also allows Monoidal return values from event
listeners, so you can collect 'responses' from each event you fire if you wish.

When designing applications in Eve; it's crucial to think about how the
state of you application will be stored, and how different components interact.
Eve works best when components are separated and communicate with each-other through
events. This is because it allows those who will eventually write extensions to your
application to 'hook' into those events to add functionality.

There are some definite Pros and Cons to Eve's approach:

### Pros

-   Implementing most core functionality as extensions ensures a powerful and
    elegant extension interface.
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
