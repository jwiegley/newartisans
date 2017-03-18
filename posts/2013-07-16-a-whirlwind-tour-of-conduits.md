---
title: A whirlwind tour of conduits
tags: haskell
---

While talking with people on IRC, I've encountered enough confusion around
conduits to realize that people may not know just how simple they are.  For
example, if you know how to use generators in a language like Python, then you
know pretty much everything you need to know about conduits.

## The basics

Let's take a look at them step-by-step, and I hope you'll see just how easy
they are to use.  We're also going to look at them without type signatures
first, so that you get an idea of the usage patterns, and then we'll
investigate the types and see what they mean.

Everything in conduit begins with the `Source`, which `yield`s data as it is
demanded.  The dumbest possible form of source is an empty source:

``` haskell
empty = return ()
```

The next dumbest is a source that yields only a single value:

``` haskell
single = yield 1
```

In order to use any `Source`, I must ultimately connected it with a `Sink`.
`Sink`s are nothing more than code which `await`s values from a `Source`.
Let's look at an example in Python, where these concepts are features of the
language itself:

``` haskell
def my_generator():
    for i in range(1, 10):
        yield i

for j in my_generator():
    print j
```

Here we have a generator (aka Source): a function which simply yields values.
This generator is being passed to `for` statement that consumes the values
from it and binds them one by one to a variable `j`.  It then prints each
value after it is consumed.

The equivalent code using conduit employs a different syntax, but the general
"shape" of the code is the same:

``` haskell
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (whileJust_)
import Data.Conduit

myGenerator = forM_ [1..9] yield

main = myGenerator $$
           whileJust_ await $ \j -> 
               liftIO $ print j
```

I can make the code a little bit closer to Python's example (making the call
to `await` implicit) if I use `Data.Conduit.List`:

``` haskell
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (whileJust_)
import Data.Conduit
import qualified Data.Conduit.List as CL

myGenerator = forM_ [1..9] yield

main = myGenerator $$ 
           CL.mapM_ $ \j -> 
               liftIO $ print j
```

## Just regular code

Neither `Source`s nor `Sink`s have to be special functions, however.  They are
just regular code written in the `ConduitM` monad transformer:

``` haskell
import Data.Conduit
import Control.Monad.IO.Class (liftIO)

main = do
    (do yield 10
        yield 20
        yield 30)
        $$
        (do liftIO . print =<< await
            liftIO . print =<< await
            liftIO . print =<< await
            liftIO . print =<< await)
```

Each time `await` is called, it returns a value that was `yield`ed by the
source wrapped in `Just`, or it returns `Nothing` to indicate the source has
no more values to offer.

There, now you know the basics of the conduit library.

## Conduits

Between sources and sinks, there is a third kind of conduit, which is actually
called just `Conduit`.  A `Conduit` sits between sources and sinks, and is
able to call *both* `yield` and `await`, applying some kind of transformation
or filter to the data coming from the source, before it reaches the sink.  In
order to use a `Conduit`, you must fuse it to either a source or a sink,
creating a new source/sink which has the action of the `Conduit` bound to it.
For example:

``` haskell
import Data.Conduit
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (whileJust_)

main = do
    (do yield 10
        yield 20
        yield 30)
        $=
        (do whileJust_ await $ \x ->
                yield (x * 2))
        $$
        (do liftIO . print =<< await
            liftIO . print =<< await
            liftIO . print =<< await
            liftIO . print =<< await)
```

This example fuses a conduit that doubles the incoming values from the source
to its left.  We could equivalently have fused it with the sink to the right.
In most cases it doesn't matter whether you fuse to sources or to sinks; it
mainly comes into play when you are using such fusion to create building
blocks that will be used later.

## Use the types, Luke

Now that we have the functionality of conducts down, let's take a look at
their types so that any errors you may encounter are less confusing.

A source has the type `Source m Foo`, where `m` is the base monad and `Foo` is
the type of what you want to pass to `yield`.

A sink has the corresponding type `Sink m Foo a`, to indicate that `await`
returns values of type `Maybe Foo`, while the monadic operation of the sink
returns a value of type `a`.

A conduit between these two would have type `Conduit Foo m Foo`.

You're probably going to see the type `ConduitM` in your types errors too,
since the above three are all synonyms for it.  It's a more general type that
these three specialized types.  The correspondences are:

``` haskell
type Source m o    = ConduitM () o m ()
type Sink i m r    = ConduitM i Void m r
type Conduit i m o = ConduitM i o m ()
```

The `Void` you see in there is just enforcing the fact that sinks cannot call
`yield`.

## What's next?

Beyond this, most of the conduit library is a bunch of combinators to make
them more convenient to use.  In a lot of cases, you can reduce conduit code
down to something which is just as brief and succinct as what you might write
in languages with native support for such operations.  It's a testiment to
Haskell, rather, that it doesn't need to be a syntactic feature to be both
useful and concise.

And what about `pipes`, and the other competing libraries in this space?  In
many ways they are each equivalent to what I've described above.  If you want
to use `pipes`, just write `respond` and `request` instead of `yield` and
`await`, and you're pretty much good to go!  The operators for binding and
fusing are different too, but what they accomplish is likewise the same.

If you're interested in learning more about conduit and how to use it, check out
[the author's own tutorial](https://www.fpcomplete.com/school/advanced-haskell-1/conduit-overview).
