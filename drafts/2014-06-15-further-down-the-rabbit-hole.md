---
title: Further down the rabbit hole of types
description: desc here
tags: haskell
date: [2014-06-15 Sun 21:53]
category: Haskell
---

In my last post I described the core types of a streaming I/O library I've
been developing around the idea of simple monadic folds.  To recap, here is
the core type I left off with:

``` haskell
type Source m a r =
    r -> (r -> a -> m (Either r r)) -> m (Either r r)
```

It turns out there is still some simplicity to be had here.  First, let's
rearrange the types to fit the form of a right fold function:

``` haskell
type Source m a r =
    (a -> r -> m (Either r r)) -> r -> m (Either r r)
```

This, as it turns out, is just `Cont`:

``` haskell
type Source m a r = Cont (r -> m (Either r r)) a
```

I also found after implementing many functions that requiring the caller to
fix the type of `r` does not gain much and makes the function types noisier.
So let's make it read it easier to read by using a newtype (so we can create
our own instances, among other things) and making it polymorphic over `r`:

``` haskell
newtype Source m a = Source
    { getSource :: forall r. Cont (r -> m (Either r r)) a) }
    deriving Functor
```

This, as it turns out, is all we need!  Plus, the standard instances become
trivial to implement now that we can rely on `Cont`:

``` haskell
deriving instance Functor (Source m)

instance Applicative (Source m) where
    pure = return
    (<*>) = ap

instance Monad (Source m) where
    return x = Source $ return x
    Source m >>= f = Source $ m >>= getSource . f
```

## Cont as a function algebra

It turns out that `Cont` has another trick up its sleeve.  Let's first define
a type to express F-algebras:

``` haskell
newtype Algebra f a = Algebra (f a -> a)
```

Using this type, we discover the following equivalence:

``` haskell
   Cont r a  ~  Algebra ((->) a) r
```

That is, a continuation of `r` over `a` is isomorphic to the function algebra
from `a` over `r`!  Unfortunately, there are few type class instances I can
think of for this algebra, because `r` ends up in both covariant and
contravariant positions.  However, it does clearly show that our sinks are
abstractions of catamorphisms (omitting `EitherT` for clarity):

``` haskell
  sink :: Sink a m b
~ sink :: Source m a -> m b
~ sink :: Cont (r -> m r) a -> m b
~ sink :: Algebra ((->) a) (r -> m r) -> m b
  sink alg = alg (\a -> \r -> return (r ++ [a])) $ []
```

The use of the algebra here may not be so clear, so here are the steps:

 - Evaluation requires a function that takes an `a` (since the functor is
   `((->) a)`)

 - That function must return another function, mapping each state to an action
   producing the next state.  (I close over `a` in that function, to combine
   the current value with the input state).

 - The result of evaluating the algebra is a function from some initial state
   to a monadic action yielding the final state.

I think the `Cont` type is more intuitive and useful for Haskell programmers
(since the last type variable is in positive position in the unfolded type,
allowing it to be a `Functor`, `Monad`, etc.).  On the other hand, the
`Algebra` type actually makes a bit more sense in terms of what is going on
when we reduce encoded fold functions to their result.

## Relationship with conduit

As I work on these types, I'm also pushing the boundary to see how much of
Michael's Snoyman's `conduit` library I can imitate within the limitations of
such a simple structure.  In turns out there a few things I simply cannot do:

  - Left-overs

  - Handling of exceptions on a per-element basis.  Handling them over the
    whole pipeline works fine.

  - Zip conduits and sinks, although Zip sources work fine.

I've tried a different attempts at implementing these, with varying degrees of
success.  For example, if we allow the sinks the run concurrently, Zip sinks
can be implemented using the `async` library, as long as any resource use is
synchronized.

Where I did have some success is at converting `conduit` Producers into
`simple-conduit` Sources.  It turns out there is a faithful, one-way
conversion:

``` haskell
import Conduit.Simple
import qualified Data.Conduit.Internal as C

adaptFrom :: forall m a. MonadBaseControl IO m
          => C.Producer m a -> Source m a
adaptFrom (C.ConduitM m) = source go
  where
    go :: r -> (r -> a -> EitherT r m r) -> EitherT r m r
    go z yield = f z m
      where
        f r (C.HaveOutput p c o) =
            yield r o >>= \r' -> f r' p `finally` lift c
        f r (C.NeedInput _ u)    = f r (u ())
        f r (C.Done ())          = return r
        f r (C.PipeM mp)         = lift mp >>= f r
        f r (C.Leftover p l)     = yield r l >>= flip f p
```

Converting in the other direction loses too much functionality to be useful,
and requires delimited continuations to work at all:

``` haskell
adaptTo :: MonadDelimitedCont p s m
        => Source m a -> C.Source m a
adaptTo src = C.ConduitM $ C.PipeM $ reset $ \p ->
    liftM C.Done $ unwrap $ runSource src () $ \() x ->
        lift $ shift p $ \k ->
            return $ C.HaveOutput (C.PipeM $ k (return ()))
                (return ()) x
  where
    unwrap k = either id id `liftM` runEitherT k
```
