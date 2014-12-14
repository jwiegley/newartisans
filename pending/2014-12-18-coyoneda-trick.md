title: The Coyoneda Trick
description: desc here
tags: haskell
date: [2014-12-18 Thu 23:48]
category: Haskell

There are times when a thing which could act like a `Functor`, cannot.  For
example, in order to write the definition of `fmap` for `TVar`, it would
require use of the `STM` monad to implement, which is not possible:

    instance Functor TVar where
        fmap f var = readTVar var >>= newTVar var . f

This fails to check because instead of type `(a -> b) -> TVar a -> TVar b`, it
has type `(a -> b) -> TVar a -> STM (TVar b)`.
