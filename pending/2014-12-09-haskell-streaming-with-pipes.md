---
title: Haskell streaming with pipes
description: desc here
tags: haskell
date: [2014-12-09 Tue 13:42]
category: Haskell
---

This is an introduction to the concept of "effectful streaming" in Haskell,
why it is useful, and how to take advantage of it using the `pipes` library by
Gabriel Gonzalez.  It is not meant to replace the `pipes` tutorial, but to
serve as a precursor to it, introducing the concepts necessary to take full
advantage of the functionality found there.

In order to discuss what streaming is about, we must start by motivating its
use.  Let's do that by starting with some code that deceptively seems to have
nothing to do with streaming at all:

    fibs :: [Int]
    fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

This function `fibs` is a pure, total function that behaves as though it
returns an infinite list of integer values, representing the Fibonacci
sequence.

We can now use function composition to build another function that yields the
sum of the first thousand Fibonacci numbers:

    firstThousand :: Int
    firstThousand = sum . take 1000 . fibs

Despite how this function may appear, there no lists involved in the
computation of its answer.  Thanks to lazy evaluation, and optimizations
provided by the GHC compiler, the function `firstThousand` is compiled down to
something very close to what an imperative programmer might have written to
perform this same, running computation.  Furthermore, the memory overhead as a
result is extremely small, and does not increase no matter how many digits of
`fibs` we choose to sum.

The reason this function can make the intermediate list completely vanish, and
perform its computation in constant memory space, is a trick called fusion.

Imperative progra
