---
title: Journey into Haskell, part 6
---

Another thing to be learned down the Haskell rabbit-hole: Thinking in infinites.  Today someone posed a puzzle which I tried to solve in a straight-forward, recursive manner: Building a list of primes.  The requested algorithm was plain enough:

> Create a list of primes "as you go", considering a number prime if it can't be divided by any number already considered prime.

However, although my straightforward solution worked on discrete ranges, it couldn't yield a single prime when called on an infinite range -- something I'm completely unused to from other languages, except for some experience with the SERIES library in Common Lisp.

<!--more-->
## An incomplete solution

Looking similar to something I might have written in Lisp, I came up with this answer:

    primes = reverse . foldl fn []
        where fn acc n
                  | n `dividesBy` acc = acc
                  | otherwise         = (n:acc)
              dividesBy x (y:ys)
                  | y == 1         = False
                  | x `mod` y == 0 = True
                  | otherwise      = dividesBy x ys
              dividesBy x [] = False
 
But when I suggested this on [#haskell](irc://irc.freenode.net/haskell), someone pointed out that you can't reverse an infinite list.  That's when a light-bulb turned on: I hadn't learned to think in infinites yet.  Although my function worked fine for discrete ranges, like `[1..100]`, it crashed on `[1..]`.

So back to the drawing board, later to come up with this infinite-friendly version:

    primes :: [Int] -> [Int]
    primes = fn []
        where fn _ [] = []
              fn acc (y:ys)
                  | y `dividesBy` acc = fn acc ys
                  | otherwise         = y : fn (y:acc) ys
    
              dividesBy _ [] = False
              dividesBy x (y:ys)
                  | y == 1         = False
                  | x `mod` y == 0 = True
                  | otherwise      = dividesBy x ys

Here the accumulator grows for each prime found, but returns results in order whose value can be calculated as needed.  This time when I put `primes [1..]` into GHCi it printed out prime numbers immediately, but visibly slowed as the accumulator grew larger.

