---
title: Journey into Haskell, Part 1
description: desc here
tags: fibonacci, journal, tutorial
date: [2009-03-16 Mon 17:12]
category: Uncategorized
id: 217
---

Having just begun my descent down the rabbit hole, I thought I'd try journaling about what I discover along the way, so that those who are merely curious can play the part of language voyeur.  I've always wanted to do that: to see how someone dives into Erlang or O'Caml or Forth -- or Haskell.  Here's your chance.

<!--more-->
This is day 5 of the Haskell experience, and I'm having quite a bit of fun so far.  It's definitely twisting my head into pretzel shapes.  I've spent hours getting less done than I could achieve with Python in moments.  The hope is that all this retraining will pay off further down the road.

## Fibonacci

My first attempt was a Fibonacci function, which I failed at miserably.  Turns out I was unable to conceive of "lazy recursion".  When I looked up the answer, it just seemed beautiful:

    fib = 1 : 1 : zipWith (+) fib (tail fib)

This function starts out the list with 1, followed by 1, then it starts adding two lists together -- provided by the same function before it's even done!  In imperative land this would blow the stack in a heartbeat, but in Haskell it makes sense.  The recursive call to `fib` returns `1, 1, 2, 3, 5` and the recursive call to `last fib` returns `1, 2, 3, 5`.  Add them together, and you get the sequence.

There is also the traditional definition, which matches what you find in math textbooks:

    fib 0 = 0
    fib 1 = 1
    fib n = fib (n-1) + fib (n-2)

If evaluated at the interactive prompt, this function will generate numbers forever, so you have to ask for just a few, like the first 20:

    take 20 fib

So, things began with my face on the ground, which was humbling, but also refreshing that such a simple problem could floor me so easily.

## Splitting strings

The next problem I tried to tackle was splitting a string into substrings at each colon.  That is:

    "Hello:World"
      => ["Hello", "World"]

Again, fail.  How shocking it was to spend over an hour on this and ultimately have to resort to Google.  The answer was pretty straightforward:

    splitAtColons :: String -> [String]
    splitAtColons = sac' []
        where sac' acc []       = [acc]
              sac' acc (':':xs) = acc : sac' [] xs
              sac' acc (x:xs)   = sac' (acc ++ [x]) xs

What I missed was using an accumulator to collect the current string.  I kept thinking it was something I had to return as I went along, not passed down to each deeper level -- and then returned after I'd added to it.  Here's the breakdown:

    splitAtColons :: String -> [String]

Defines the type of the function as something which takes a `String` and returns a list of `String`.

    splitAtColons = sac' []

This is essentially what I missed.  The definition of `splitAtColons` calls a sub-function, passing in an empty string (aka list) as the "accumulator".

        where sac' acc [] = [acc]

If `sac'` sees an empty string (`[]`) -- the end of the string currently being processed -- return the accumulated string in its own list.

              sac' acc (':':xs) = acc : sac' [] xs
              sac' acc (x:xs)   = sac' (acc ++ [x]) xs

Otherwise, take apart the current string into its first character, `x`, and the remainder, `xs`.  If that first character is a colon, return a list with the current accumulator as the head, and recurse to process the rest of the string (and so on).  Otherwise, add the non-colon character to the current accumulator, and recurse to process the rest of the string.

## First reactions

Moral of my first story: prepare to be humbled.  Google and IRC were a lifeline, and the people on [#haskell](irc://irc.freenode.net/haskell), both helpful and patient.  More soon.

