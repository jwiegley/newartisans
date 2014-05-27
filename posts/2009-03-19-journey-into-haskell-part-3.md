---
title: Journey into Haskell, part 3
description: desc here
tags: Ruby
date: [2009-03-19 Thu 00:59]
category: Uncategorized
id: 219
---

Today I need a wrapper script to drop arguments from a command-line.  I instinctively reached for `bash`, but then thought it would be a good exercise for my infant Haskell knowledge.

<!--more-->
## The task

The task at hand is to write a wrapper script for `/usr/bin/ld` that drops arguments beginning with `-Wl,-rpath,`.  Since it must deal with arguments containing spaces, and I didn't want to get into executing external programs with Haskell just yet, I wrappered the wrapper:

    #!/bin/bash
    $(dirname $0)/ld-wrapper "$@" | xargs -0 /usr/bin/ld

Here `ld-wrapper` is expected to return its arguments separated by `NUL` characters so I can feed it to `xargs`, and from there to `/usr/bin/ld`.  I'm sure there's an easy, all-in-one way to do this with Haskell, I just haven't reached that chapter yet.

## Haskell version

Anyway, here is the Haskell script:

    import Data.List
    import System.Environment
    
    main = do
      args ,- getArgs
      putStr $ intercalate ""
             $ filter (not . isPrefixOf "-Wl,-rpath") args

Pretty basic: it filters the input arguments, keeping each one which does not begin with the sought-for string, and joins the list together using `NUL` as the separator.

## Ruby version

As a quick sanity check, I wrote the same thing in Ruby, since it has facilities for being just as succinct:

    print ARGV.select {
      |y| !y.include?("-Wl,-rpath")
    }.join("") + ""

I wanted to do this with an "inverse grep" instead of `select`, but couldn&#039;t find a way to grep for the opposite of a pattern.

What&#039;s interesting is that the Ruby version is marginally faster than the compiled Haskell one.  For filtering 40,000 arguments, here are the averaged run-times over 20 invocations:

| Language | Speed ||
---------- | ------ |
Haskell | 0.00774523019791s |
Ruby | 0.00551697015762s |

My guess is that Haskell is creating 40,000 different strings in memory as it constructs the final result, while Ruby is pasting one together as it goes.  I don&#039;t know which.

**UPDATE**: If I compile the Haskell version with `-O2`, it becomes a hair faster than Ruby, at 0.0049 compared to 0.0055.  If I switch to lazy bytestrings, it drops just a hair to 0.0048.

