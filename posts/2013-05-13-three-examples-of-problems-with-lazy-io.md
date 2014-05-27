---
title: Three examples of problems with Lazy I/O
description: desc here
tags: 
date: [2013-05-13 Mon 01:44]
category: Haskell
id: 428
---

### Problem 1: The source of exceptions is obscured

    main = getArgs >>= readFile . head >>= print . length

Even though `length` is a pure function, this is where the I/O will happen
(lazily), which means that is where any exceptions relating to I/O will get
raised.  Pure code should avoid raising exceptions, which this example violates.

### Problem 2: Sharing may cause file contents to remain in memory

    main = getArgs >>= readFile . head >>= print . (length . words &&& length)

Because of the way that lazy I/O reads in strings, this line of code will
cause the entire contents of the file to be loaded into memory by the call to
either `length` or `words`, and then it will stay in memory to be handled by
the other call to `length`.  You would expect it to process the input at the
very least one line at a time, to avoid exhausting memory on very large files.

**NOTE**: It has been pointed out that this is not really a problem with Lazy
I/O, but with laziness in general.  The only real way, then, in which an
iteratee-type library helps here is that it's more typical to connect sources
and sinks directly together, than to read all the data from a source at one
time, and then hand it to two sinks that way.  So the problem there is not
solved either, it's just less common to the idiom.

### Problem 3: File handles are not closed when you might expect

    main = getArgs >>= mapM_ (readFile >=> print . length)

If `getArgs` returns N files, Haskell will open N file handles, rather than
one at a time as you might expect, meaning that running this in a very large
directory may exhaust system resources.

### Conclusion: Use conduit/pipes/io-streams library to avoid surprises

Lazy I/O is great for prototypical simple examples, but for serious code these
problems can be hard to track down -- and are eliminated by a library such as
conduit.
