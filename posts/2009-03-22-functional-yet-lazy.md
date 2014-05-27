---
title: How laziness changes thinking in Haskell
description: desc here
tags: laziness, lazy
date: [2009-03-22 Sun 06:06]
category: Uncategorized
id: 222
---

As I explore Haskell, I'm discovering that one of its trickiest aspects is not structuring things functionally, but the lazy evaluation.  It turns out lazy evaluation comes with both great benefits, and significant difficulties.  I'd like to point a few of these out, as they're becoming clearer to me.

<!--more-->
## Benefits

One of the great benefits of lazy evaluation is that your code *doesn't need to account for the scale of an operation*.  Let's take a simple example: checksumming a large file whose contents are being read, on-demand, over HTTP.

In C++, if I wanted to checksum a large file read over HTTP, I couldn't buffer it memory because I don't know how large it might get.  Nor do I want my checksumming code to know anything about HTTP, or where the data comes from.  The answer here is to use I/O streams.  By passing a generic `istream` interface around, I can hide any knowledge of where the data came from.  The checksumming algorithm just reads data from the stream as it needs to, and the HTTP layer downloads more bytes as required (typically caching to avoid constant network access).

However, there's a downside to this: the checksumming code now knows something about I/O.  In actuality, a checksumming algorithm only cares about the bytes being checksummed and little else.  It shouldn't have to know about I/O, or strings, or any of the details of where data comes from or how it's structured.  It should ideally receive a pointer to an arbitrary large sequences of 8-bit bytes, and return a fixed size checksum representing a fingerprint of those bytes.

Yet this naive approach can't really be done in C++.  If it were a file I was checksumming, I could memory map the file and pass around a byte pointer, and the OS would take care of lazily reading in the bytes for me as needed.  But for a file being accessed over HTTP this would require first downloading the file and then checksumming it, when I specifically wanted to "checkum as I go".  Who knows, maybe I'll discover a reason to stop summing beyond a certain point and I'd like to stop downloading at that point as well.

Well, just as memory mapping gives me lazy access to the contents of a file, a language with lazy evaluation gives me lazy access to the results of any algorithm -- including downloading data over HTTP.  With Haskell, I can indeed write my checksum algorithm as if it receives a giant byte buffer, and the language takes care of downloading only as much data as I've accessed (plus caching).  This simplifies my checksumming code, and reduces the amount of knowledge that has to be passed around, such as a "stream" as opposed to a generic, 8-bit pointer.[^1]

This simplification lets you design your algorithm as if in an ideal world.  You want to process a bunch of numbers?  Work on a list.  What you say, the numbers are coming in from a socket and you don't know when it will end?  Doesn't matter, just work on a list.  In C++ I'd have to switch from passing a vector to passing an istream iterator, but in Haskell, I don't care what algorithm is populating my list, only that it *is* a list, and that I know how to work it.

[^1]: And if I do have to include state with this raw, lazy data, but I don't the algorithm to know anything about it?  That's where the Monad steps in.  Say instead of checksumming a file, I'm parsing an expression.  There are a lot of details that go along with parsing that have little to do with interpret the next bit of text, such as token position, error context, backtracking information, etc.  I want to be able to write a routine that parses a number very simply, without knowing about all those details.  It's the Monad that manages this extra information.  You can [read more here](http://en.wikibooks.org/wiki/Haskell/Practical_monads#Parsing_monads).

## Detriments

For all its beauty, laziness has three costs I've run into so far.  The first is that it lets you very easily write functioning algorithms with horrible performance characteristics.  This happens because laziness causes a promise[^2] to be constructed, which takes memory and time to do.  Sometimes, the cost of the underlying operation is far less than the memory cost of carrying a promise around to do that operation at a later point.  This isn't true of a slow operation like reading from a socket, but it's certainly true of something trivial like summing two integers.  It means one has to be aware of promises, when they're constructed, and when it's more beneficial to force evaluation always versus the benefits to be had from deferred a computation whose result may never be needed.

The second is that when a poorly performing algorithm dies, it dies when its value is used, not when the promises are made.  This can make it look like the consumer is to blame, when really it's the producer.  Here is a trivial example:

    mysum = foldl (+) 0
 
    main = print (mysum [1..1000000])

Although `foldl` is tail-recursive, so we aren't blowing stack through recursive calls, it still blows stack because it builds up a huge, nested structure of promises that only gets evaluated once print is called to render it as a string.  That is, the return value from mysum itself is no problem, it's just a lazy computation against a large list.  But then print needs the result, so it asks mysum to fulfill its promise.  This in turns causes mysum to churn through the large list of integers, building up the return value as it goes.

However, and here is where the surprise comes in: foldl doesn't actually compute those values as it walks the input list.  No, even these are done lazily, because it can't know how many of those values will actually be needed.  We may know from looking at the code that it will need them all, but it doesn't know.  So it constructs something on the stack looking like this:

    ((((((((0+1)+2)+3)+4)+5)+6)+7)+...)

And so on, all the way to the last integer.  Only when `mysum` is done constructing promises across the entire input list, and the promise structure is returned, will it actually get evaluated by summing the integers together and finalizing each promise.  If you pick a input list large enough, there goes available memory.

The trick here is that the stack fault won't ocur in foldl, or in mysum.  It will occur in print, where the need to resolve the promise result in the call to mysum actually being made, which then calls foldl, which then starts building thunks until memory is gone.  In this trivial example there's very little code or time distance between the problem and its cause, but in real world code there may be enormous gaps between them.

In consequence of this I learned that it's [hard to get GHC to produce stack traces](http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci-debugger.html#tracing) for you when there's a runtime error.  Your code can be going on its merry way, when suddenly there's a stack fault.  But that's all you see: a stack fault indicating something went wrong.  Where did it go wrong?  Based on the behavior of the program, I'm led to believe it happened near what the code was actually *doing*[^3] -- but in fact the problem may have started long, long before, except that laziness differed the trigger to a later time.

So, even though laziness can delay costs and abstract how data is determined, by the same taken it also delays errors and abstracts blame.  In C++ if I pass in an I/O stream and there's a crash reading from it, I know to look at my stream code.  But in Haskell if I get a stack fault simply by processing a list, how am I to know what's wrong?  It's not going to be in the List code, and probably not in the code walking the list, but in code which promised to produce the list potentially a long time ago.

I still think the benefits can outweight the difficulties -- especially when it comes to parallelism, and avoiding unnecessary computations, and allowing code to safely traverse infinite series -- but it definitely requires a level of algorithmic conciousness on the part of the engineer which seems quite a bit higher than with imperative languages.

[^2]: Promises are what get turned into real values when data is finally needed.

[^3]: If you use profiling libraries along with `-prof -auto-all`, you can get a much clearer picture of what was executing at the time of the fault.

