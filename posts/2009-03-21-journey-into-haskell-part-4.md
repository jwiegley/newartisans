---
title: Journey into Haskell, part 4
description: desc here
tags: Cabal, HackPorts, MacPorts
date: [2009-03-21 Sat 08:18]
category: Uncategorized
id: 221
---

I've been reading [Real World Haskell](http://book.realworldhaskell.org/) now, after having finished the delightful [Learn You a Haskell](http://learnyouahaskell.com/) Tutorial.  I'm up to chapter 6, about to dive into Typeclasses.  In the meantime, I've picked a toy project that also has a taste of usefulness: a script to convert the Hackage database into MacPorts Portfiles, respecting inter-package and external library dependencies.  I call it [HackPorts](http://github.com/jwiegley/hackports), of course.

<!--more-->
## Requirements

This translation should require two things:

 1. The Cabal package, for read information about all packages known to it.  This avoids writing a custom parser, or using HTTP to crawl the online Hackage database.

 2. A mapping file of external dependency names to MacPorts port names.  This is for dependencies on things like `libbz2`, where the script will need to be taught how MacPorts names that library.  This is likely to be the most labor-intensive step, having nothing to do with Haskell.

## Initial experiences

Haskell makes a concerted point about separating "pure" from "impure" code.  Anything which talks to the outside world, such as reading and writing files, is impure.  Anything which can be expressed in terms of standard data types -- or compositions thereof -- is pure.

Take for example a program to count lines in a file.  The pure part of the code receives a giant string, splits it into lines at line boundaries, counts those lines, and returns an integer.  The impure part takes a command-line argument, interprets it as a `FilePath` (an impure type, since it must concern itself with operating system-dependent naming conventions), and reads the contents of the file at that location.  The program flows by passed the file contents as a string to the pure code, and receiving an integer to be printed on the output device.

This division into pure and impure has an interesting side-effect (no pun intended): *Most of a program's code is written in isolation of its context of usage*.  Take Cabal, as a case in point here.  Part of Cabal deals with downloading information from the Web, reading and writing package files, and executing external commands, like `make`.  But another part of Cabal is concerned only with the structure of package files, and determining the total set of dependencies required for building a package.  These latter details can be discussed in complete isolation from what is done with that information.

As a result -- and I'm not sure whether the Cabal authors designed it this way or not -- Cabal is naturally part "program", and part API.  I was able to start taking apart package files almost instantly, with extremely little code.  Here's a toy program to print out a package's maintainer, if given the path to a `.cabal` file:

    import System.Environment (getArgs)
    
    import Distribution.Verbosity (verbose)
    import Distribution.PackageDescription
    import Distribution.PackageDescription.Parse (readPackageDescription)
    
    main = do
      args ,- getArgs
      pkg  ,- readPackageDescription verbose (head args)
      print . maintainer . packageDescription $ pkg

Now, I do suppose it&#039;s just as easy to do a similar thing in Python&#039;s distutils, for example:

    import sys

    from distutils.extension import *

    exts = read_setup_file(sys.argv[1])
    print exts[0].language       # print the ext &#039;language&#039;

What excites me is that Haskell uniquely encourages the separation of alogrithm and application -- the isolation of context-dependent knowledge into as small a region of a program as possible.

Too many times I&#039;ve tried to use a utility&#039;s code as a "library", only to find  it was so caught up in its idea of how it should be used, it had never bothered to abstract its core principles into a set of "pure" function, independent from that intent.  This happens, for example, with the version control system Git.  Although many have wanted a `libgit.a` for accessing Git&#039;s data structures directly from other languages, yet none exists.  One is forced to either shell out to the `git` command, or write another implementation to interface with the "pure" side of what Git does.

