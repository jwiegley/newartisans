---
title: Demystifying the loeb function
description: desc here
tags: haskell
date: [2014-06-18 Wed 21:07]
category: Haskell
---

Recently I was reading some articles by Chris Done, Dan Piponi and others
about a mysterious, yet curiously useful function: the `loeb` function:

``` haskell
loeb :: Functor f => f (f a -> a) -> f a
```

The type of this function models a logical proposition by ___ Loeb, which
states ___.

Most explanations of one use of function involves an analogy about
spreadsheets, but this never clicked for me, until I realized that something
else I use often is actually a loeb function in disguise.  So I wanted to
share my alternate insight, in case it might offer clarify for others.

But first, the loeb function can be stated plainly as: Given a list of functor
algebras (i.e., evaluators), produce a list of pending evaluations.  These
pending evaluations, when forced, take that same list of pending evaulations
as input, and evaluate recursively until a result is produced.

There are some immediate consequences of this:

  1. Self-referencing elements (unless they are themselves lazy) will result
     in non-termination.

  2. If any evaluation terminates, the total set of evaluations performed will
     describe a directed, acyclic graph.

  3. Only those elements required to satisfy the evaulation of the requested
     element (and so on, recursively) are evaluated.

The place where I encounter this every day is when I use the Nix package
manager.  Consider:

Nix is a purely functional language that defines a system's package set as a
mapping of names to values, which describe each package.  Those values are
lazy (that is, left unevaluated until forced), and many of them are functions
which take as input that same, entire package set.

When you ask to install a package, Nix needs to know is what build actions to
perform, so it forces evaluation of the package's function to determine the
source URL, build scripts, etc.  That package almost always has dependencies,
which themselves are defined by attributes in that same, global package set.
Since these may need to be installed too, Nix forces their evaluation and
performs the same install step, recursively, until it works it way through the
dependency graph, and finally is able to install the top-level dependencies
that you initially asked for.

This process of dependency resolution: by evaluating lazy, pure functions from
a list of "pending evaluations" that each take as input the entire list, is
precisely the loeb function.

Another thing I've noticed about the loeb function is that unless
self-references are themselves lazy, the only interesting functors for loeb to
act on are those in which the type mapped by the Functor occurs more than
once, as with lists.  This means that for `Identity`, `Maybe`, the tuple
functor, `Reader`, `State`, etc., there is really not much interesting that
`loeb` can do.  For example:

``` haskell
main = print $ take 10 $
    runIdentity $ loeb (Identity ((1 :) . runIdentity))
```

This is just a fancy way of writing the `cycle` function.  The innermost
function, when evaluated, is passed an `Identity` value that contains itself,
so all it can do is lazily build some value that ties the knot.

You would think functions might provide an interesting `loeb`:

``` haskell
f_loeb :: (e -> (e -> a) -> a) -> e -> a
f_loeb f x = f x g where g y = f y g
```

But this is just `fix . flip`:

```
>>> :t fix . flip
fix . flip :: (a -> (a -> c) -> c) -> a -> c
```

What if we move to `Comonad`?

``` haskell
w_loeb :: Comonad w => w (w a -> a) -> w a
w_loeb w = fix (flip extend w . flip extract)
```

This seems to be not much more useful than plain loeb, except that now we're
forced to use something which "contains" at least a single evaluator:

``` haskell
import Control.Comonad
import Data.List.NonEmpty as NE

w_loeb (   (\xs -> NE.length xs)
        :| [ (\xs -> xs NE.!! 0)
           , (\xs -> xs NE.!! 1 + 3)
           ]
       ) -- prints: 3 :| [3,6]
```

## An ordering-independent parser

Another way that loeb could be used is to parse a language syntax where
declarations are ordering independent, such as Haskell.  Instead of parsing
each construct into its AST, parse the definitions into pending AST
reductions, and then use loeb to reduce it to ASTs.  This will give you "graph
reduction" on the declarations such that they occur in naturally sorted order
if possible.
