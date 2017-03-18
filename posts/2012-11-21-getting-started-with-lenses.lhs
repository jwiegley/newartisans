---
title: Getting Started with Lenses
category: Haskell
---

The following is the first in a series of articles I hope to write as a gentle
introduction to Edward Kmett's excellent
[lens](http://hackage.haskell.org/package/lens) library.

> import Control.Lens           -- HIDE
> import Control.Arrow

`Control.Lens` provides a composable way to access and modify sub-parts of
data structures (where by modify I mean: return a new copy with that part
changed).  In this introduction I won't be talking about the theory or laws
behind Lens -- both of which are worthy of study -- but rather how you can use
them to write simpler, more expressive code.

\## The mighty tuple

You've probably used tuples quite often by now, and may have discovered that
`(,) e` is a Functor, letting you do things like:

    [ghci]
    fmap (+1) (1,2)
      (1,3)

After which a very common question is: "How do I do that for the first
element?"  One way is with the Arrow library:

    [ghci]
    first (+1) (1,2)
      (2,2)

But this method does not generalize for *n*-tuples.  What about the 3rd element
of a 3-tuple?  Enter lenses.  Here's the lens equivalent of `fmap` and `first`:

    [ghci]
    over _1 (+1) (1,2)
      (2,2)
    over _2 (+1) (1,2)
      (1,3)
    over _3 (+1) (1,2,3)
      (1,2,4)

In this example, `_1` is a **lens**, which means it represents both a getter
and setter focused on a single element of a data structure.  In this case we
are using it in both senses, since we are first getting the value from the
tuple, applying `(+1)`, and then setting the result back to create a new
tuple.

You can also apply your function to both elements of a pair at the same time:

    [ghci]
    over both (+1) (3,4)
      (4,5)

There is also an infix operator notation for this:

    [ghci]
    both %~ (+1) $ (3,4)
      (4,5)

For the simple case of integer addition, you can use `+~ n` instead of `%~
(+n)`:

    [ghci]
    both +~ 1 $ (3,4)
      (4,5)

Most of the math operators are available in this form: `-~`, `*~`, `//~`,
`^~`, etc.  Or you can use `.~` to force both members to a specific value:

    [ghci]
    both .~ 9 $ (3,4)
      (9,9)

In the operator form it's fairly easy to chain applications, which makes it
easier to apply the same operator to the odd members of, say, a 5-tuple.  I'll
use a new function for this, `set`, which sets the element selected by the
lens.  Since the result is the new value, we can compose these:

    [ghci]
    set _1 9 . set _3 9 $ (1,1,1,1,1)
      (9,1,9,1,1)

Consider what that would look like without lenses:

    [ghci]
    let (a,b,c,d,e) = (1,1,1,1,1) in (9, b, 9, d, e)
      (9,1,9,1,1)

The main difference is that you have to capture and pass through all the
members, just to modify the few you're interested in.  But even more, the lens
version above will work on any tuple with 3 or more elements, while the
non-lens version is fixed to working with 5-tuples.

There are lots of these modifier operators for lenses, here's a selection:

Operator  Meaning
--------- ---------------------------
`.~`      Replace the focused element with some `x`
`?~`      Replace the focused element with `Just x`
`%~`      Apply a function to the element
`+~`      Add to it
`-~`      Subtract
`*~`      Multiple
`//~`     Divide
`^~`      Take the integral power
`^^~`     Take the fractional power
`**~`     Take an arbitrary power
`||~`     Logically or with a boolean
`&&~`     Logically and with a boolean
--------- ---------------------------

In the next post, we will look at how lenses interact with some of the other
basic functors: `Maybe`, `Either` and `List`.
