---
title: Monads in Pictures
tags: haskell
---

This is not a tutoral on monads, nor will I use any math terms here.  This is
for people who have learned enough about monads to use them, but want to get a
better picture of what they're doing and why they exist.

## Functions

One way to develop a first intuition about monads is to follow the progression
of abstractions from functions to monads.  Here is a simple picture of what a
function does.  I've put Haskell syntax for calling the function on top, and a
graphical representation of the operation on the bottom:

![Function use](http://tech.wp.newartisans.com/files/2012/08/Function.png)

A function maps some value `a` to another value, here shown as `b`.  What happens
along the way between input and output is anybody's guess, although it's
usually some form of computation.  My own programming background led me to
view every function as requiring some kind of processing work, but that's only
one way to implement a function.  Functions, in the abstract, are simply a way
to go from one value to another.

## Functors

The next step up the abstraction ladder is the Functor.  Why do you need
functors?  Because sometimes you don't just want to go from `a` to `b`.
Instead, you know that `a` wraps (or contains, or provides) another value, and
what you really want is to apply your function to "the inner value of `a`".

Lists do this.  If you have a list of integers, there are times when you want
to apply a function to the integers in the list rather than to the list
itself.  So Lists are great candidate for being Functors, which they are.

To represent the "context" around the value you really want, I've used
brackets in the following diagrams.  But these brackets do not mean lists,
they just mean "context":

![Functor use](http://tech.wp.newartisans.com/files/2012/08/Functor.png)

Here we use the same function as before, only instead of mapping from `a` to
`b` directly with a function call, the function `fmap`:

1. "Unboxes" the value from the incoming Functor;

2. Calls `f` to turn that value to a new value;

3. "Boxes" the result in another Functor of the same shape and kind.

**NOTE**: Although I use physical metaphors here, like boxing, shape, etc., do
not be misled into thinking of Functors as always being like physical things.
It's possible for a Functor to itself be a function, in which case the
"context" models computation, rather than containership.  The best way of
thinking about a Functor depends entirely on how it's implemented.

## Monads

Believe it or not, Monads are just a simple tweak on Functors.  Browsing the
Internet might leave you thinking they are a highly specialized entity only
properly understood by mathematicians.  But the real truth is that if you grok
Functors, you're only one step away from comprehending Monads in all their
glory.

Up above we said that `fmap` does three things: it unboxes a Functor, applies
your function to the value that was in the box, and then boxes the result into
a new Functor of the same shape and kind.  This is the very soul of Functors.

Monads do almost exactly the same thing!  They just make one little change:
they don't re-box the result value.  Boxing the result still needs to happen,
but that job gets moved from the Monad to your function.

Here's a picture showing a Monad at work:

![Monad use](http://tech.wp.newartisans.com/files/2012/08/Monad.png)

What?!  This looks just like the Functor picture!  Only look closely: instead
of the grey application arrow going from `a` to `b`, it now goes from `a` to
the **context of `b`**.  Also, instead of calling `fmap f [a]`, we use an
infix function that swaps the arguments: `[a] >>= f`.

This is all that makes Monads special.  But what the picture doesn't tell you
is why they're awesome, and what the implications of such a change are.

Because our function now returns a new context, it can decide to change that
context, a service Functors cannot provide.  If I map a function over a
Functor, the result is always a new Functor of the same shape and kind.  But
if I bind a function over a Monad (note the difference in terminology), the
result can be a new Monad of the same kind but a *different* shape.  It's this
potentional for difference that provides the power of Monads.

Consider this chain of Monadic binds:

![Monad chain](http://tech.wp.newartisans.com/files/2012/08/Monad_chain.png)

At each step along this chain, context can change.  It can be used to carry
mutating state, a token stream, an auxiliary result value, an error code, etc.
All because the functions involved now participate directly in the binding
operation, by having job of boxing intermediate results in new monads.

This new responsibility can be a burden.  You can't bind a pure function over
a Monad that knows nothing about the Monad.  At the very least, you have to
call `liftM` on your function, to get a new function that does know about the
Monad.  There are times when all this lifting, and having to be conscious of
the "monadic context" can get wearisome.

But there you have it: Functions give you the ability to associate values;
Functors give you the ability to associate values within contexts; and Monads
let you carry that context through a sequence of binding operations.

## Arrows

Sometimes, the Monad abstraction keeps its context in the wrong place: around
the values passed between functions.  There are times when you'd rather have
context surround the *operation*, rather than the data.  This is what Arrows
provide.  Put simply, they add context to the concept of value mapping (i.e.,
the service that functions provide, although this is not the only way to map
values).  In fact, any function can be turned into an arrow with the `arr`
function.

Here is the function call from above again, this time upgraded to an Arrow
operation:

![Arrow use](http://tech.wp.newartisans.com/files/2012/08/Arrow.png)

Note the use of `run<Arrow>`.  Each arrow provides its own method for
executing it -- or it may not expose this functionality at all.  It's quite
possible for a library to provide completely opaque arrows, which only get
executed under controlled conditions.  Thus, the input and output types to an
arrow are all the user of an arrow needs to know about.  There could be all
kinds of other information there, including other functions that get called
when arrows of such type are composed.

So what can arrows be used for?  Any time you want context passed around with
your function.  Take, for example, a database query function you want to pass
to another function.  Ordinarily (and thanks to lazy evaluation), you'd just
invoke the query and pass the result, and the query would only happen if the
results were actually needed.  But what if the function needs to execute the
query repeatedly?  In that case, the callee must perform the query.

With regular functions, you'd need to pass both the query function and the
database handle for it to execute the query on.  Or you could use a reader
Monad, infecting the code performing the query with knowledge of that Monad.
What would be preferable would be to bundle that database handle with the
query, creating an enriched query function that knows itself which database to
talk to.  Enter the arrow.

Even more interesting use cases for arrows typically involve rich
compositions.  You can take one arrow with context, and another arrow with
context, and compose them in various ways to create a composed arrow with
composed context.  What that composition means depends entirely on the Arrow
type involved.

## Applicative Functors

As a bonus -- though it probably won't help you grok Monads any better -- I
want to mention Applicative Functors.

Applicatives upgrade our use of Functors in one special way: Whereas `fmap`
only accepts functions that go from one value to another, Applicative lets you
map functions that take any number of arguments over an equal number of
Applicative Functors:

![Applicative use](http://tech.wp.newartisans.com/files/2012/08/Applicative.png)

In this example, rather than applying an `f` that goes from `a` to `b` over a
Functor that provides an `a`, we get to apply an `f` that takes four arguments
over four separate Functors, all at once.

This isn't the fully story of Applicatives, by any means, but it is the crux.
The `Control.Applicative` module provides a lot of helper functions to help
take advantage of currying, composing and sequencing applicative applications,
similar to what you do with regular function applications.  The key intuition
is the ability to transform *any* function into a function that operates in
the realm of Functors, no matter how many arguments it takes.  Once you can do
that, your functions can be made to operate freely within the context of
Applicatives, without needing to knowing anything about that context.
