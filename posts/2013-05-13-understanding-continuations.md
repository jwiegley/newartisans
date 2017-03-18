---
title: Understanding continuations
category: Haskell
---

In this tutorial I would like to talk all about continuations, to attempt to
demystify this rather simple concept somewhat.  For it's not so much that
continuations are difficult, but that the ways in which they're used can get
complex pretty fast.

In essence, a continuation is a function which represents the next block of
code to be executed.  Take this code for example:

``` haskell
main = do
    putStrLn "alpha"
    putStrLn "beta"
    putStrLn "gamma"
```

It may not look it, but this code uses continuations! In order to see them,
let's desugar that do-notation. You can click on the Run button to convince
yourself that this code has the same behavior:

``` haskell
main = putStrLn "alpha"
       >>= \_ ->
           putStrLn "beta"
           >>= \_ ->
               putStrLn "gamma"
```

As you can see, each expression, such as `putStrLn "alpha"` is bound to a
continuation function that "continues" the execution of main in an explicit
series. Thus, any language which allows for sequential statements uses
implicit continuations. It just happens that the continuation always means
"the rest of the code".

Let's give our continuation functions explicit names, just for fun:

``` haskell
main = putStrLn "alpha"            -- main
       >>= \_ ->                   -- k
           putStrLn "beta"
           >>= \_ ->               -- j
               putStrLn "gamma"
```

We can now read this code as follows: `main` evaluates the expression
`putStrLn "alpha"`, then takes the result of that evaluation and calls the
function `k`, which in turn calls the function `j`, which implicitly calls a
function we might call `exit`.

So far this isn't very useful. But what if we could truly name our
continuation function, and even call them whenever and wherever we wanted?

## Named continuations

In order to name continuations, we must operate within a special monad called
`Cont`. We'll actually use the transformer variety, called `ContT`, so that we
can do some IO at the same time. Here is our `main` function, moved into
`ContT`:

``` haskell
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont

main = flip runContT return $ do
    lift $ putStrLn "alpha"
    lift $ putStrLn "beta"          -- k
    lift $ putStrLn "gamma"         -- j
```

The `flip runContT return` prolog just means that we're entering the `ContT`
monad transformer, demarcating a kind of "scope" within which named
continuation functions may be called. Once we exit the `ContT` block, it is
not possible the invoke our named continuations again.

Within `ContT`, we use the function `callCC` ("call with current
continuation"), to name the "current" continuation: the continuation which now
follows immediately after the call to `callCC` itself:

``` haskell
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont

main = flip runContT return $ do
    lift $ putStrLn "alpha"
    callCC $ \k -> do
      k ()
    lift $ putStrLn "beta"          -- k
    lift $ putStrLn "gamma"         -- j
```

Nothing special happening here. We inject a call to `callCC`, capture the
current continuation as a function value, and then immediately call it -- just
as the code would have done itself had we not interfered. In fact, all we're
doing right now is making something explicit that was always there, hiding
behind the mask of do-notation.

Also note that in the code above, not calling `k` would have had the same
effect, since there is an implied call to `k` after the call to `callCC`. Our
making it explicit really had no effect at all. But things can get
progressively interesting from here, so let's cover each possibility in turn.

## Early exits

The first useful thing to do with a continuation is to call it from someplace
other than where it would get called ordinarily. For example:

``` haskell
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont

main = flip runContT return $ do
    lift $ putStrLn "alpha"
    callCC $ \k -> do
      k ()
      lift $ putStrLn "uh oh..."
    lift $ putStrLn "beta"          -- k
    lift $ putStrLn "gamma"         -- j
```

Can you guess what the output of this code will be before running it?

That's right! Calling `k` invokes the continuation, meaning that execution
moves to the block immediately after the `callCC` block, giving us a way to
terminate the `callCC` block early, as if the rest of the code it contained
didn't exist, in this case.

We can get even trickier when there is logic within the `callCC` block:

``` haskell
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont

main = flip runContT return $ do
    lift $ putStrLn "alpha"
    num <- callCC $ \k -> do
      if 42 == 7 * 6
          then k 42
          else lift $ putStrLn "uh oh..."
      return 43
    lift $ putStrLn "beta"          -- k
    lift $ putStrLn "gamma"         -- j
    lift $ print num                -- l
```

## Coming back again

But wait, there's more. I never said that the continuation function could only
be called once, or that it had to be called within the `callCC` block! Check
this out:

``` haskell
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont

main = flip runContT return $ do
    lift $ putStrLn "alpha"
    (k, num) <- callCC $ \k -> let f x = k (f, x)
                               in return (f, 0)
    lift $ putStrLn "beta"          -- k
    lift $ putStrLn "gamma"         -- j
    if num < 5
        then k (num + 1) >> return ()
        else lift $ print num       -- l
```

You may want to spend some time with this example, to get comfortable with
what's happening here. The lazily recursive magic inside the `callCC` block is
saying the following: We want to return from `callCC` a function which, when
called with a number, will invoke the current continuation and return that
same function along with the given number. So basically, this code packages up
the continuation in a nicely callable form.

As an exercise, try finding a way to hand back `k` directly, without wrapping
it up in the helper function `f`. You'll run into problems with infinitely
recursive types. But why is that? Read the definition of `f` more closely to
find your answer. (As a bonus: The trick we're using here is called "tying the
knot", and allows us to deal with just these sorts of recursive expressions).

## Conclusion

Let's cover what we've learned so far:

 1. Any code which represents sequential "statements" uses implied
    continuations.
 2. `callCC` within the `Cont` (or `ContT`) monad allows us to name these
    continuations.
 3. We can call a named continuation at any time to jump to that point in the
    code.
 4. We can invoke continuations as many times as we like, with different
    arguments.
 
Using only what we've learned so far, it should be possible to implement:

 - pretty much any iterative control construct from your favorite imperative
   language.
 - exception handling (*hint*: the "try" block is just a `callCC`, with the
   continuation pointing at the "catch" block following just after it, and
   "throw" is just calling the continuation function with an exception value.
   But how do you make the continuation function known to the code that does
   the throw?).
 - `goto`!  or `setjmp` and `longjmp`.
 - green threads (*hint*: when you "sleep" to transfer control to another
   thread, you are really invoking `callCC`, calling the continuation with a
   "not now" argument, returning that continuation function to the scheduler,
   which then later calls it with an "ok now" argument that allows the thread
   to resume executing).
 
Next up, delimited continuations, which let us get even fancier!
