---
title: Meta-programming with the Free Monad
tags: haskell
---

The Free Monad is something I've been having a great deal of difficulty wrapping my head around. It's one of those Haskell concepts that ends up being far simpler than any of the articles on the Net would have you think. So, here's a whirlwind tour of this Monad and how it can be super handy.

First, imagine you're building a robot to walk through a maze. The robot is programmed to go forward until it can't go forward anymore, and then check a set of instructions to learn if it should turn left, turn right, or shutdown. A possible data type to model such instructions could be:

``` {.sourceCode .literate .haskell}
data Directive = L | R | S
```

Here's what our processing function might look like:

``` {.sourceCode .literate .haskell}
instrs = [L, R, L, S]

interpret :: [Directive] -> IO ()
interpret = mapM_ process
  where process L = putStrLn "Going left"
        process R = putStrLn "Going right"
        process S = putStrLn "Saw shutdown, stopping"
```

And the output, as expected:

    ghci> interpret instrs
    Going left
    Going right
    Going left
    Saw shutdown, stopping

Easy as pie, right? But a lot of the simplicity here is because the example is simplistic. What if we want to vary the operations depending on hints from the caller? So let's trade a little bit of simplicity up front, for a lot more expressiveness (and a return to simplicity) further on down the road...

Enter the Free Monad
--------------------

The first step toward using the Free Monad is to make our `Directive` type recursive, and give it a Functor instance:

``` {.sourceCode .literate .haskell}
data FDirective next = FL next | FR next | FS
  deriving Functor
```

We will now chain directives together using the `Free` data type, from `Control.Monad.Free` (in the `free` package on Hackage). Here's what the Free data type looks like:

``` {.sourceCode .haskell}
data Free f r = Free (f (Free f r)) | Pure r
```

And our set of instructions encoded using it:

``` {.sourceCode .literate .haskell}
instrs2 = Free (FL (Free (FR (Free (FL (Free FS))))))
```

Pretty ugly, right? But it's easy to pattern match on this using a recursive function, giving us another interpreter for robotic instructions:

``` {.sourceCode .literate .haskell}
interpret' :: Free FDirective a -> IO ()
interpret' (Free (FL f)) = putStrLn "Going left"  >> interpret' f
interpret' (Free (FR f)) = putStrLn "Going right" >> interpret' f
interpret' (Free FS)     = putStrLn "Saw shutdown, stopping"
interpret' (Pure _)      = error "Improper termination"
```

Now, why go through all this mess rather than use a list? To gain the power of monads, almost for free. All we have to do is add a few more helper functions:

``` {.sourceCode .literate .haskell}
left     = liftF (FL ())
right    = liftF (FR ())
shutdown = liftF FS
```

And we get this:

``` {.sourceCode .literate .haskell}
instrs4 :: Free FDirective a
instrs4 = do
  left
  right
  left
  shutdown
```

Check to make sure the output is the same:

    ghci> interpret' instrs4
    Going left
    Going right
    Going left
    Saw shutdown, stopping

The new `runRobot` works! We've gone from a list that used brackets and commas, to a list that uses just newlines. But we've gained something along the way: we can now express logic directly in the robot's programming:

``` {.sourceCode .literate .haskell}
instrs5 :: Bool -> Free FDirective a
instrs5 goLeftAlways = do
  left
  if goLeftAlways
     then left
     else right
  left
  shutdown
```

And check again:

    ghci> interpret' (instrs5 True)
    Going left
    Going left
    Going left
    Saw shutdown, stopping

As the logic gets more complicated, it would be much harder to do -- and less optimal in many ways -- if we were still using lists to sequence instructions.

What the Free Monad therefore gives us is the ability to create imperative-style DSLs, for which we can write any number of different interpreters. Consider it another power tool in your meta-programming toolbox.

Another bonus is that the interpreter ignores any further instructions after the call to `shutdown`; we also get an error if the user forgets to `shutdown`. And all of this for free, just by using the Free Monad. (Although I still don't know what the adjective "Free" means in the term "Free Monad". It has something to do with mathematics, but that will just have to wait for another day).

