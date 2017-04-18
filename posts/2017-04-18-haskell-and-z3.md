---
title: Submitting Haskell functions to Z3
tags: haskell
---

[Conal Elliott](http://conal.net/) has been working for several years now on
using categories, specifically cartesian closed category, as a way to abstract
Haskell functions at compile-time, so you can render the resulting
"categorical term" into other categories.

Here's an example Haskell function:

    \x -> f x (g x)
    
And here's its categorical rendering, just to give the flavor of the idea:

    eval ∘ (f' △ g')
    
Where `eval` means `uncurry ($)`, and `f'` and `g'` are the renderings of
those two functions; and the `△` operator is `(&&&)`. I'm not using the
typical Haskell names for these, by the way, in order to convince myself not
to "think in Haskell" when working with these terms, but rather I'm choosing
whatever symbols I find most often using in the literature on catgeory theory.

There are a few things to notice about these categorical terms:

  1. They must be point-free. There is no such thing as naming a term, only
     morphisms that use or produce objects. Hence Awodey calls category theory
     "the algebra of functions".
     
  2. They quickly become very large and unreadable. All but the simplest terms
     are nearly impossible to understand just by looking at them. Think of it
     as the binary code for categories.
     
  3. Because they are just, in effect, chains of composition, without any name
     binding or scoping issue to consider, the nature of the computation is
     laid out in a very direct (albeit verbose) way, making rewrite rules
     available throughout the abstract term.

Although it seems a bit technical at first, the idea is quite simple: Discern
the abstract, categorical meaning of a Haskell function, then realize that
term in any other category that is cartesian (has products) and closed (has
functions as objects, i.e., higher-order constructions). Nothing else needs to
be known about the target category for the abstract term to have meaning
there. That's the beauty of using category theory as a universal language for
expressing ideas: the meaning transports everywhere.

Here's an equation meant for the solver, written in plain Haskell:

    equation :: (Num a, Ord a) => a -> a -> Bool
    equation x y =
        x < y &&
        y < 100 &&
        0 <= x - 3 + 7 * y &&
        (x == y || y + 20 == x + 30)

Here's how I run the solver, using [z3cat](https://github.com/jwiegley/z3cat),
which is built on top of Conal's [concat](https://github.com/conal/concat)
library:

    mres <- liftIO $ runZ3 (ccc (uncurry (equation @Int))) $ do
        x <- mkFreshIntVar "x"
        y <- mkFreshIntVar "y"
        return $ PairE (PrimE x) (PrimE y)
    case mres of
        Nothing  -> error "No solution found."
        Just sol -> putStrLn $ "Solution: " ++ show sol

And the result, also showing the equation submitted to Z3:

    (let ((a!1 (ite (<= 0 (+ (- x!0 3) (* 7 y!1)))
                    (ite (= x!0 y!1) true (= (+ y!1 20) (+ x!0 30)))
                    false)))
      (ite (< x!0 y!1) (ite (< y!1 100) a!1 false) false))
    Solution: [-8,2]

Now with one function, I have either a predicate function I can use in
Haskell, or an input for Z3 to find arguments for which it is true!

Note that the typical approach of deeply embedding a DSL, and writing
evaluators, requires those evaluators to know the intimate details of both
semantic domains. What the categorical compilation approach does is to
introduce the *lingua franca* of category theory, so that both domains only
have to agree on a common abstract language. This means that every time you
write a new set of instances, you've automatically created an interpreter for
every input domain renderable into CCCs. It's like a toolbox whose power
multiplies with each new set of instances.

In addition to using Conal's work in Haskell, I'm also working on
a
[Coq rendering](https://github.com/jwiegley/category-theory/blob/master/Tools/Abstraction.v) of
his idea, which I hope will give me a more principled way to extract Coq
programs into Haskell, by way of their categorical representation.
