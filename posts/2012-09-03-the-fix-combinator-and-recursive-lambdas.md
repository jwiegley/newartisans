---
title: The fix combinator and recursive lambdas
tags: haskell
---

The `fix` combinator is a higher order function that can turn any function into a potentially recursive one. It can be a bit difficult to wrap your head around, since neither the definition nor the documentation is very clear on what you might use it for. However, I've found one handy use case: creating recursive lambdas.

Say you have a list of `Maybe` values and you're writing a `map` with a lambda expression to walk over them. For whatever reason, your algorithm turns `Nothing` into `Just 10`, and any other value into some complicated expression (useless example given here):

     import Data.Function (fix)
     
     foo :: [Maybe Int] -> [Maybe Int]
     foo = map $ \x ->
       case x of
         Nothing -> Just 10
         Just y  -> Just (length (replicate (y * 2) 'c'))

    ghci> print $ foo [Just 10, Nothing, Just 20]
      [Just 20,Just 10,Just 40]

Then you decide that instead of returning `Just 10` for the `Nothing` case, you want to return `Just 10` with the same logic applied as every other non-`Nothing` value. The typical way to do this would be to factor out the relevant code as a function, and call it twice:

     foo' :: [Maybe Int] -> [Maybe Int]
     foo' = map $ \x ->
       case x of
         Nothing -> Just (func 10)
         Just y  -> Just (func y)
       where func z = length (replicate (z * 2) 'c')

But there's another way: make your lambda recursive, and reuse your logic. It's like giving the lambda a "try again" feature:

     fooFix :: [Maybe Int] -> [Maybe Int]
     fooFix = map $ fix $ \f x ->
       case x of
         Nothing -> f (Just 10)
         Just y  -> Just (length (replicate (y * 2) 'c'))

What `fix` is doing here is that when the lambda function is called, it now receives a new first argument: *that same lambda, curried with itself as the first parameter*. That is, `fix (\f x -> f x)` is an infinite loop, doing nothing but calling itself over and over again with the same arguments. This is identical to the never-terminating `fix id`.

This use of `fix` is mainly a matter of flexibility, since you can always implement recursion in other ways, such as factoring out code into new functions. But there are times when it's simpler (and clearer, if the reader understands `fix`) to make the lambda itself recursive.

