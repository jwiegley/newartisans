---
title: A case of reflection
tags: haskell
---

A while back, Edward Kmett wrote a library called `reflection`, based on
a [2004 paper](http://okmij.org/ftp/Haskell/tr-15-04.pdf) by Oleg Kiselyov and
Chung-chieh Shan that describes a neat trick for reifying data into types
(here the word "reify" can be understood as turning a value into something
that can be referenced at the type level). There was also
an
[article written by Austin Seipp](https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection) on
how to use the library, and some great answers
on
[reddit](https://www.reddit.com/r/haskell/comments/3hw90k/what_is_the_reflection_package_for/) and
[stackoverflow](https://stackoverflow.com/questions/17793466/black-magic-in-haskell-reflection) that
go into detail about how it works.

And yet, in all these years, though I've been on the lookout for a way to make
use of this library, I wasn't able to fit it into my workflow -- until today!
So let's look at my real world use for `reflection`, which solves a problem
that maybe others have encountered as well.

As you may know,
the [QuickCheck](https://hackage.haskell.org/package/QuickCheck) library
provides a facility for generating arbitrary data sets. The property testing
features of QuickCheck make use of this generation to search for test data
that might violate a set of properties.

However, the generation facility can also be used on its own, separate from
the testing components, to randomly generate data for any purpose. The library
for producing this random data offers lots of combinators, and is based around
instances for a type class called `Arbitrary`. Here's a basic example:

```
module Main where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

data Foo = Foo [Int] [String]
    deriving Show

instance Arbitrary Foo where
    arbitrary = do
        xs  <- listOf chooseAny
        len <- choose (1, 100)
        ys  <- vectorOf len (shuffle "Hello, world")
        return $ Foo xs ys

main :: IO ()
main = print =<< generate (arbitrary :: Gen Foo)
```

This creates a specifically shaped set of random data, where the list of
integers may be of any length, and any value, but the list of strings will
always be from 1 to 100 elements long, and the strings will only consist of
random arrangements of the characters found in `"Hello, world"`.

Now, what if you wanted to guide the generation process for `Foo` using
external information? Such as picking the length of the list of strings from a
value provided by the user? Since `Arbitrary` does not allow the use of
`Reader`, how do we get that user-supplied value into the `arbitrary` function
above? And without using global `IORef`s or `unsafePerformIO`?

The `reflection` library allows us to reify a runtime value into a type (whose
name we'll never know, requiring us to reference it through a type variable),
and then communicate that type via a constraint, such that we can reflect the
value back out as needed. If this sounds a bit confusing, maybe an example can
make it clearer:

```
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Proxy
import Data.Reflection
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import System.Environment

data Foo s = Foo [Int] [String]
    deriving Show

instance Reifies s Int => Arbitrary (Foo s) where
    arbitrary = do
        xs  <- listOf chooseAny
        len <- choose (1, reflect (Proxy :: Proxy s))
        ys  <- vectorOf len (shuffle "Hello, world")
        return $ Foo xs ys

main :: IO ()
main = do
    [len] <- getArgs
    reify (read len :: Int) $ \(Proxy :: Proxy s) ->
        print =<< generate (arbitrary :: Gen (Foo s))
```

There are a few additional things to note here:

 1. A phantom type variable has been added to `Foo`. This type variable
    associates the reified data to our type, so it can be reflected back out
    in the instance for this type.

 2. The `Arbitrary` instance for `Foo s` has incurred a new contraint, stating
    that the type represented by `s` somehow reifies an `Int`. How this
    happens is the magic of the `reflection` library, and uses a clever GHC
    trick representing Edward's unique twist on Oleg and Chung-chieh's work.
    This instance requires the `UndecidableInstances` extension.
    
 3. We now call `reify` with the data we want to pass along. This function
    takes a lambda whose first argument is a `Proxy s`, giving us a way to
    know which type variable to use in the type of the call to `arbitrary`.
    This requires the `ScopedTypeVariables` extension.

That's it: `reflection` gives us a way to plumb extra data into instances at
runtime, at the cost of adding a single phantom type.

If the phantom type seems excessive for one use case, or if adding the phantom
would effect a large family of types, then an alternative is to enable the
`FlexibleInstances` extension, and use Edward's `tagged` library to carry the
phantom instead:

```
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Proxy
import Data.Tagged
import Data.Reflection
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import System.Environment

data Foo = Foo [Int] [String]
    deriving Show

instance Reifies s Int => Arbitrary (Tagged s Foo) where
    arbitrary = fmap Tagged $ do
        xs  <- listOf chooseAny
        len <- choose (1, reflect (Proxy :: Proxy s))
        ys  <- vectorOf len (shuffle "Hello, world")
        return $ Foo xs ys

main :: IO ()
main = do
    [len] <- getArgs
    reify (read len :: Int) $ \(Proxy :: Proxy s) ->
        print . unTagged =<< generate (arbitrary :: Gen (Tagged s Foo))
```

This way we leave the original type alone -- which may be the only option if
you're generating arbitrary data for types from libraries. You'll just have to
wrap and unwrap the `Tagged` newtype wrapper as necessary.

Another benefit of using `Tagged` is that, because it can be wrapped and
unwrapped as necessary, it becomes possible to change the refied information
in cases where nested types are involved. In this last example, the user is
allowed to specify the value that should be supplied to the `Bar` constructor
during data generation.

```
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Proxy
import Data.Tagged
import Data.Reflection
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import System.Environment

newtype Bar = Bar Int
    deriving Show

data Foo = Foo [Bar] [String]
    deriving Show

instance Reifies s Int => Arbitrary (Tagged s Bar) where
    arbitrary = return $ Tagged $ Bar $ reflect (Proxy :: Proxy s)

instance Reifies s (Int, Int) => Arbitrary (Tagged s Foo) where
    arbitrary = fmap Tagged $ do
        let (len, bar) = reflect (Proxy :: Proxy s)
        xs <- listOf (reify bar $ \(Proxy :: Proxy r) ->
                          unTagged <$> (arbitrary :: Gen (Tagged r Bar)))
        l  <- choose (1, len)
        ys <- vectorOf l (shuffle "Hello, world")
        return $ Foo xs ys

main :: IO ()
main = do
    [len, barValue] <- getArgs
    reify (read len :: Int, read barValue :: Int) $ \(Proxy :: Proxy s) ->
        print . unTagged =<< generate (arbitrary :: Gen (Tagged s Foo))
```
