---
title: Journey into Haskell, part 2
description: desc here
tags: Example, Monad, Schroedinger, tutorial
date: 2009-03-18 07:07
category: Uncategorized
id: 218
---

Everybody talks about Monads when they mention Haskell, so I got a bit ahead of myself and wanted to see something of what they're about.  No, don't worry, I'm not aspiring to yet another Monad tutorial.  I feel I have a ways to go before I'm ready to craft my own light-saber.

I did read about 10 Monad articles on the Web, and found myself more confused when I came out than when I went in.  Today's exercise took about 5-6 hours of pure frustration, before a kind soul on IRC finally set me straight.  It sure is difficult when getting past a single compiler error takes you *hours*.

<!--more-->
## That bedeviled cat

Most geeks know about Schrödinger's cat, the fated beast who, when put into a box with a random source tied to a deadly gas trigger, remains in a state of quantum superposition in which he's neither alive nor dead until someone opens the box to look.

Well, people kept saying that Monad are like "computational containers", so I wanted to model the following:

 1. There is a Schroedinger Monad into which you can put a Cat.
 2. When you create the Monad, it is Unopened, and the Cat's has no state.
 3. You also pass in a random generator from the outside world.  This involves another Monad, the IO Monad, because randomness relates to the "world outside".
 4. As long as you don't use the monad object, the Cat's is neither Dead nor Live.
 5. As soon as you peek into the box, or use it in any calculation, the Cat's fate is decided by a roll of the dice.

When I run the program ten times in a row, here's what I get:

    Opened (Live (Cat "Felix"))
    Opened Dead
    Opened Dead
    Opened Dead
    Opened Dead
    Opened (Live (Cat "Felix"))
    Opened Dead
    Opened Dead
    Opened (Live (Cat "Felix"))
    Opened (Live (Cat "Felix"))

Let's look at the code, and where I had troubles writing it.

## A flip of the coin

The first function flips a coin and returns True or False to represent Heads or Tails:

    import System.Random
    
    flipCoin :: StdGen -> Bool
    flipCoin gen = fst $ random gen

The sugar `fst $ random gen` is just shorthand for `fst (random gen)`.  There is no difference, I was just playing with syntax.  You do need to pass in a valid random generator, of type StdGen, for the function to work.

## Cats

    data Cat = Cat String deriving Show
    data Probable a = Dead | Live a deriving Show

These two types let me make Cats out of Strings, along with a Probable type which models a Live thing or a Dead thing.  It treats all Dead things as equal.  I can create a Live Cat with:

    felix = Live (Cat "Felix")

Following my "fun with syntax" up above, I could also have written:

    felix = Live $ Cat "Felix"

It doesn't matter which.  The `$` character is the same as space, but with much lower precedence so that parentheses aren't needed around the argument.  If there were no parens, it would look like I was calling `Live` with two separate arguments: `Cat` and `"Felix"`.

## Flipping a Cat

    flipCat :: StdGen -> a -> Probable a
    flipCat gen cat = if flipCoin gen 
                      then Live cat
                      else Dead

When I have a Cat, I can subject it to a coin toss in order to get back a Live Cat or a Dead one.  I should probably have called this function `randomGasTrigger`, but hey.

The type of the function says that it expects a random generator (for `flipCoin`), some thing, and returns a Probable instance of that thing.  The Probable means "can be Live or Dead", according to how I defined the type above.  The rest of the function is pretty clear, since it looks a lot like its imperative cousin would have.

## Bringing in Schroedinger

    data Schroedinger a
        = Opened (Probable a)
        | Unopened StdGen a deriving Show

This type declaration is more complicated.  It creates a Schroedinger type which has two data constructors: an Opened constructor which takes a Probable object -- that is, whose Live or Dead state is known -- and an Unopened constructor which takes a random generator, and an object without a particular state, such as a Cat.

Some values I could create with this type:

    felix   = Opened (Live (Cat "Felix")) -- lucky Felix
    poorGuy = Opened Dead                 -- DOA
    unknown = Unopened (mkStdGen 100) (Cat "Felix")

In the third case, the idea is that his fate will be determined by the random generator created with `mkStdGen 100`.  However, I want a *real* random source, so I'm going to get one from the environment later.

## Here comes the Monad

    instance Monad Schroedinger where
        Opened Dead >>= _ = Opened Dead
        Opened (Live a) >>= f = f a
        Unopened y x >>= f = Opened (flipCat y x) >>= f
        return x = Opened (Live x)

As complex as Monads sound on the Web, they are trivial to define.  Maybe it's a lot like binary code: nothing could be simpler than ones and zeroes, yet consider that *all* complexity expressable by computers, down to video, audio, programming languages, and reading this article, are contained within the possibilities of those two digits.  Yeah.  Monads are a little like that.

This useless Monad just illustrates how to define one, so let's cut it apart piece by piece.  By the way, I didn't author this thing, I just started it.  Much of its definition was completed by folks on IRC, who had to wipe the drool from my face toward the end.

    instance Monad Schroedinger where

Says that my Schroedinger type now participates in the joy and fun of Monads!  He can be discussed at parties with much auspiciousness.

        Opened Dead >>= _ = Opened Dead

The `>>=` operator is the "bind" function.  It happens when you bind a function to a Monad, which is like applying a function to it.  This line says that if you apply a function to an Opened box containing a Dead thing, what you'll get back is an Opened box with a Dead thing.

        Opened (Live a) >>= f = f a

If, however, you bind a function to an Opened box with a Live thing, it will apply the function to what's in the box -- in this case, the Cat itself.  The function `f` is assumed to return another instance of the Schroedinger type, most likely containing the same cat or some transformed version of it.

        Unopened y x >>= f = Opened (flipCat y x) >>= f

Here is the meat of this example, it's reason for being, all contained within this one line: If you bind a function to an Unopened box, it gets bound in turn to an Opened box containing a Cat whose fate has been decided by the dice.  That's all.  The reason I used a Monad to do this is to defer the cat's fate until someone actually looked inside the container.

        return x = Opened (Live x)

Lastly, if someone returns a cat from a box, assume its an Opened box with a Live Cat.  I don't honestly understand why this is necessary, but it seems Opened Dead cats are handled by the binding above, as shown by the output from my program.  I'll have to figure this part out soon...

## The main function

The last part of the example is the main routine:

    main = do
      gen ,- getStdGen
      print (do
              box ,- Unopened gen (Cat "Felix")
              -- The cat&#039;s fate is undecided
              return box)

This is fairly linear: it gets a random generator from the operating system, then creates an Unopened box and returns it, which gets printed.  `print` does its work by calling `show` on the Schroedinger type, since it was derived from `Show` earlier.

Something I still don&#039;t understand: at exactly which point does the flipping happen?  When `box` is returned?  When `show` gets called?  Or when `print` actually needs the value from `show` in order to pass it out to the IO subsystem?

## Closing thoughts

The full version of this code is [on my server](http://ftp.newartisans.com/pub/haskell/schroedinger3.hs).  There is also [a simpler version without Monads](http://ftp.newartisans.com/pub/haskell/schroedinger.hs).  I worked on the Monad version just to tweak my brain.  At least I can say I&#039;m closer to understanding them than when I started.

