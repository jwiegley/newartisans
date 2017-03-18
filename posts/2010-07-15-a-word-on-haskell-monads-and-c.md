---
title: A word on Haskell Monads and C++
tags: c++, haskell
---

After spending a good while trying to understand monads in Haskell, and why the Haskell world is so fascinated by them, I finally understand why they aren't as exciting to other languages, or why they are completely missing from languages like C++: because they're mostly already there.

At its simplest, *a monad is an abstraction of a value which knows how to apply functions to that value, returning a new monad*.  In other words, it's a way to turn values into little packages that wrap additional functionality around that value.  Sounds a lot like what an object does...

<!--more-->
But this doesn't tell you what's exciting about them, from Haskell's point of view.  Another way of looking at them, without going into the wheres and whys, is this: In a lazily-evaluated, expression-based language, monads let you express sequenced, interdependent computation.

Consider the following two code examples.  First, in C++:

    #include 
    int main() {
      std::cout << "Hello, world!"
                << "  This is a sample"
                << " of using a monad in C++!"
                << std::endl;
      return 0;
    }

And the same code in Haskell:

    module Main where
    main :: IO ()
    main = do putStr "Hello, world!"
              putStr "  This is a sample"
              putStr " of using a monad in C++!"
              putStr "\n"

What the IO monad in the second example is doing is making the sequenced evaluation of the print statements possible using a nice, normal looking syntax.  The C++ code doesn&#039;t need monads to do this, because it already embodies the concept of abstracted values (here, the iostream passed between insertion operators) and sequenced computation (because it&#039;s not lazy).

To compare Monads with C++:

1. Monads are abstractions of values.  So are most C++ objects.

2. Monads permit functions to be applied to the "contained" value, returning a a new version of the monad.  C++ objects provide methods, where the mutated object is the new version.

3. Monads provide a way to encapsulate values in new monads.  C++ objects have constructors.

As another example, consider the case where you have to call five functions on an integer, each using the return value of the last:

    j(i(h(g(f(10))))

This is an identical operation in both Haskell and C++.  But what if the return value of each function wasn&#039;t an integer, but an "object" that could either be an integer, or an uninitialized value?  In most languages, there&#039;s either a type, or syntax, for this concept:

    C++      boost::optional
    C#       int?
    Java     Integer
    Haskell  Maybe Int

If each function returns one of these, but takes a real integer, it means we have to check the "null" status of each return value before calling the next function.  In C++ this leads to a fairly common idiom:

    if (boost::optional x1 = f(10))
      if (boost::optional x2 = g(*x1))
        if (boost::optional x3 = h(*x2))
          if (boost::optional x4 = i(*x3))
            j(*x4);

Note that not only are these calls sequential, but due to the meaning of optionality, they are also inherently short-circuiting.  If `f` returns `none`, none of the other functions get called.

Haskell can do this type of thing natively as well, and it looks similar:

    case f 10 of
      Nothing -> Nothing
      Just x1 -> 
        case g x1 of
          Nothing -> Nothing
          Just x2 -> 
            case h x2 of
              Nothing -> Nothing
              Just x3 -> 
                case i x3 of
                  Nothing -> Nothing
                  Just x4 -> j x4

But it's ugly as sin.  In C++, we can be evil and flatten things out using basic features of the language, assuming we pre-declare the variables:

    (   (x1 = f(10))
     && (x2 = g(*x1))
     && (x3 = h(*x2))
     && (x4 = i(*x3))
     && (x5 = j(*x4)), x5)

Or you can eliminate the use of temporaries altogether by creating a wrapper class:

    template  struct Maybe {
      boost::optional value;

      Maybe() {}
      Maybe(const T& t) : value(t) {}
      Maybe(const Maybe& m) : value(m.value) {}
    
      Maybe operator>>(boost::function,Maybe(const T&)> f) const {
        return value ? f(*value) : *this;
      }
    };

If we change our functions to return `Maybe` instead of just `boost::optional`, it allows us to write this:
   
    f(10) >> g >> h >> i >> j

Which in Haskell is written almost the same way:

    f 10 >>= g >>= h >>= i >>= j

But where Haskell needs Monads to make this type of thing reasonable and concise, C++ doesn't.  We get passing around of object state between function calls as part of the core language, and there are many different ways to express it.  However, if you confined C++ to function definitions and return statements only -- where all function arguments were pass-by-value -- then things like Monads would become an essential technique for passing knowledge between calls.

So it's not that you can't use Monads in C++, it's just that they require enough extra machinery, and aren't unique enough compared to core features of the language, that there isn't the same level of motivation for them as there is in Haskell, where they can really add to the expressiveness of code.

