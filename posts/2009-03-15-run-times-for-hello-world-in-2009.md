---
title: Run times for Hello, World in 2009
---

Someone recently asked what my issue was regarding the JVM, since at the moment it prevents me from falling too much in love with [Clojure](http://clojure.org) -- a language with the double-benefits of functional programming, and Lisp syntax and macros.

Well, below is my reason.  These may not seem like much time in the scheme of things, but psychologically it builds up on me when I have to run a particular script over and over and over again.  I've already noticed the pain with Groovy.

| Language | Running time ||
-- | --------------- |
C | 0.00415675640106 |
C++ | 0.0043337225914 |
Haskell (compiled) | 0.00494946241379 |
Perl | 0.00773874521255 |
Ruby (1.8.7) | 0.00913717746735 |
Ruby (1.9.1-p0) | 0.0196997523308 |
Python | 0.0269904136658 |
ECL (Common Lisp) | 0.126332080364 |
Java (JDK6) | 0.146584188938 |
Haskell (interpreted) | 0.20009740591 |
Groovy (JDK6) | 1.07791568041 |

If you'd like to generate some of these timings for your own system, I have created a [Hello, world project on GitHub](http://github.com/jwiegley/helloworld).

