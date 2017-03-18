---
title: The groovy thing about Groovy
category: Uncategorized
---

One of my favorite languages this year is [Groovy](http://groovy.codehaus.org/), a scripting language for the Java VM.  It has a nice, clean syntax, in combination with some very powerful ideas, like it's brand of closures.  It can be close enough to Java as to be nearly indistinguishable -- which is good, if you're selling it to Java programmers; and close enough to Python that it seems like a first cousin.

<!--more-->
But none of these things is what gets me excited about Groovy.  Syntax is just syntax, after all.  One can always find a way to bury ugly syntax inside nicely named functions, so that the difference between "and" and "&&" doesn't mean much.  No, what I find groovy about Groovy has to do with the way it was developed.

You see, an enterprising programmer wanting to invent a new language has to consider three core areas before he can write meaningful programs:

 1. The syntax and semantics of the language itself.
 2. Its object module and memory model.
 3. Its runtime library and dynamic type facilities.

Without a doubt, area #1 is the sexiest of the three.  People will ooh and aah at your specialized ">>>" operator, but rarely do they care about generational garbage collectors versus mark-and-sweep.  Oh sure, you'll get kudos from the CS types for allowing runtime discrimination of types, but it's the static versus dynamic typing argument that rules the day.

Thus I find that many budding inventors place a great deal of emphasis on their syntax and semantics, and spend slightly less time on the other two areas.  After all, who really cares that Python uses a reference counting memory model, while Lisp uses a garbage collector?  I mean, there are people who really do care, but that isn't what all the heated language debates are about.

This is one of the reason's I'm not yet a big fan of Ruby.  True, it has a powerful, expressive syntax; maybe a bit on the arcane side, but if you have any Perl leanings, it's awfully tasty.  But what about the object model, the runtime, and the bytecode interpreter?  It turns out at least one person has had [serious issues](http://blog.cbcg.net/articles/2007/04/22/python-up-ruby-down-if-that-runtime-dont-work-then-its-bound-to-drizzown) with long-running Ruby processes; the 1.8 runtime doesn't handle native threading; and the core APIs would be "yet another thing I have to learn from scratch", without offering much functional difference from, say, Python.  Hasn't the world been forced to learn enough different ways of grabbing the length of a file?

But don't take this as Ruby bashing; I'm sure it's an insanely useful scripting language, probably much quicker at solving things than my current choice, Python.  It's just that I don't see as much attention being paid in the hype to its underpinnings as I do to how it looks on the outside.  And that, my friend, has everything to do with syntax, and little to do with its VM.

This is precisely where Groovy excites me.  The creators of Groovy decided not to reinvent a new runtime, but to take an existing, proven architecture -- the Java VM -- and base all their syntax and semantics decisions around it.  Imagine being able to start with a successful, proven runtime: in use at countless thousands of installations around the world, stressed by heavy-duty, multi-threaded, large scale processing, and then to sit back and say, "What would be a better syntax for this platform?"  This is a quite a bit different from starting the other way round: by saying, "What would a great syntax be for a new language?" and then leaving many of your object model decisions until after revision 1.0.

Groovy doesn't seek -- nay, doesn't even try -- to alter the runtime characteristics of the Java VM.  One of its design goals is to offer code that can be compiled into native Java bytecode, nearly indistinguishable from an equal class written in Java.  There is a performance penalty to be paid in terms of method indirection, and other support structures need by Groovy Meta-Object Protocol, but these exist within the existing Java framework, not as exceptions to it.  Once you compile your Groovy script to a `.class` file, it's interchangeable with any other Java `.class` file.  Make a `.jar` out of it, include the `groovy-all-1.0.jar` dependency, and anyone in the world can run it on their own Java VM.  Fully cross-platform on any operating system!!  And that's just in their 1.0 release.

This excites me because of how stable and solid the Java platform is.  It may be irrational at times, but I can really trust it.  It loads a bit too slowly for me to consider Groovy a *utility language*, but for heavier bits of code, it's perfect.  And what's even better: all that time you've spent mastering Java runtime APIs, guess what!  You don't have to throw away any of that knowledge.  You can start writing Groovy programs in the style of Java code right now, at this moment, without even knowing anything about Groovy except how to invoke it.  And then, gradually, you can start adopting the syntactic sugars that make it such a joyful language to code in.

This, to me, seems like an ideal way to approach a rational language syntax.  Only Lisp has impressed as much, with its sedate maturity and absolutely solid linguistic foundation (alas, it's runtime leaves one less than sated, especially if you work on Windows as well as Linux).  So if you work with Java at all -- or have to -- do give Groovy a look.  It's got sexiness, but it's the mature soul within who'll really win you over.

