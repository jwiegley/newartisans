I want to offer a different experience from Jon, so that people know that
Haskell can also be perfectly suitable for this sort of development.

I worked at FP Complete during the time they were building their Web IDE; I
also wrote an in-house billing system to support sales for that IDE. All of
this code, written by several people, used many different Haskell features:
transformer stacks, exceptions, networking, concurrency, databases, Template
Haskell, the C FFI, etc We didn't use many advanced, type-level features,
which may be one reason the experience was so much better. We also used
conduit pretty much everywhere from the beginning, which helped keep space
usage and performance constant.

Overall, Haskell was a great language to develop this application in. It
allowed a fairly small team to respond quickly to changing requirements, and
let us build feature-rich, performant code within a brief time table. Space
leaks were fairly rare, and I don't recall pulling my hair out to find them.
Also, our target was rather performance sensitive (especially with regard to
latency), so it wasn't that we were ignoring these issues, either.

We did learn a few things, though. First, keep the amount of "richness" to a
moderate level. Introducing new libraries and new language features often came
at a price that was only discovered much later. I remember tracking down a
latency bug for several days that ended up stemming from a recent use of the
"pretty-show" library to prettify debug strings, not realizing how slow it can
be if the values have a large amount of structure. Sticking with ordinary
"show" would have avoided the problem.

Type classes were often more trouble than they were worth. Transformer stacks
were bad, too, in terms of type errors, but you get used to them. You just
need to understand the semantics of how the levels interact very, very well.

Error handling was mostly resolved by using Either to reflect evaluation
failures (i.e., an input value that didn't make sense), and exceptions to
reflect execution failures (e.g., trying to read from a socket that was
suddenly closed).

The lack of standardization didn't bite so hard, because again, we weren't
using much beyond Haskell2010 and several convenience features (ViewPatterns,
OverloadedStrings, DeriveFunctor, etc). I'm not against the use of advanced
features, but they come at a cost that needs to be understood from prior
experience, rather than inflicted on a new project for the first time.

My worst bugs ever? They came in two flavors: crashes in C and C++ over the
FFI (typically a nightmare to debug); and cases where the type system was
unable to detect a semantics violation (for example, passing a function to
"hoist" that fit the type, but was not a monad morphism).

I also came to a gradual awareness that if you feel like you're pushing your
code uphill, you're doing too much work. Good Haskell has a certain lightness
of being to it, making it typically quite easy to debug and extend. Whenever
the code stopped having this feeling, almost invariably it was because I'd
added something that shouldn't have been there. I recall many pleasant
evenings reviewing code with Michael Snoyman, only to watch half the functions
and variables disappear because we were using too many "words" to say a simple
thing.

If you try to cram everything Haskell can do into one project, expect it to
fail brilliantly. But if you apply the right economy of thought, I think you
will find it quite rewarding.
