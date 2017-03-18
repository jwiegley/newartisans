---
title: The JVM, and costs vs. benefits
category: Uncategorized
---

In a [recent entry](/2009/03/hello-haskell-goodbye-lisp.html) on differences between Haskell and Lisp, one of the Lisp community's long-time members, Daniel Weinreb, asked about my stated aversion to JVM-based languages for everyday computing (sometimes referred to as "scripting").  Specifically, it was asked in relation to Clojure, and why I hasn't been immediately taken by that language -- despite it's having so many features I respect and admire.

I wanted to respond to Daniel's question in a separate blog entry, since this topic has come up so often, it seems, and deserves thought.  The JVM is a rich, mature platform, and you get so much for free by designing new languages on top of it.  The point of debate is: what are the costs, and are they always worth the asking price?

<!--more-->
Daniel's question was:

> In your own case, you mention “tiny” and “fast-running” executables. I am not sure why “tiny” matters these days: disk space is very cheap, and the byte code used by the JVM is compact.  Common Lisp programs compiled with one of the major implementations, and programs written for the Java Virtual Machine, execute at very high speed.

> The fact that you distinguish between server-side and client-side applications suggests to me that what you’re really talking about is start-up latency: you’re saying that a very small program written for the JVM nevertheless has a significant fixed overhead that causes perceived latency to the user. Is that what you have in mind?[...]

> As a hypothetical question just to clarify your meaning: if there were a JVM implementation that started up instantly, so that the speed of execution of a small program would be the same as the speed of the same code appearing in the middle of a long-running server process, would that answer your objections?

Hi Daniel, thank you for your [in-depth reply](/2009/03/hello-haskell-goodbye-lisp.html#comment-325).  As always, I enjoy reading what you've contributed to the Net's compendium of thought on Lisp and related languages.

Your clarification was most accurate: When I said "scripting", I was talking about a context of usage, not a particular language paradigm.  I like that Haskell seems to be just as appropriate for tiny, throw-away scripts as it is for large, long-running programs.

When it comes to the latter, I really no have objections at all to the JVM or its startup time.  I'm more than willing to wait 5 minutes for something to execute, if it will run for months at high efficiency.  I face this situation all the time at work, where we have a huge EJB application hosted on JBoss.  It may complicate debugging sometimes, but the costs are worth the benefits.  The sheer number of things that J2EE and JBoss manage on our behalf, compared the small amount of code necessary to take advantage of them, is quite amazing.

What the JVM takes away, at least in 2009, is the choice of what those costs will be, and when I have to pay them.  I think one of C's biggest attractions for a long time has been that most of its costs are a conscious decision.  If you favor startup time, or a small memory footprint, or fast execution, you can pretty much decide.  This makes it as appropriate for embedded apps, as it is for running an HTTP server, as it is for building operating systems and compilers.  With Java, despite all the things you get for "free", it comes at the cost of other freedoms.  And sometimes, Java's priorities are not mine.

So while I can and do use the JVM for server-side computation, it's a bit heavy weight for small and simple tasks.  Common Lisp's answer to this problem was an ingenious one.  Instead of building programs that you run over and over, it offers an "environment" in which code is iteratively evaluated, so that you actually grow and nurture a burgeoning set of functionality within a long-running VM.  I like this model *when appropriate*, and enjoy it, for example, in Emacs, which I can leave running for days on end while at the same time extending its functionality by writing new functions and customizing variables.

To answer your query then: yes, if JVM startup time could be eliminated, it would "free my hand".  I very much respect the maturity and stability of the JVM libraries Groovy and Clojure have access to.  Also, what you said about the JIT, and alternative VMs, can be supplemented by mentioning all the other JVM facilities that exist, like code coverage, performance and memory analysis, and live introspection; along with the ability to pick JVMs to run on phones, or satisfy real-time computing requirements.  It's a rich platform, no doubt.

But why do we never see complaints about languages that link to the C++ standard library, or Boost, or any other of the large frameworks that exist?  Because in those worlds, you don't pay for what you don't use.  It's been a design philosophy behind C++ for years, and to good effect.  We might complain about the language, or its APIs, but you hardly notice if *other* projects use it, because largely, one can pretend it's not even there.  Not so with the JVM.  Every time I start a Java application on my system, I feel it.  Run several of them at once, and even my 3Gb laptop starts swapping.  Only with the JVM are such things a source of common complaint.

I'm hoping that some day, projects like the LLVM will start to abstract these two sides of development.  I want to be able to pick my language for its type safety, clarity, expressiveness, and joy of use; while at the same time I'd like to pick my VM for its security, footprint, handling of parallelism and messaging, and run-time appropriateness.  This would let me choose Lisp, Haskell, Python or C++, depending on the skillset of engineers available to me; and the JVM, .NET platform, or LLVM, depending on how I meant the code to be used.  Wouldn't that be a powerful set of tools at one's disposal?

