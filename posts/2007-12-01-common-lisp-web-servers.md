---
title: Common Lisp web servers
description: desc here
tags: Hunchentoot
date: 2007-12-01 17:49
category: Uncategorized
id: 177
---

Recently at work my manager asked me to create a server solution that was both, "Fun, and easy for me to get things running quickly in."  Well, to me that's a roundabout way of saying Common Lisp, so I started looking at possible solutions based on that platform.  The solution will run as a webserver in a potentially high-demand scenario, so I figured it would pay to compare the options available to me.

<!--more-->
Unfortunately, I did not have equal platforms on which to create these test results.  If this were my only task this week, I would have setup a dedicated Linux machine and created identical tests for each setup described below.  Because I do not have this luxury, some of the results may be due solely to where and how I ran them.  So please take these numbers with a huge grain of salt, since you may see very different results in your own environment.

First, I wanted to test Apache serving static files, to get a feel for what Lisp is up against.  On my MacBook Pro, running OS X 10.5.1 and Apache 2.2.6, Apache is able to serve a simple HTML file at roughly 4000 requests per sec.  This was determined by repeatedly running ApacheBench, using 1000 accesses and 10 concurrent threads.

Next I tried Hunchentoot, serving the exact same file.  I'm using SBCL 1.0.12.6 on Intel OS X (32-bit), with threading enabled.  Note that threading is not officially supported on this platform, and was something I had to explicitly enable during the build process.  What you get from MacPorts will not have threading enabled.  It could be this that led to some of the results I saw.

Anyway, Hunchentoot in this scenario serves between 250-400 pages per second.  Honestly, I'm not sure if that's good or bad for a pure-Common Lisp implementation, but I did find myself a bit disheartened.  Also, Hunchentoot on OS X does not handle worker thread exhaustion very well.  I'm certain that part of this is due to the preliminary thread support on OS X, but then again, the same tests has troubles on Linux as well (see below).  Using 1 concurrent thread in ApacheBench, Hunchentoot does fine.  At 5 threads it does fine most of the time.  At 10 threads, Hunchentoot has a tendency to drop requests, causing ApacheBench to stop working altogether.  I had to try several times in a row before I could get all 1000 page loads to complete.

Next I tried Portable AllegroServe with the same SBCL version.  I used `paserve` `1.2.47-vbz-0.1.4`, a version custom-fitted for use with SBCL.  The numbers here were a bit more promising: on average, 1000 pages/sec.  The real advantage, though, is that listener thread exhaustion only causes AllegroServe to wait until another thread is available.  I was able to run ApacheBench with 1000 concurrent connections and AllegroServe didn't bat an eye.

Since I don't have `mod_lisp` running on my OS X box, the next tests all used Linux virtual machines running under VMware Fusion.  I used 64-bit CentOS 5 (x86-64) for all tests.

The Apache baseline is much slower in this case, as expected.  Now Apache serves direct files at ~1200 pages/sec.  Hunchentoot running behind mod_lisp serves pages at around 190 pages/sec.  The nice thing, though, is that threading problems are non-existent.  What's strange, however, is that if I increase the number of concurrent access, performance actually goes up!  At 100 concurrent accesses (as opposed to 10), Hunchentoot behind mod_lisp is able to serve pages at around 330 pages/sec.  I have no idea why this would be the case.  Also, I'm not doing static file tests this time, but using the dynamically generated default page for Hunchentoot.

If I access Hunchentoot in this setting directly, it doesn't break off connections the way it does on OS X, but it does stall them.  As a result, with 10 concurrent accesses, it serves up data at around 30-50 pages/sec.  So using mod_lisp is definitely a huge win.  Also, at 100 concurrent accesses, I was able to send the SBCL process into a 100% CPU burn, from which it took over a minute to recover -- even after aborting ApacheBench.  At 5 concurrent access, there was less contention and it served data at around 200 pages/sec.

For the last scenario, I switched to another Lisp implementation altogether, this time to [Armed Bear Common Lisp][].  I figured I would try running a Java servlet under Tomcat, thereby leveraging the stability and great threading support I've come to expect from the Java VM.  Also, I can easily integrate with other Java code we have in house, which is sure to become a requirement if the project succeeds.  So I created another 64-bit CentOS 5 virtual machine and loaded Tomcat5 using [JPackage][].  I also built my own `tomcat-native` package, so I could have Tomcat running as fast as possible.  This setup used Sun's JDK5 JVM, Tomcat 5.5, and Armed Bear version 0.0.10.

Rendering a dynamic Lisp page, the Tomcat solution sees speeds on average of 330 pages/sec, in the same scenario where Hunchentoot behind mod_lisp offers 30-50 pages/sec (due to thread contention).  Also, concurrency with the Tomcat servlet is no problem at all; with 100 concurrent accesses, speed is simply not affected.  Even at 1000 concurrent accesses, nothing really changes.  This could be due to smart caching on Tomcat's part, or just to great thread handling.

I did not try AllegroServe on Linux, because I think running on the Java VM will be smart for other reasons not relating to Common Lisp.  I was a bit disheartened by Hunchentoot's performance and threading behavior, but at the same was quite pleasantly surprised by Armed Bear.  I do hope the author of ABCL continues to put effort into his project; and I'm crossing my fingers that basing code on a 0.0.10 release won't end up being a huge mistake.  If it does become a problem, though, I should be able to switch over to Groovy pretty quickly, without changing any of the framework at all.  It's just that using Common Lisp would make this task such a *joy*.

[Armed Bear Common Lisp]: http://armedbear.org/abcl.html
[JPackage]: http://jpackage.org

