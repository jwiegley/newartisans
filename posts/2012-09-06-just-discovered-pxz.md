---
title: Just discovered pxz
description: desc here
tags: 
date: [2012-09-06 Thu 08:01]
category: Uncategorized
id: 359
---

To show just how significant parallelized algorithms can be, today I discovered [pxz](http://jnovy.fedorapeople.org/pxz/), a parallelized version of the `xz` compression utility, which I use constantly. The proof is in the numbers:

  --------------------------------------------------------------------------
  Command
  Before
  After
  Ratio
  Time
  -------------- -------------- -------------- -------------- --------------
  xz             pxz -9e
  2937M          2937M
  305M           281M
  0.104          0.096
  32m            4m(!)
  --------------------------------------------------------------------------

I put this alias in my `.zshrc`:

    alias tar='tar --use-compress-program=pxz'

Note that to build `pxz` on the Mac, I had to comment out a reference to `MAP_POPULATE`, which the OS X's `mmap` function doesn't support.

