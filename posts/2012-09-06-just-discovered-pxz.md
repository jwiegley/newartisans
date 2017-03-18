---
title: Just discovered pxz
---

To show just how significant parallelized algorithms can be, today I
discovered [pxz](http://jnovy.fedorapeople.org/pxz/), a parallelized version
of the `xz` compression utility, which I use constantly. The proof is in the
numbers:

<!--more-->

  --------------------------------------------------------------------------
  Command        Before         After          Ratio          Time
  -------------- -------------- -------------- -------------- --------------
  xz             2937M          305M           0.104          32m
  pxz -9e        2937M          281M           0.096          4m(!)
  --------------------------------------------------------------------------

I put this alias in my `.zshrc`:

    alias tar='tar --use-compress-program=pxz'

Note that to build `pxz` on the Mac, I had to comment out a reference to
`MAP_POPULATE`, which the OS X's `mmap` function doesn't support.
