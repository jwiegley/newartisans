---
title: CL HyperSpec Info pages in Emacs
description: desc here
tags: 
date: [2007-11-16 Fri 00:03]
category: Uncategorized
id: 178
---

I just discovered the [following blog article][] by Bill Clementson, from way back in 2003.  Luckily, the links still worked, so I was able to get Info pages today for the Common Lisp HyperSpec courtesy of the [GCL][] project.

Once installed, I found I could not easily lookup documentation for, say, `mapcar`, because it's actually on the page for `mapc`.  But [SLIME][]'s `hyperspec.el` contained the indexing info I needed to write a new module which fires up the Info system on the correct section for the symbol you want defined.

This new module is called `cl-info.el` and is available [from my Lisp repository][].  It rebinds the standard Emacs key for function help (`C-h f`) to lookup help in the HyperSpec instead, if you're in a `lisp-mode` buffer.

**NOTE**: A fellow Lisper pointed me to [this blog entry][] which offers a much nicer way to get the HyperSpec in Info form.  It's a little more work, but the quality of the result is superior and it has an index!  Also, it makes my `cl-info.el` unnecessary, by relying entirely on the Info system itself.

[following blog article]: http://bc.tech.coop/blog/031002.html
[GCL]: http://www.gnu.org/software/gcl/
[SLIME]: http://common-lisp.net/project/slime/
[from my Lisp repository]: ftp://ftp.newartisans.com/pub/emacs/cl-info.el
[this blog entry]: http://www.foldr.org/~michaelw/log/programming/lisp/dpans-texinfo-edition

