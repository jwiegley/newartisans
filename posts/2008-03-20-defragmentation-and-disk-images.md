---
title: Defragmentation and disk images
description: desc here
tags: 
date: 2008-03-20 05:17
category: Uncategorized
id: 171
---

There has been a small debate among some Mac users about whether defragmenting your disks is necessary for the smooth operation of OS X.  I've always been in the camp of those who do it regularly, because I've seen what my disk ends up looking like after a few weeks of not doing it (and how pretty the graph looks afterwards).  It could all be psychological, but I find the progress bar rather hypnotizing, and my wife has been known to find me staring at it for hours on end.  Ok, very psychological.

Well, I've found one circumstance where defragmentation is definitively helpful: compressing sparse disk images.

<!--more-->
I have a habit of creating encrypted disk images for every client I work for.  A lot happens in those images, which tend to grow and shrink quite a bit.  OS X has a command to squeeze out the unused space, which looks like this:

    $ hdiutil compact DiskImage.sparseimage

I just ran this command on one of my work images, and it reported a savings of 786 Mb out of 3 Gb.  Just for interest's sake, I ran [iDefrag][] on the same volume, which eliminated the small amount of fragmentation that had built up, and compacted the files down toward the beginning of the image.

Running `hdiutil compact` on the same disk image again resulted in a further savings of 518 Mb!  Given that the image itself is now 1.4 Gb, that's about a 30% further reduction.

So, if you're like me and you use lots of virtual disk images -- and you've always wondered if defragmentation tools were worth anything at all -- here's one reason to consider it.

[iDefrag]: http://www.coriolis-systems.com/iDefrag.php

