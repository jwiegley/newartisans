---
title: "Script of the week: linkdups"
category: Uncategorized
---

It's been a while since I've posted a script; life has been distracting lately.  I also wanted to let this current script mature a lot more before sharing it, as it has the potential to be destructive.  Use wisely!

It's name is `linkdups`, and it's a Python program to recursively walk through a directory tree and hard-links any files together whose contents match exactly.  That means that if you have two files, each taking up 10 Kb, afterwards they will be linked to the same contents for a total savings of 10 Kb.

<!--more-->
*This type of space savings only works for media that never changes*.  It's great for use inside of archival disk images, website mirrors, backup directories, etc.

It seems that most programmers, at some point or another, writes a script to do exactly this kind of thing.  My brother was telling me the other day about one he'd written.

The main purpose of this script is to be extremely efficient.  I tested it against a directory hierarchy containing over 40 million entries, many of which required hard-linking.  My requirements were that memory consumption never grew beyond a small, startup footprint, and that it not waste cycles computing unnecessary details (like checksumming everything and then looking for matches).

Here's how the algorithm works:

1. First, I divide the problem between large files (easy gains), and small files.  The default cutoff is 16K.  This division means you get the biggest savings up front.  It also cuts down on memory usage for internal tables.

2. Next, a table is created of how big each file in the target set (large or small) is.  Groups are then made for like-size files; singletons are dropped.

3. Within the like-size groups, MD5 is used for a quick content check.  Files in a size group with matching checksums are considered candidates for linking.

4. The candidates from step 4 are byte-wise compared to ensure an exact match.  If any two match, the second one is removed and the first is hard-linked to the second's name.  A tally is then added of how many bytes were saved.  This is repeated for all other files in the group.  (Note: byte-wise comparisons are made only against the first member of a group, on the assumption that MD5 most likely guarantees a match).

5. At the end of the run -- or after Control-C has been pressed -- the total byte savings is displayed in human readable form.  Use the `--verbose` option to watch the algorithm do its work.

The script is also designed to be abortable.  You can hit Control-C any time if you get bored, and your files will be left in a good state.  It does as much linking as it can, but once Control-C is pressed, it stops wherever it is, wraps up the last task, and ends safely.

It can be downloaded from my FTP server: [linkdups.py](ftp://ftp.newartisans.com/pub/python/linkdups.py).

