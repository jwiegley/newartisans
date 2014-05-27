---
title: Run the Spotlight indexer at a lower priority
description: desc here
tags: 
date: [2008-04-09 Wed 01:25]
category: Uncategorized
id: 169
---

I realized the other day that on OS X, the Spotlight indexing process is started using `launchd`.  This makes it very easy to modify the `launchd` configuration script to insure that background indexing uses the least amount of CPU and I/O bandwidth possible.

Edit the configuration script by running this command as root:

    # open /System/Library/LaunchDaemons/com.apple.metadata.mds.plist

You should find yourself in the Property List Editor application.  Now add two keys at the top-level, one named **LowPriorityIO**, which is a boolean set to true, and another named **Nice** which should be an integer set to 20.

Now whenever the `mds` spawns `mdworker` processes to index recent changes to the file system, it won't get in your way quite as much as before.  (Without this change, `mdworker` processes run at the same priority as user processes, according to output from the `ps axl` command).

