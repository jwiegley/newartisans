---
title: "Script of the week: sizes"
---

For the next few months, I will have a "script of the week" each week: just some tiny little scripts I've developed over the years that I happen to find particularly useful.

Today's is a shell script called `sizes`.  It's a fairly simplistic interface to the `du` commands which just shows you all files and directories in the current directory that are larger than one megabyte.

<!--more-->
Here's an example of running it in my `/Library` directory:

    ~/Library $ sizes
    2.8M   Scripts
    3.9M   Components
    5.6M   Java
    6.0M   QuickTime
    6.1M   PreferencePanes
    7.6M   TexPackages
     12M   Keychains
     15M   Fonts
     20M   Preferences
     36M   Logs
     51M   Lisp
     55M   Emacs
     86M   Backups
    398M   Application Support
    1.7G   Caches

Here's the script:

    #!/bin/sh
    
    du -shx * .[a-zA-Z0-9_]* 2> /dev/null | \
        egrep '^ *[0-9.]*[MG]' | sort -n > /tmp/sizes.$$
    
    egrep '^ *[0-9.]*M' /tmp/sizes.$$
    egrep '^ *[0-9.]*G' /tmp/sizes.$$
    
    rm -f /tmp/sizes.$$

