---
title: Script of the week: bzdmg
description: desc here
tags: 
date: [2007-11-05 Mon 03:04]
category: Uncategorized
id: 184
---

I haven't written much this past week because I've been upgrading all the home's machines to Leopard.  So far it's gone very smoothly, and I like the new OS!

The script for this week is about disk images.  Since version 10.4 of the operating system, OS X has had the ability to internally (and transparently) compress disk images using `bzip2`.  Probably because of compatibility issues with 10.3, I rarely ever see vendors compressing their disk images this way (I even see them using `gzip` on the image after it's made, which makes no sense at all since internal `gzip` compression has been supported for a long time!).  And so I wrote this script, which re-compresses disk image files using internal `bzip2` compression.  This can result in significant space savings over many images.  And if it's already been compressed with `bzip2`, the script reports this and changes nothing.

<!--more-->
Here it is, which I call `bzdmg`:

    #!/bin/bash
    
    if (( $# == 0 )); then
        echo "usage: bzdmg "
        exit 1
    fi
    
    for file in "$@"; do
        FORMAT=$(hdiutil imageinfo "$file" | grep ^Format:)
        if [[ "$FORMAT" == "Format: UDBZ" ]]; then
            echo $file is already compressed with bzip.
        else
            TEMP=/tmp/image-$$.dmg
            if hdiutil convert -format UDBZ -o $TEMP "$file"; then
                mv -f $TEMP "$file" 2> /dev/null
                if [[ -f $TEMP ]]; then
                    echo Error converting disk image file '$file'
                    rm -f $TEMP
                fi
            fi
        fi
    done

