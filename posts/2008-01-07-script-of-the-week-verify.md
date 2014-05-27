---
title: Script of the week: verify
description: desc here
tags: 
date: [2008-01-07 Mon 01:09]
category: Uncategorized
id: 173
---

This week's script uses Leopard's new `xattr` tool to store MD5 checksum information alongside any file you wish.  Later, you can run the same script to ensure that the checksum has not changed.

<!--more-->
For example, I run it on my Applications directory like this:

    find /Applications -print0 | sudo xargs -0 verify

The first time it's run, `verify` will print a lot of warnings about how none of the files currently have checksum information.  But the second time you run it, it silently verifies that none of the files have changed.  If there are changes, an error is printed to standard error, and an exit code of 1 is returned.  It's like a poor man's [Tripwire][], but no external databases or configuration files are necessary, since it stores the checksum data in an extended attribute called "checksum".  This means that if you later move the file (even to another HFS+ volume), the `verify` script will still verify the current contents against the initial checksum.

If the file has legitimately changed, you must first delete the old checksum:

    xattr -d checksum FILE

I recommend using this script on archival data, where changes are never expected.  I created this script because I recently discovered some data corruption in a set of ISO files, and found to my dismay that ISO offers no built-in scheme to determine which files had been corrupted and which hadn't, forcing me to throw out almost the entire lot.  Now I use `verify` to ensure that from this point on, the ISO never changes.

On a security note: This scheme offers no security, unlike systems like Tripwire.  A hacker who has permissions to change the file will also have permissions to reset the extended attribute containing the checksum.

Here's the bash script:

    #!/bin/bash
    
    # verify, version 1.0
    #   by John Wiegley 
    
    if [[ -z "$1" ]]; then
        echo "usage: verify [OPTIONS] "
        echo "options:"
        echo "  -v     be verbose about verification/checksum setting"
        echo "  -s     only set checksums for files which don't have them"
        echo "  -f     force setting the checksum even if file doesn't match"
        exit 1
    fi
    
    ATTRNAME=checksum
    
    verbose=false
    setonly=false
    force=false
    
    while [[ "${1:0:1}" == "-" ]]; do
        if [[ "$1" == "-v" ]]; then
            verbose=true
            shift 1
        elif [[ "$1" == "-s" ]]; then
            setonly=true
            shift 1
        elif [[ "$1" == "-f" ]]; then
            force=true
            shift 1
        else
            break
        fi
    done
    
    error=false
    
    for file in "$@"; do
        name=$(basename "$file")
        if [[ -f "$file" && "$name" != ".DS_Store" && \
              "$name" != ".localized" ]]; then
            CHKSUM=$(xattr -p $ATTRNAME "$file" 2> /dev/null)
            if [[ -z "$CHKSUM" ]]; then
                if [[ $verbose == true || $setonly == true ]]; then
                    echo "Note: No existing checksum for $file, setting..."
                fi
                CURSUM=$(md5 -q "$file")
                xattr -w $ATTRNAME $CURSUM "$file"
                CHKSUM=$CURSUM
            elif [[ $setonly == false ]]; then
                CURSUM=$(md5 -q "$file")
                if [[ $CURSUM != $CHKSUM ]]; then
                    echo "Error: Checksum mismatch for $file: $CURSUM != $CHKSUM" \
                        > /dev/stderr
                    error=true
                    if [[ $force == true ]]; then
                        xattr -w $ATTRNAME $CURSUM "$file"
                    fi
                elif [[ $verbose == true ]]; then
                    echo "Verified: $CHKSUM  $file"
                fi
            fi
        fi
    done
    
    [[ $error == true ]] && exit 1
    
    exit 0

[Tripwire]: http://sourceforge.net/projects/tripwire/

