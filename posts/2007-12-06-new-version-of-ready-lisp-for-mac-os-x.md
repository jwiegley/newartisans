---
title: New version of Ready Lisp for Mac OS X
tags: lisp
---

There is a new version of Ready Lisp for Mac OS X available.  This version is based on SBCL 1.0.12.17, and requires OS X Leopard 10.5.  The most notable change from the previous version is that it is now fully universal, supporting PowerPC and 32- bit and 64-bit Intel machines.  Also, threading has been turned on for Intel processor.  See the NEWS below.

<!--more-->
What is Ready Lisp?  It's a binding together of several popular Lisp packages for OS X, including: Aquamacs, SBCL and SLIME.  Once downloaded, you'll have a single application bundle which you can double-click -- and find yourself in a fully configured Common Lisp REPL.  It's ideal for OS X users who want to try out Lisp with a minimum of hassle.  [The download](ftp://ftp.newartisans.com/pub/lisp/ReadyLisp-1.0.12-10.5.1.dmg) is approximately 87 megabytes.

There is a GnuPG signature for this file in the same directory; append `.asc` to the above filename to download it.  To install my public key onto your keyring, use this command:

    $ gpg --keyserver pgp.mit.edu --recv 0x824715A0

Once installed, you can verify the download using the following command:

    $ gpg --verify ReadyLisp-1.0.12-10.5.1.dmg.asc

Below is a full rundown of what's new.

## Now fully universal

Ready Lisp is now fully universal, and runs on the following platforms:

  * Intel 64-bit
  * Intel 32-bit
  * PowerPC 32-bit

There is no port of SBCL to 64-bit PowerPC.  Experimental threading has been enabled for both Intel platforms.

## Updated versions

The following pieces were updated:

  * SBCL, to version 1.0.12.17
  * SLIME, to CVS version 2007-12-06

Aquamacs remains at version 1.2a.

## Full Info documentation

Info documentation for the Common Lisp pieces is now bundled in.  Just type `C-h i` to read it.  Also, when editing Common Lisp files, you can type `C-h f` to instantly access the HyperSpec index.  In Emacs Lisp files, `C-h f` will get you help on Emacs Lisp functions.

There is also HTML and PDF versions of all documentation in:

  * `Ready Lisp.app/Contents/Resources/html`
  * `Ready Lisp.app/Contents/Resources/doc`

## More libraries

There are a few more Common Lisp libraries bundled in the core file with this release:

  * `CL-FAD`
  * `LOCAL-TIME`
  * `SERIES`
  * `MEMOIZE`
  * `CL-PPCRE`

I find these libraries very handy, but mainly I'm including them because the upcoming release of my CL-Ledger accounting tool depends on them, so it will work for Ready Lisp users out-of-the-box.  See the "doc" subdirectory above for documentation on how to use these libraries (except `MEMOIZE`, which does not have separate documentation; use `memoize:memoize-function` to mark a function as memoized).

