---
title: Writing SELinux policies
category: Uncategorized
---

I started the adventure of writing an SELinux security policy from scratch today.  This is on CentOS 5, which uses the new policy modules approach rather than the older scheme involved a huge number of policies and a Makefile.`

It took hours of searching on the Net to find out that there's really nothing out there to teach you how to start a new policy from nothing.  I found one "step-by-step" guide, but it involved using a GUI tool that I don't have.  Every other article on writing policy is about using `audit2allow` to make existing policies more permissive.

The missing piece turned out to be the package `selinux-policy-devel`, which installs a tree of macro files in `/usr/share/selinux/devel`.  Going into there, I found a complete example policy!  So I copied these example files and started configuring them for Trac, the Python daemon I want to lock down.  After that, I just had to run make, and insert the new policy using "`semodule -i trac.pp`".  And now it looks like I'm in for a few days of tweaking, as I narrow down exactly what Trac does and doesn't need to be able to do.

