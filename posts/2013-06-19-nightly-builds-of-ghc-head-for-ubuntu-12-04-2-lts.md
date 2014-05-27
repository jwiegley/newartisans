---
title: Nightly builds of GHC HEAD for Ubuntu 12.04.2 LTS
description: desc here
tags: 
date: [2013-06-19 Wed 01:32]
category: Haskell
id: 441
---

Chatting with merijn on #haskell, I realized I have a file server running Ubuntu in a VM that's idle most of the time, so I decided to set up a jenkins user there and make use of it as a build slave in the evenings.  This means that at [http://ghc.newartisans.com](http://ghc.newartisans.com), you'll now find nightly builds of GHC HEAD for Ubuntu as well (64-bit).  It also includes fulltest and nofib results for each build.
