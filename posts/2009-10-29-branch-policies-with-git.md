---
title: Branch policies with Git
description: desc here
tags: 
date: [2009-10-29 Thu 03:05]
category: Uncategorized
id: 226
---

I've been managing my [Ledger](http://wiki.github.com/jwiegley/ledger) project with Git for some time now, and I've finally settled into a comfortable groove concerning branches and where to commit stuff.

<!--more-->
Essentially I use four branches, in increasing order of commit frequency.  Each branch has its own policy and purpose, which are described below.

## maint

Every release of Ledger is made from the maint branch, and every commit on that branch is potentially a release.  This means that no commit is made until some serious vetting takes place.  When the master branch is at a state where I want to finally release it, I merge with =--no-ff=, so the merge gets represented as a single commit on the maint branch.  Then I tag the release and make a distribution tarball.

It's possible after a release that patches need to get applied to maint, and a point release made.  Once this is done, the applicable patches are either merged into master, or if the two diverse too greatly I will begin cherry-picking instead.  Once cherry-picking starts, no more merges into master will occur until after the next release merge happens in maint.

The purpose of maint is to provide the most stable release possible to the public.

## master

Master is where most people get the latest source code from, so it is kept reasonable stable.  There is a commit hook which guarantees that all commits to this branch build and pass the test suite.  Since most development work happens on "next", each time next is stable I merge into master, using =--no-ff= to keep the merge commits together.  I also use =--no-commit=, so the merge must pass the commit hook in order to go in.

Note that no commits are ever made directly to master, unless I've seriously broken something that needs to be addressed sooner than the next merge from "next".  In that case, I'll cherry pick this commit into master afterward.  Merges only happen into master from next, and only from master into maint.

The purpose of master is to provide reasonably stable development snapshots to the public.

## next

The next branch is where I commit most often, and while I try to keep it functional, this is not always the case.  I don't run unit tests here for every commit, just before every push (mostly).  Most of my friends follow this branch, because it updates very often.

The purpose of next is to provide potentially unstable, frequent development snapshots to the public.

## test

The test branch comes in and out of existence, and should only ever be pulled using =pull --rebase=.  It contains trial commits that I want someone to test out.  It's a delivery branch, and after it's been used I either delete it or ignore it until the next time it's necessary.

The purpose of test is to communicate patch candidates to a particular person at a particular time.

## topic

Then there are the various local-only topic branches that live on my machine, in which I develop highly unstable code relating to one feature or another, awaiting the day when it becomes stable enough to be merge into "next".

