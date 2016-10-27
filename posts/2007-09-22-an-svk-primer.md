---
title: An SVK primer
description: desc here
tags: 
date: 2007-09-22 03:38
category: Uncategorized
id: 200
---

Today’s entry is a little primer I wrote for some co-workers at CEG, on setting up SVK to do remote development.  We continue to use a central Subversion repository, but I often find myself working in cafés where I don’t have immediate access to the server.  Also, I like to branch and check-in much more frequently than would be sane to do with Subversion -- I also like the distinction between a “check-in” being a simple, quick snapshot, and an `svk push` as the real deal.

<!--more-->
## Setting up remote mirrors

SVK is a simple wrapper around Subversion that provides some of the better features of Distributed Version Control systems without a lot of the complexity that such systems usually involve.  This primer aims at giving a moderate to seasoned Subversion user quick access to the better features of SVK.

### Creating "depots"

The first step to using SVK is to create a local mirror of a remote repository.  But even before that, you need a local depot to track them in.  Here is the first command you need to run to get started:

	svk depotmap -i

This initializes a private depot named "//" in `~/.svk/local`.  All of the projects you mirror locally will be tracked here, and any projects you create on your own machine are kept here.  `~/.svk` is where SVK "lives".  When SVK asks if you want to create the missing directory,just say yes.

It's also possible to have multiple depots -- you might have one for personal projects, one for work, and one for tracking free software. Here's how you'd go about creating a depot named `/CEG/` for tracking CEG projects, in the directory `~/CEG/.svk`:

	svk depotmap -i /CEG/ ~/CEG/.svk

SVK asks to create the directory for you, and then initializes it so it's ready for importing/creating projects within it.

### Mirroring projects

To recap, every project you track lives in a depot.  The depot name occurs at the start of every directory string you use to identify projects.  The default depot name is "//"; if you created a CEG depot,that one is named "/CEG/".  Here's an example of how to mirror a personal project in `//`, and a CEG project in `/CEG/`:

	svk mirror https://ledger.svn.sourceforge.net/svnroot/ledger/trunk \
	           //mirror/ledger/trunk

	svk mirror svn://svnhost.3dex.com/project/MyProject/trunk \
	           /CEG/mirror/MyProject/trunk

The path "/CEG/mirror/" is just a convention, but it will be very helpful later on.  Also, rather than mirroring the entire project `MyProject`, it's much better just to mirror the trunk and any specific branches you need.  With SVK, it's easy to integrate mirrors of other branches later on.  Let's quickly add a mirror for the `a-new-port` project to the CEG depot:

	svk mirror svn://svnhost.3dex.com/project/MyProject/branches/a-new-port \
	           /CEG/mirror/MyProject/branches/a-new-port

Voila! the `a-new-port` branch is now being mirrored also, alongside the trunk.

### Getting remote changes

Now that you have mirroring setup, you must "sync" to get all the latest changes -- which on the first run means *all* changes.  This first run will take a long time, so don't be dismayed, or abort the process thinking that it's hung.  The command to sync all of your mirrors is:

	svk sync --all

To sync a specific mirror, name the depot path.  You can also sync all the mirrors for a particular depot:

	svk sync /CEG/mirror/MyProject/trunk

	svk sync --all /CEG/

To get a list of your depots, use:

	svk depotmap -l

To see a list of all mirrored projects, use:

	svk mirror -l

As time goes by, you can now periodically update your mirrors using `svk sync --all`, which downloads all the changes that have been committed since the last time you ran it.

## Using SVK, Subversion-style

It's possible to use SVK solely as a mirroring Subversion client.  In this form of usage, checkins are committed to the Subversion repository immediately, just as if you were using Subversion itself.  The only benefit gained by using SVK in this mode is that you have full access to the repository's history, even when you're not connected.

Checking out a working tree from a mirror is a lot like `svn checkout`, except that you give the depot path, not the Subversion URL:

	svk checkout /CEG/mirror/MyProject/trunk MyProject

This creates a local working tree named `MyProject`, following the remote trunk.

Now let's say you disconnect from the network.  You will still be able to run the following command, showing differences between version 200 and the `HEAD`:

	svk diff -r200:HEAD META-INF/ejb-jar.xml

With Subversion, this command would need access to the remote repository to succeed; with SVK, it always happens at the speed of local access.

(NOTE: SVK uses it own revision numbers, which are not identical to those used in the Subversion repository.  This is because SVK revision numbers track *the number of changes that have occurred in your depot*, whereas Subversion tracks the number of changes that have happened to the remote repository overall (including changes in branches you may not be tracking).  So it always helps to use `svk log` to determine the correct revision numbers of the changes you're looking for.)

### Updating your working tree

If someone commits a change to the remote Subversion tree, you can get it by doing an `svk sync --all`, following by an `svk update` in your working tree:

	svk sync --all
	svk update

### Checking in changes

To check in changes, just do a `svk sync --all`, following by an `svk update`; resolve any merges conflicts -- just as you would with Subversion -- and then commit the changes:

	svk sync --all
	svk update
	# 
	svk commit -m "My commit comment"

The changes are posted immediately to the remote Subversion repository, and your local mirror is updated at the same time.

## Using SVK, Distributed-style

Using SVK in distributed mode requires only one extra step: creating a local branch of the remote mirrored project.  This local branch lives on your own machine, and all your future commits are made against it. To get changes down from the server, or push them back up to the server, SVK provides the commands "push" and "pull".  Here is a quick guide to setting up a local branch for distributed development:

	# Create the branch by doing a cheap copy
	# (this is identical to creating a branch in Subversion)

	svk cp -p -m "Created branch" \
	    /CEG/mirror/MyProject/trunk \
	    /CEG/local/MyProject/trunk

This command create a local branch in the `/CEG/` depot, with *almost* the same name as the mirror of the remote repository.  The mirror path begins with `/CEG/mirror` to show its contents are tracking the remote; the local branch begins with `/CEG/local` to show its contents live only on the local machine.

Once we've created the local branch, we can checkout a copy exactly as we did above, only using the new local branch path instead:

	svk checkout /CEG/local/MyProject/trunk MyProject

Now we have our local working tree again, whose contents (at the moment) are identical to what would have happened from a regular Subversion checkout.

### Updating your working tree

Let's say someone checks in changes to the Subversion repository.  We need to: 1) synchronize our mirror, 2) merge the changes from the mirror to our local branch, and 3) merge these new changes from the local branch into our working tree.  Fortunately, SVK has rolled all these commands into one:

	svk pull

That's it.  It will do the sync, update the mirror, update the branch, and then update our local working tree.  If you had wanted to do it manually, the steps would have been:

	svk sync --all
	svk smerge /CEG/mirror/MyProject/trunk \
	           /CEG/local/MyProject/trunk
	svk update

The `smerge` command is described later.  Most of the time, all you need will be `svk pull`.  You won't even have to do a sync anymore!

### Checking in changes

Checking in changes to a local branch is the best part about SVK, because they don't have to go to the remote repository right away. This means you can do multiple, quick checkins during a large work in progress without breaking any builds.

You can commit to the local branch in the same way as any Subversion commit:

	svk commit -m "First change"
	svk commit -m "Second change"
	svk commit -m "Third change"

These commits are quick and cheap, since they all go to a local branch on your own machine.  When you next do an `svk pull`, it will merge in any changes from the remote repository "underneath" your new changes, meaning it's easy to keep up-to-date with the latest trunk revision without interrupting your workflow.  This is the real beauty of distributed version control.

### Posting your changes

Because we've only committed our changes locally, we now have to "post" them back to the remote repository.  SVK also has an equally easy command for this:

	svk push

The `push` command can work in one of two modes: it can "replay" each local commit on the remote server, in order to preserve all your commit history; or it can post all your local changes into one big commit, with all the merge comments glommed together in one comment:

	svk push      # push each local commit as a remote commit
	svk push -l   # "lump" all local changes into one remote commit
	svk push -C   # don't actually commit; show if it would conflict

For interests sake, the individual steps of the push command in this example would look like this:

	svk sync --all
	svk smerge /CEG/local/MyProject/trunk \
	           /CEG/mirror/MyProject/trunk

The process of merging into `/CEG/mirror` causes those commits to be immediately staged into the remote Subversion repository, since SVK maintains the mirror in perfect sync with the remote repository.  We are now back in line with the main trunk!

### Creating a local topic branch.

Let's say you're doing some heavy work, and you want to experiment with a possible optimization.  This means you want to pause current development in your local branch -- but you want to do your test work on top of these local changes, without having to check them in first.  In SVK this is a breeze.

First, fully commit your current work into the local branch.  Then, make a snapshot of your local branch to a local topic name:

	svk commit -m "Committing work to begin topic branch"
	svk cp -p -m "Created topic branch" \
	    /CEG/local/MyProject/trunk \
	    /CEG/local/MyProject/branches/optimization-test

Now switch your local working tree to track "optimization-test":

	svk switch /CEG/local/MyProject/branches/optimization-test

The changes you commit from this point onward are committed to the "optimization-test" topic branch.  If you ever need to switch back to the main local branch for any reason, just commit all current changes into your topic branch and say:

	svk switch /CEG/local/MyProject/trunk

As long as you commit before switching, you can switch back and forth as much as you like.  Plus, using `svk pull` *in either working tree* will pull in whatever recent changes have been made to the remote repository.  This lets you work on multiple branches of local development easily, without ever getting out of sync with the main trunk.

If you end up not liking your changes to the `optimization-test` branch, just switch back to your main local branch and delete the topic branch:

	svk switch /CEG/local/MyProject/trunk
	svk rm -m "Bad code" \
	    /CEG/local/MyProject/branches/optimization-test

If instead you really liked the changes and want to integrate them into your main local branch (to prepare them for committing to the remote), use the powerful `smerge` command to copy the changes over:

	svk switch /CEG/local/MyProject/trunk
	svk smerge /CEG/local/MyProject/branches/optimization-test .

The smerge command says to merge all changes committed in the `optimization-test` branch into the current working tree (`.`).  If you like the result, `svk commit` the changes back into your local branch. Then you can `svk push` to reflect them up to the remote repository.

## The power of "smerge"

`svk smerge` can be used not only for merging branch changes into a working tree, but also for merging changes directly from repository to repository, without involving any working tree at all.  However, it's easier to test the results of a merge if you use a clean working tree as the "staging area".

You can also use the `-C` option to `smerge` to do a "merge check".  This doesn't actually do any merging, but instead tells you what would have happened, and if any conflicts would have resulted from the merge.

Further, the `smerge` command maintains a historical state of all past merge operations, using regular Subversion properties.  This means that if you merge in changes from a topic branch one week, and then merge in later changes from the same branch a week later, only the new changes get merged in the second time.  `smerge` knows that it already has the older changes.

Here's how you would successively merge changes from the `MyProject` Subversion trunk into the `a-new-port` branch, using SVK.  I personally run this command every time I see new changes committed to the trunk:

	svk smerge /CEG/mirror/MyProject/trunk \
	           /CEG/mirror/MyProject/branches/a-new-port

By running this command every week, the "a-new-port" port in the remote repository stays up to date with changes in the trunk.

On the day when `a-new-port` is finally ready for prime time use, the reverse command will merge all those changes back into the trunk -- without overlapping any changes from those previous `smerge` runs:

	svk smerge /CEG/mirror/MyProject/branches/a-new-port \
	           /CEG/mirror/MyProject/trunk

Of course, with a command like this, it's MUCH safer to stage the merge results into a working tree for verification first.  Here's how such a session might play out:

	svk checkout /CEG/mirror/MyProject/trunk
	svk smerge /CEG/mirror/MyProject/branches/a-new-port .

	# resolve conflicts and/or correct any breakages
	svk commit -m "Merged in a-new-port"

	svk rm -m "Removed SVK mirror; we don't need it anymore!" \
	    /CEG/mirror/MyProject/branches/a-new-port

	svn rm -m "Removed Subversion branch; we don't need it anymore!" \
	    svn://svnhost.3dex.com/project/MyProject/branches/a-new-port

