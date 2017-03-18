---
title: Securing the Buildbot
---

One of the services I'm setting up on `newartisans.com` is [Buildbot][], so that every time changes get checked in the Ledger source tree, they'll be automatically built and tested on every platform I have access to.  Automatically.  Without any intervention on my part whatsoever.

[Buildbot]: http://buildbot.net/trac

In terms of ease of use and assuring that things get done, Buildbot is a dream.  In terms of system security, it's a bit more of a nightmare.

Buildbot is a Python script that runs as a standalone daemon, listening for connections from the slaves in its network.  Well, in terms of security I can never really trust a Python script.  If anyone was able to subvert the code over the public TCP port, they could tell the script to do just about anything.  It might not have much in the way of privilege, but at the very least it could read a whole lot of files on the system.

To run Buildbot securely means doing four things:

 1. Freeze the Buildbot Python script in a single static binary, with no references at all to the system while running.

 2. Run the server in a chroot jail, with access only to its own configuration files, its logs, and the state directories it uses to keep track of how the slaves are
 doing.

 3. Run the server as the nobody user in the nobody group, so that it has the least permission possible.

 4. Write an SELinux policy so that the daemon can only read and write exactly the files it needs to, and so it has permission to open a single TCP port and to send and receive traffic on that port.

With the environment set, a subverted daemon should just be a nuisance.  The attacker could start instructing all of the slaves to keep building continuously, hoping to starve their system resources.  And this can be alleviated by restricting builds to once daily.  I'll write more about the SELinux policy once it's written.

