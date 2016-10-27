---
title: OpenSSH connection mastering
description: desc here
tags: 
date: 2007-09-25 23:43
category: Uncategorized
id: 197
---

I just discovered a very cool feature of SSH today: control mastering.  It lets you multiplex a single ssh connection so you don't have to open multiple TCP connections to the remote host; instead, all your SSH/SCP commands "share" the initial connection.  This speeds up subsequent connections to the same host, and also means you don't have to enter your password more than once for hosts who don't know your public key yet.  I use this feature to implement a script for setting up new remote accounts.

<!--more-->
To use control mastering from the command line, first you need to open a connection which will act as the "master".  If you plan on keeping an interactive session open on the remote host, you might do something like this, if you use [the wonderful "screen" utility](http://en.wikipedia.org/wiki/GNU_Screen):

    ssh -Mt -S /tmp/ssh USER@HOST exec screen -DR

This command creates a master connection to the remote host, and invokes the screen command -- causing it to re-establish any previous session that might exist, but telling it to create a new session if none does.  I usually make this into a shell script for quickly opening new screen sessions to remote hosts.

Now that I have the master open, I can "piggy back" on the connection to run a quick command on the same host, without having to actually create a new TCP connection:

    ssh -S /tmp/ssh USER@HOST ls -l

Now, of course this is a little too verbose to be very useful.  But OpenSSH supports some extremely cool commands in your `~/.ssh/config` file.  Here's all you need to make of use "opportunistic" connection mastering:

    Host *
      ControlMaster auto
      ControlPath /tmp/%r@%h:%p.sock

What this says is that whenever an SSH connection is made, if there is no master for the connection already, make the new connection into a master.  However, if there is a master available, use a channel on the master's connection instead of initiating a new one.  You'll notice when a connection is a "slave" because it will create much, much faster than a regular master connection.  Of course, once the master quits, all of the slaves will be terminated, so be careful if you use this kind of setup!

As another example of how this can be used, here is my "connection prep" script, which I use to setup my basic shell environment on a brand new account for which I only have password-based SSH access.  The first thing I want it to do is to install my public-key, and then configure and change my shell to `zsh`.  By using connection mastering, I only need to type my password twice: once for the initial SSH master connection, and a second time for the `chsh` command.

    #!/bin/sh
    
    user=$2
    server=$1
    
    if [ "$user" = "" ]; then
       user=johnw
    fi
    
    ssh -MNf -S /tmp/sshsock.$$ $user@$server
    
    if ! ssh -S /tmp/sshsock.$$ $user@$server test -d .ssh; then
        ssh -S /tmp/sshsock.$$ $user@$server mkdir .ssh\; chmod 700 .ssh
    fi
    
    if ! ssh -S /tmp/sshsock.$$ $user@$server test -f .ssh/authorized_keys; then
        scp -o ControlPath=/tmp/sshsock.$$ \
    	~/.ssh/id_dsa.pub $user@$server:.ssh/authorized_keys
        ssh -S /tmp/sshsock.$$ $user@$server \
    	chmod 600 $user@$server:.ssh/authorized_keys
    fi
    
    scp -p -o ControlPath=/tmp/sshsock.$$ \
        ~/.screenrc ~/.zshenv ~/.zshrc $user@$server:.
    ssh -S /tmp/sshsock.$$ $user@$server ln .zshenv .zlogin
    ssh -S /tmp/sshsock.$$ $user@$server chsh -s /bin/zsh $user
    
    ssh -S /tmp/sshsock.$$ -O exit $user@$server

