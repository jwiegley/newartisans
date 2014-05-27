---
title: Running Common Lisp behind Apache
description: desc here
tags: Hunchentoot
date: [2007-11-05 Mon 23:25]
category: Uncategorized
id: 183
---

It's hard for me to think of a more ideal platform for web design than Common Lisp.  Imagine having a system that runs indefinitely, with the ability to "snapshot" its running state and restore exactly where you left off, and where updates can be applied live, at functional-level granularity, from anywhere.  Oh, and let's not forget the remote debugging and inspection capabilities!  And I thought Visual Studio with ASP.NET was nice.

<!--more-->
Where Lisp lacks today is primarily in easy to use, pre-packaged services.  One of these is getting it to run behind Apache, which, although easy to do, took a bit of figuring out from several different web pages.  Hopefully I can simplify the process of getting such a system running, so you can try out this highly understated environment for yourself.

## Install SBCL

The first step is to get Common Lisp running.  The system I chose for my remote webserver is the 64-bit version of [SBCL][] (version 1.0.11) running on CentOS 5.  I downloaded the pre-built x86-64 binary from their website and ran the following commands:

    # cd /usr/local
    # mkdir stow; cd stow
    # wget 'http://downloads.sourceforge.net/sbcl/sbcl-1.0.11-x86-64-linux-binary.tar.bz2?modtime=1193393807&big_mirror=1'
    # tar xvjf sbcl-1.0.11-x86-64-linux-binary.tar.bz2
    # stow sbcl-1.0.11-x86-64-linux

You should now have an `sbcl` executable in `/usr/local/bin/sbcl`.  Note: If you don't have `stow` -- which greatly simplifies the installation and management of local packages like this -- you can find the version I used [on rpmfind.net][].

[SBCL]: http://www.sbcl.org/
[on rpmfind.net]: http://rpmfind.net//linux/RPM/fedora/devel/x86_64/stow-1.3.3-5.fc6.noarch.html

## Install Hunchentoot and CL-WHO

SBCL is just a core system; it has very little in the way of pre-installed libraries.  To enrich your Common Lisp environment, therefore, you will need to install packages from the Internet.  This is made extremely easy by a system called `asdf-install`, which comes pre-built with SBCL.

The two packages we want to install are: the [Hunchentoot][] webserver; and the [CL-WHO][] HTML templating library, both by the same author.  The following commands are all you need:

    # /usr/local/bin/sbcl
    * (require 'asdf-install)
    * (push :hunchentoot-no-ssl *features*)
    * (asdf-install:install :hunchentoot)

This starts the installation, which downloads all of the dependency libraries Hunchentoot needs.  I've told Hunchentoot *not* to install SSL support, because we have Apache to do that for us.

For each dependency, you will be asked if this is to be a system-wide installation, or user-only.  Choose system-wide each time.  Then it asks if you want to skip the GPG signature check.  You can press zero followed by return to skip each signature check, or, if you're concerned, install the related signature files during installation and use the Retry restart to attempt each check again.

The output from this command is very noisy, because like most things in the Lisp world, `asdf-install` is just a core set of functions, not a presentation platform.  Get used to that.  After all, it's not that `asdf-install` needs to change -- it works very well for what it does -- but that a good UI be wrapped around it in the form of another library.  For Lisp excels at separating functionality from presentation; but since it's mainly played with by hackers and engineers, the presentation aspect is left for last, and by then more interesting problems have come up!

Anyway, once Hunchentoot finishes installing, next install CL-WHO:

    * (asdf-install:install :cl-who)

You should be able to run the demo webserver now, to test that everything is working OK:

    * (require 'asdf)
    * (asdf:oos 'asdf:load-op :hunchentoot-test)
    * (hunchentoot:start-server :port 4242)

If you point your browser at port 4242 on your webserver (assuming you have a way to access this port from a browser), you'll now see the intro:

[Hunchentoot]: http://www.weitz.de/hunchentoot/
[CL-WHO]: http://weitz.de/cl-who/

## Install Swank

This step is optional, but will allow you to interact with your live Hunchentoot installation from remote, via Emacs.  You'll need to download [Emacs SLIME][] and untar it somewhere.  Let's put it in `/usr/local/share/emacs/site-lisp`:

    # mkdir -p /usr/local/share/emacs/site-lisp
    # cd $_
    # wget http://common-lisp.net/project/slime/slime-2.0.tgz
    # tar xvzf slime-2.0.tgz
    # mv slime-2.0 slime

The `init.d` script you'll be downloading next is already configured to start the Swank server for you, if it finds SLIME installed in `/usr/local/share/emacs/site-lisp/slime`.

Once Hunchentoot is running, you can start an ssh tunnel to talk to your web server securely:

    localhost $ ssh -fN -L 4005:localhost:4005 hostname.where.hunchentoot.runs

Now type `M-x slime-connect` from your local Emacs, connect to host `127.0.0.1` at port 4005, and voila! you're interacting in real time with your Common Lisp app server.

[Emacs SLIME]: http://common-lisp.net/project/slime/

## Install mod_lisp2

In order to get Apache 2.2 to talk to Hunchentoot, we need two things: first, to get `mod_lisp2` built and installed; and second, to make sure that Hunchentoot starts on its own when the system is booted.

To build `mod_lisp2` for your platform, install the sources from the [mod_lisp home page][].  Let's put them in `/usr/local/src`, and then build and install the module for Apache:

    # cd /usr/local/src
    # wget http://www.fractalconcept.com:8000/public/open-source/mod_lisp/mod_lisp2.c
    # apxs -c -i -a mod_lisp2.c

[mod_lisp home page]: http://www.fractalconcept.com:8000/public/open-source/mod_lisp/

## Configure Apache

Configuring Apache is the easiest part.  I'm using virtual hosts on my server, so here's what my setup looks like:

    LoadModule lisp_module modules/mod_lisp2.so
    
    
        DocumentRoot /srv/httpd/newartisans.com
    
        ServerName www.newartisans.com
        ServerAlias newartisans.com
    
        LispServer 127.0.0.1 8080 "hunchentoot"
    
        
            SetHandler lisp-handler
        
    

With this configuration, if you browse to [http://www.newartisans.com/lisp][], you'll see my Hunchentoot app server up and running (though I'm not doing anything with it just yet, just playing around).

[http://www.newartisans.com/lisp]: http://www.newartisans.com/lisp

## Create a Hunchentoot user

The last thing you want is to run your Hunchentoot server with root privileges, so it's safer to create a dedicated system account just for running Common Lisp code.  Here are the commands to make such an account on Redhat systems:

    # adduser -d /var/lib/hunchentoot -c "Hunchentoot" -M -r htoot
    # mkdir /var/lib/hunchentoot
    # chown htoot:htoot /var/lib/hunchentoot
    # chmod go-rwx /var/lib/hunchentoot

## Start up Hunchentoot

Before Apache can talk to Hunchentoot, it has to be running as a server process that will automatically restart after a reboot.  Fortunately, Common Lisp has the very powerful feature of saving and resumes your entire Lisp environment's runtime state across reboots.  The `hunchentoot` initialization script to be downloaded below will take care of this detail for you.

Since I'm using CentOS, I like to run all system services from `/etc/init.d`.  So I created a [Hunchentoot startup script][] which you can download and install in your `/etc/init.d` directory.  You'll also need to install [some Lisp initialization code][] into `/var/lib/hunchentoot`, which takes care of server initialization, shutdown, and saving state between reboot.  Once this is all done, you can startup Hunchentoot and start using it right away:

    # cd /var/lib/hunchentoot
    # wget ftp://ftp.newartisans.com/pub/lisp/startup.lisp
    # cd /etc/init.d
    # wget ftp://ftp.newartisans.com/pub/lisp/hunchentoot
    # chown root:root hunchentoot
    # chcon system_u:object_r:initrc_exec_t hunchentoot
    # chkconfig --add hunchentoot
    # chkconfig hunchentoot on
    # service hunchentoot start

After a few seconds Hunchentoot should be listening for new connections on port 8080.  You won't be able to point your browser at this port; rather, you must use Apache, which talks to Hunchentoot on your behalf.  Also, I can only assume you've setup your firewall to prevent all but local accesses to this port.

[Hunchentoot startup script]: ftp://ftp.newartisans.com/pub/lisp/hunchentoot
[some Lisp initialization code]: ftp://ftp.newartisans.com/pub/lisp/startup.lisp

## Open up SELinux

For those running SELinux, a tiny bit of extra configuration is required, so that Apache is allowed to talk to Hunchentoot.  You'll need to create a file named `httpd-ext.te` containing these declarations:

    module httpd-ext 1.0;
    
    require {
        class file { getattr read };
        class tcp_socket name_connect;
        type httpd_t;
        type http_cache_port_t;
        type tmp_t;
        role system_r;
    };
    
    allow httpd_t tmp_t:file { getattr read };
    allow httpd_t http_cache_port_t:tcp_socket name_connect;

What this does is allow Apache to open connections to TCP port 8080 (where I have Hunchentoot running).  If you prefer to use a different port, change `/etc/init.d/hunchentoot`, and your Apache `config` file, restart Hunchentoot and Apache, and then reload your test page.  After it fails, run this command:

    # cat /var/log/audit/audit.log | audit2allow -r

You should see the statements you to add to your SELinux file there.

Once the SELinux file is ready, process it and load in into your running kernel:

    # checkmodule -M -m -o httpd-ext.mod httpd-ext.te
    # semodule_package -o httpd-ext.pp -m httpd-ext.mod
    # semodule -i httpd-ext.pp
    # rm -f httpd-ext.mod httpd-ext.pp

But after all this, all we've done is open up Apache to talk to Hunchentoot.  *We have not secured Hunchentoot with SELinux at all*.  To do that requires authoring a new policy for SBCL which indicates precisely what it can and cannot do.  Since doing so is a rather involved process, which depends entirely on how you're going to use Hunchentoot, I leave that as an exercise to the reader.  (On the other hand, if you'd like to hire me on a consulting basis to help you craft a custom SELinux policy for your Hunchentoot installation, please [contact me][]).

[contact me]: /contact.php

## Yay, Lisp!

If all has gone as planned, you should now have a Common Lisp app server running behind Apache, which remains available after rebooting.  Now here are a few "next steps" for those wishing more advanced functionality:

1. Use [Postmodern][] to bind your Lisp schema to a [PostgreSQL][] database.  This can provide advanced state management for systems with large data stores.

2. Use [HT-AJAX][] to add Ajax functionality to your Hunchentoot pages.

3. Use [caching in Apache][] to lighten the load on your Hunchentoot server, where applicable.

4. Create an SELinux security policy for SBCL, so Hunchentoot can't do anything you didn't intend it to do.

[Postmodern]: http://common-lisp.net/project/postmodern/
[PostgreSQL]: http://www.postgresql.org/
[HT-AJAX]: http://www.cliki.net/HT-AJAX
[caching in Apache]: http://httpd.apache.org/docs/2.2/caching.html

