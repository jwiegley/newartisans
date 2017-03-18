---
title: Too much good stuff
category: Uncategorized
---

I have been somewhat lax with blogging lately, because I’ve been too absorbed in playing with too many new things (and busy at work, with real life, and etc).  So thought I’d just make a quick list today to share some of the exciting stuff I’ve been finding...

<!--more-->
## Puppet

[Puppet][] is a framework for configuring systems, both your local system for the sake of replication, or countless legions of remote systems such as servers you might be required to administer at work.  I’m using it now to configure my VPS server, my laptop, and a few Linux servers at work.  There is also a great book titled [Pulling Strings with Puppet][].  That, the [Puppet][] website, the helpful people on #puppet at irc.freenode.net, and reading through the Ruby source code have been a pleasant adventure this past week.

[Puppet]: http://reductivelabs.com/
[Pulling Strings with Puppet]: http://www.apress.com/book/view/1590599780

## Ruby

Since Puppet is written in [Ruby][], it’s forced me to take another look at this somewhat eclectic language.  My reading material has been a PDF of the new edition of [Programming Ruby][].  I’m still having trouble with Ruby’s aestethic inconsistencies (`initialize` instead of `init`, but then `to_s` instead of `to_string`?), but there are a lot of things to like too.

[Ruby]: http://www.ruby-lang.org/en/
[Programming Ruby]: http://www.pragprog.com/titles/ruby/programming-ruby

## Groundwork

At work we have a huge number of uncataloged servers running all kinds of services.  It has happened more than once that we suddenly discover a service has stopped running -- right when we needed it.  [Groundwork][] is a free site monitoring facility, based on Nagios, Cacti, and a few other packages, which can be used to keep tabs on the health of your entire network.  You can even download a [Groundwork VMware appliance][], saving you from having to install anything at all!  You only need a [VMware Server][] (also free!) running to host the appliance.

[Groundwork]: http://www.groundworkopensource.com/
[Groundwork VMware appliance]: http://www.groundworkopensource.com/community/downloads/vmware.html
[VMware Server]: http://www.vmware.com/products/server/

## ECL

One of my lost loves lately has been Common Lisp: a truly wonderful language for programmers, and a pretty nightmare for users inexperienced with deploying a Lisp environment on their machines.  Enter [ECL][], the embedded Common Lisp solution for C++.  With [ECL][] you can compile a Lisp program down a standalone binary which depends on the “libecl” shared library.  I find that such binaries start up about twice as slow as Python, but it’s the simplest way to use Lisp for little one-off tasks.

I tried for a few days to port [Qi][] -- a Common Lisp-based functional programming language -- to ECL, but to no avail.  Has anyone else gotten further with this?

[ECL]: http://ecls.sourceforge.net/
[Qi]: http://en.wikipedia.org/wiki/Qi_(programming_language)

## magit.el and egg.el

My search for a better Git interface for Emacs has led me to [Magit][], and its recent fork, [Egg][].  I still like `git.el` for some things, but [Magit][] is growing on me.

[Magit]: http://zagadka.vm.bytemark.co.uk/magit/magit.html
[Egg]: http://github.com/bogolisk/egg/tree/master

## Git

Playing with [Git][] has become a passion!  I’ve started a [git-scripts][] for all the tiny helper scripts I use with Git.

[Git]: http://git.or.cz/
[git-scripts]: http://github.com/jwiegley/git-scripts

## DD-WRT

A while ago I bought a Linksys [WTR54GL][] wireless router because I knew it was capable of supporting better firmware.  So I spent several hours last night reading about [DD-WRT][], and downloading everything I’m going to need to make the switch.  Hopefully this weekend!

[WRT54GL]: http://en.wikipedia.org/wiki/Linksys_WRT54G_series
[DD-WRT]: http://www.dd-wrt.com/dd-wrtv3/index.php

## ScreenRecycler

I’ve been wanting to buy a second monitor for some time now, but the cost of getting it to Grenada is too prohibitive.  I do, however, have another laptop here with me, my old PowerBook.  Tonight I found out about this cool program, [ScreenRecycler][], which lets you turn a second computer into a second monitor!  Will try tonight.

[ScreenRecycler]: http://www.screenrecycler.com/home.html

## Adeona

Being a bit of a security nut, I always like to try out the latest in protection and retrieval software.  At some point I stumble across this free program called [Adeona][], which keeps secure, anonymous records of everywhere your machine has been for the past week.  If it ever gets stolen, you (and you alone) can access those records in order to provide tracking information to the police.  And for those with MacBooks, it will even capture and record pictures every half-hour, in the hopes that you can catch the thief red... faced!

[Adeona]: http://adeona.cs.washington.edu/

## Lockdown

Also in the category of security apps, the nifty tool [Lockdown][] provides an easy way to better ensure that your Mac doesn’t grow legs and walk if you leave it unattended for a few moments at the local café.  It works based on your Apple Remote, so this is a MacBook only tool.

[Lockdown]: http://www.foozoodesign.com/lockdown.html

## JanusVM

For privacy nuts, there is the [Tor][] anonymity service.  But it can be a pain to install and get running sometimes.  If you just want to play with it, try out the VMware appliance [JanusVM][].  You fire it up, route your IP traffic through the virtual box, and suddenly you have a security-focused router between you and the Internet!

[Tor]: http://www.torproject.org/
[JanusVM]: http://www.janusvm.com/

## ipfw

I love firewalls, and firewall building tools.  The tool I use the most on my Mac is [ipfw][], which OS X inherited from its BSD origin.

Until this week I hadn’t gotten my firewall script from Tiger running again under Leopard, but it is now.  I’ve made it publically accessible, here: [rc.firewall][], and it’s usable on any Leopard or Tiger system.  For an example of how I run it, see [Firewall.hermes][].  I run it that script as a StartupItem on my MacBook Pro.

[ipfw]: http://www.macdevcenter.com/pub/a/mac/2005/03/15/firewall.html
[rc.firewall]: http://github.com/jwiegley/jw.firewall/tree/master/rc.firewall
[Firewall.hermes]: http://github.com/jwiegley/jw.firewall/tree/master/Firewall.hermes

## rpmreaper

Linux systems are very easy to configure these days, but I still find that “cruft” often accumulates in the form of installed packages later forgotten.  The excellent little utility [rpmreaper][] has been a joy to use, helping me to clean up my CentOS-based VPS which was starting to show signs of bloat.

[rpmreaper]: https://fedorahosted.org/rpmreaper/

## Shimo

I use [OpenVPN][] both personally and for work, but a recent contract has required me to use the Cisco VPN client.  I did some research on better Mac clients than the one offered by Cisco, and found the cool little app [Shimo][].  Not only is it a much better Cisco client, but it supports [OpenVPN][] as well!  The only downside so far is that, unlike [Tunnelblick][], I can only have one VPN connection active at a time.  I can, however, run them both if necessary.

[OpenVPN]: http://openvpn.net/
[Shimo]: http://www.shimoapp.com/
[Tunnelblick]: http://code.google.com/p/tunnelblick/

## ExpanDrive, FUSE and encfs

I stumbled across a [review][] of [ExpanDrive][] the other day, and decided to give it a try.  It really is quite fast.  I tried setting up [MacFuse][] and using [sshfs][], but it was nowhere near as responsive.

I’ve been trying to get more into FUSE, as I’ve thought about switching to [encfs][] as a better method for keeping files encrypted, but I found a [bug][] which has completely stopped me from using it on OS X.

[review]: http://daringfireball.net/2008/03/expandrive
[ExpanDrive]: http://www.magnetk.com/expandrive
[MacFuse]: http://code.google.com/p/macfuse/
[sshfs]: http://fuse.sourceforge.net/sshfs.html
[encfs]: http://www.arg0.net/encfs
[bug]: http://code.google.com/p/encfs/issues/detail?id=11

## Porticus

Being a big fan of [MacPorts][], I’ve often pined for a better search-and-install mechanism.  Look no further than [Porticus][].

[MacPorts]: http://www.macports.org/
[Porticus]: http://porticus.alittledrop.com/

## muCommander and M-x sunrise-commander

Remember the old, old days of Norton Commander?  I used to *love* that program.  You can now have something of the old experience on the Mac with [muCommander][].  Or, if you’re an Emacs person, be sure to check out [Sunrise Commander][].

[muCommander]: http://www.mucommander.com/
[Sunrise Commander]: http://www.emacswiki.org/cgi-bin/wiki/Sunrise_Commander

## JDiskReport

Most of the Java GUI applications I’ve tried have left me with a taste of bile lingering in my mouth.  Not so with [JDiskReport][], a free disk space analysis utility.  Not only is it beautiful, but it’s just as useful as all the payware alternatives I’ve found.

[JDiskReport]: http://www.jgoodies.com/freeware/jdiskreport/

## DbVisualizer

For months now I’ve been using the free Java app [Squirrel SQL][] to query the various SQL databases that I have to deal with.  Now I’ve found a more attractive alternative in [DbVisualizer][], which is a free app as long as you don’t need to modify your database with it.  It support things like “monitors”, which will notify you if the results of a query suddenly change.

[Squirrel SQL]: http://www.squirrelsql.org/
[DbVisualizer]: http://www.minq.se/products/dbvis/

## Lingon

Did you know that your Mac can be scheduled to run programs at specific times, or upon connection to a socket?  You can have it happen at all times, or only when you’re logged in.  Until now, accessing this service, which uses `launchd`, has required writing slightly complicated rule files in XML.  With the tool [Lingon][], however, it’s now quite easy.  The biggest downside I’ve found is that it doesn’t give you access to the socket-based functionality, as its 1.x version did.

[Lingon]: http://lingon.sourceforge.net/

## BootChamp

For those who dual boot their MacBooks into Windows to play games, you have to check out [BootChamp][]!  No more doing a reset only to find out that you forgot to hold down the option key (Arghh!).  With [BootChamp][], I’m now only moments away from the next round of Call of Duty.

[BootChamp]: http://www.kainjow.com/

## Emacs Chess, and ChessDB

I started working on my chess client for Emacs again, [Emacs Chess][].  It plays on [freechess.org][] with it, as does my co-author Mario.  Recently I setup a stress test that pits Emacs Chess against a 4.2 million game chess database, using [ChessDB][] as the storage engine, to verify that Emacs Chess correctly evaluates the legality of every position in those games.  It runs at around 2100 ply/sec on my laptop now, after a bit of tweaking.

[Emacs Chess]: https://github.com/jwiegley/emacs-chess
[freechess.org]: http://freechess.org
[ChessDB]: http://chessdb.sourceforge.net/

## socat

One of the most powerful CLI networking tools out there used to be `netcat`, the concept of which has now been consummated in its big brother, [socat][].  Consider [socat][] like a layer 4 swiss army knife on steroids.

Want to securely connect stdin and stdout to a socket on a remote machine over ssh, without using tunnels?  Here’s how:

    socat EXEC:”/usr/bin/ssh $host /usr/bin/socat TCP\:$host\:$port -’ -

This kind of rule is very handy if you want to create an `inetd` rule which establishes a secure remote tunnel, but only on demand.

[socat]: http://www.dest-unreach.org/socat/

## LFE

Erlang has been one of those languages that I really want to use, but have no professional need for (yet).  Being able to access its facilities with a Lisp-like syntax makes its much more accessible to me, however, so I’ve been playing a little bit with [LFE][].  Still need to find a compelling task to solve with it, though...

[LFE]: http://best-of-erlang.blogspot.com/2008/03/lfe-lisp-flavoured-erlang.html

## Bark River Bravo-1

This last entry isn’t exactly in the world of computers, but it’s been a cool obsession nonetheless.  My brother recently ordered a Bark River [Bravo-1][] for his camping adventures.  Once I hear from him about his experience I’ll be queuing up an order for one myself.  I’ve always been a sucker for a good knife.

[Bravo-1]: http://www.dlttradingcompany.com/index.php?cPath=24_342

