---
title: "Script of the week: redirect"
description: desc here
tags: 
date: 2007-11-12 05:59
category: Uncategorized
id: 179
---

This week's script of the week is so simple, it doesn't really deserve to be called a script.  But since it's highly useful and comes as a surprise to many people that it can be done so easily, here it is.

<!--more-->
The purpose of this script is to create momentary TCP routes.  TCP routing is also called Layer 4 routing.  That is, one machine momentarily serves as a transparent gateway between two TCP ports on two other machines.  The advantages to layer 4 routing are:

1. It's "port to port" (you aren't opening up subnets to each other, or even whole machines).
2. It doesn't require complicated routing tables entries, or IP forwarding, or NAT.
3. It can be done entirely in user space.  No strange kernel drivers required!

Here's an example: Let's say you use a web server sitting on a private network which you access over VPN.  You can see the server just fine by typing it's address in your web browser.  One day, however, you find a bug on the server, but it only happen on that server like for your friend -- who knows about such servers -- to see what's happening, but you obviously can't grant him access to your secured network.

What would be really cool is if your friend could connect to your machine instead, and have your machine transparently proxy the connection into the VPN and over to that web server.  It would also proxy responses back, so that from your friend's point of view: your machine *becomes* the web server for as long as you keep the link up.

Here's the command to do this, assuming I expose port 8080 on my machine for my friend to connect to, and I'm linking him to port 80 on the VPN's web server:

    $ tcpserver  8080 nc  80

Did I mention that this doesn't even require root privileges to work?

**NOTE**: If you have the [socat][] utility installed, things get even simpler.  In that case, the above command is just this:

    $ socat tcp-listen:8080 tcp::80

Now you have a transparent route from port 8080 on your machine to your secured web server.  After your friend is done checking things out, just cancel the command and the tunnel is destroyed.  This is the best way I can think of to temporary and easily create transparent tunnels into otherwise inaccessible networks.

For this scriptlet to work, you'll need [ucspi-tcp][] installed (for the `tcpserver` command), and [netcat][], which comes pre-installed on OS X 10.5.

[socat]: http://www.dest-unreach.org/socat/
[ucspi-tcp]: http://cr.yp.to/ucspi-tcp.html
[netcat]: http://netcat.sourceforge.net/

