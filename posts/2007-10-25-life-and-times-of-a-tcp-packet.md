---
title: Life and times of a TCP packet
description: desc here
tags: 
date: 2007-10-25 06:53
category: Uncategorized
id: 188
---

Have you ever wondered how data reaches your computer from all over the world as you browse the Internet?  You may have heard of TCP/IP, but what exactly is it doing to reach that single Web server over in France, all the way from the United States?  How does that information reach you?

This article examines how a single connection works, from my computer on the island of Grenada to another computer sitting in southern France -- in this case, the website of my favorite soap company, [Marius Fabre][].

<!--more-->
To start this chain of events, click on the link in the above paragraph.  You should see the home page for the soap company after just a few seconds.  But what happened?  How did your computer suddenly find itself talking to another computer in the south of France?  How is the link between these your computers being formed?

You might think there is some central directory: that your computer asks this central machine "somewhere out on the Internet" for that other computer's address, and then connects to the remote machine somewhat like the old telephone operators of yore.  But such a system would be far too inefficient for a world with millions of computers.  Rather, the TCP/IP protocol is entirely *decentralized*, making the fact that you can always reach the same machine, from anywhere in the world, a bit magical.  Let's peel back a few layers from that magic act...

[Marius Fabre]: http://www.marius-fabre.fr/site/index.htm

## Step 1: Turning remote names into IP addresses

The first thing your web browser does when clicked on the [Marius Fabre][] link is to look up the web server's name.  The name `www.marius-fabre.fr` is nice and descriptive, but just like a person's name it doesn't tell us how to find them.  We need something like a phone book, to look up the name and turn it into a number machines can answer to.

This lookup is done using a decentralized name resolution protocol called "DNS", or Domain Name Service.  [How DNS works][] is beyond the scope of this article, but put briefly, it is a hierarchical, distributed database of domain names to address mappings.

When your Internet connection was first setup for you, somebody, somewhere, installed a DNS address in your home or work router.  If you choose, you can even configure your TCP/IP settings manually to use a specific DNS server, such as those provided by [OpenDNS][] (which I use from my laptop).

When your computer asks for an address relating to a domain name, it send this name to that configured DNS server in the form of a query.  The server responds to this query with either a) the hostname is unknown, or b) here's the address.  In the case of Marius Fabre, the IP address returned is `212.100.249.230`.  I was able to find this out on my OS X laptop by running the following command:

	Hermes:/Users/johnw $ host www.marius-fabre.fr
	www.marius-fabre.fr has address 212.100.249.230

[How DNS works]: http://www.howstuffworks.com/dns.htm
[OpenDNS]: http://www.opendns.com/

### The chain of DNS servers

For those curious, your local DNS server was able to find the address for Marius Fabre by asking one of the DNS root servers for a regional server that will answer for the "fr" top-level domain (TLD).   There are 13 root DNS servers in the world, all of them with names like `.root-servers.net` -- with the first being `a.root-servers.net`.  These root servers provide routing addresses for all the top-level domains, such as `fr` for France.  In the case of France, their regional DNS servers begin with `a.ext.nic.fr`.  So the DNS server you originally queried, after talking to the DNS root server, will then ask this server.  *That* server then goes on to ask an even more local server named `ns1.mailclub.fr`.  This server happens to be close enough to Marius Fabre's ISP that it  knows the answer to our question and is able to return the final IP address, back through the chain, to the original querying host.  Here's what that query looks like, using the tool `dnstracer`:

	Hermes:/Users/johnw $ dnstracer -s . www.marius-fabre.fr
	Tracing to www.marius-fabre.fr[a] via A.ROOT-SERVERS.NET
	A.ROOT-SERVERS.NET [.] (198.41.0.4)
	  \___ A.EXT.NIC.fr [fr] (193.51.208.14)
	       |\___ ns1.mailclub.fr [marius-fabre.fr]
	             (80.245.60.10) Got authoritative answer
	        \___ ns2.mailclub.fr [marius-fabre.fr]
	             (193.151.86.13) Got authoritative answer

The answer to this query is two addresses, one of which is `80.245.60.10`.  This is not the address for `www.marius-fabre.fr` itself, but the DNS server who is able to authoritatively answer for it.  If I now query this final DNS server directly, I get the same answer I saw above:

	Hermes:/Users/johnw $ dig +short @80.245.60.10 www.marius-fabre.fr a
	212.100.249.230

This command asks the DNS server at `80.245.60.10` whether it knows the address (the "A" record) for `www.marius-fabre.fr`.  It replies with the address I received using the simpler `host` command.

## Step 2: Building the packet

To talk to the remote web server using a web browser requires first constructing a valid packet the remote server will respond to.  This is always an "HTTP payload", tucked inside a TCP/IP packet, carried along by an Ethernet frame.  Each piece of this puzzle is called a "layer"; so modern networking consists of five layers:

1. The physical layer.  You won't ever interact with this on your computer unless you write device drivers.

2. The Ethernet frame.  People who work with firewalls or packet filters see these all the time.

3. The IP (Internet Protocol) layer.  Layer 2 does the real addressing, while this layer handle world-wide, logical addressing (see below).

4. The TCP (Transmission Control Protocol) layer.  This layer is like the Anal Retentive Internet Chef who slices up your data and then puts it all back together again on the other side.

5. The HTTP, or "protocol", layer.  This is what your web browser creates and listens for.  Believe it not, your browser is entirely ignorant of the other four layers!  It just deals in HTTP: the HyperText Transfer Protocol.

Your Ethernet card handles layer 1 all by itself.  The operating system's device drivers, and your network router, take care of layer 2.  Layers 3 and 4 are managing by your kernel's "protocol stack", which is wholly device independent (i.e., this layer is managed in software).  While layer 5 is the sole concern of the application you're using, such as the web browser.

Let's use one of my favorite utilities, [Scapy][], to manually construct one of these monsters, piece by piece.  Normally this mess is built by the various parts of your computer as the data "travels down the line", but with scapy we can assemble it all at once by ourselves (excepting for layer 1, of course).

[Scapy]: http://www.secdev.org/projects/scapy/

### Step 2.1: The Ethernet frame

An Ethernet frame is what handles the routing from one computer to another.  It like a send off from machine A to machine B, without no one in between.  It is also constantly being rewritten in order to handle the multi-segment lifetime of an IP packet (I'll go into this in greater detail in Step 3).  But for now, let me just say that a properly formed Ethernet packet has to know two things: The Ethernet address of the card it's transmitted from, and the Ethernet address of the card it expects to be transmitted to.  Other than a "type" flag to differentiate Ethernet packet types, this is all the Ethernet layer cares about.

To find the Ethernet address for my own network card on OS X, I used the `ifconfig` command and specified the `ether` address family:

	Hermes:/Users/johnw $ ifconfig en0 ether
	en0: flags=8863 mtu 1500
	        tunnel inet  -->
	        ether 00:16:cb:a1:ce:3a

So `00:16:cb:a1:ce:3a` is the Ethernet address for my builtin Ethernet card on a MacBook Pro.  No other Ethernet card in the whole world shares this same address!  Such addresses are globally unique, a thing the Ethernet protocol mostly depends on.

If this is my source address, what is the destination?  It will be my home DSL router, which is the first "hop" on my packet's way out to the Internet.  To find this, I used `netstat` to lookup the address of my default (i.e., Internet) gateway:

	Hermes:/Users/johnw $ netstat -nr -f inet | grep default
	default            192.168.1.1        UGSc       17        4    en0

The gateway's IP address is `192.168.1.1`.  Now I need to know it's Ethernet address, since the IP address is only a "logical" address, not a "physical" one:

	Hermes:/Users/johnw $ netstat -nr -f inet | grep "^192\.168\.1\.1\>"
	192.168.1.1        0:18:f3:fc:24:a0   UHLW       19        2    en0

Aha!  The Ethernet address for my router is = 0:18:f3:fc:24:a0=.  I can now build the first part of my initial packet with Scapy:

	Hermes:/Users/johnw $ scapy
	INFO: Using session [/Users/johnw/Library/Caches/scapy/session]
	Welcome to Scapy (v1.1.1 / f88d99910220)
	>>> conf.iface='en0'
	>>> packet=Ether(src='00:16:cb:a1:ce:3a', dst='0:18:f3:fc:24:a0')
	>>> packet.show()
	###[ Ethernet ]###
	  dst= 0:18:f3:fc:24:a0
	  src= 00:16:cb:a1:ce:3a
	  type= 0x0
	>>>

### Step 2.2: The IP header

Ethernet frames represent a physical addressing layer, meaning it tells the packet how to go from one machine to the next.  But the DSL router is not my final destination.  How do I tell it it should send the packet on, out into the wide world of the Internet?  This is done with the IP, or Internet Protocol, layer.  Here is where I plug in the address we received from the DNS server in step 1:

	>>> packet = packet / IP(dst='212.100.249.230')
	>>> packet.show()
	###[ Ethernet ]###
	  dst= 0:18:f3:fc:24:a0
	  src= 00:16:cb:a1:ce:3a
	  type= 0x0
	###[ IP ]###
	     version= 4
	     ttl= 64
	     proto= ip
	     src= 192.168.1.10
	     dst= 212.100.249.230
	>>>

Here we have a full address packet showing that I want to reach the Internet address `212.100.249.230` (aka `www.marius-fabre.fr`) with the first "hop" starting at my home DSL router (this is shown by Ethernet frame).  But although we've specified the final address, we have yet to identify which "port" on that machine we'll connect to, since all Internet traffic must begin and end with specific ports on the source and destination machines.  That's the job of the TCP layer.

### Step 2.3: The TCP header

On top of all we've built so far, more must be added.  We have to tell the Internet that we want to talk to the HTTP (Web) port on the destination machine, which is port 80.  That's done very easily by adding a TCP packet with the destination port specified.  Since this is the very first packet we're sending, we must set the `SYN` flag.  (You can learn more about TCP SYN packets in [an earlier article][] I wrote on how to understand TCP reset attacks).  Let's build the TCP part on top of the other parts using Scapy:

	>>> packet = packet / TCP(dport=80, flags='S')
	>>> packet.show()
	###[ Ethernet ]### ...
	###[ IP ]### ...
	###[ TCP ]###
	        sport= ftp_data
	        dport= http
	        seq= 0
	        ack= 0
	        flags= S
	>>>

Now we have three pieces of the four-layer burrito made.  Remember that the first layer is handled by our networking card, so there's nothing we can do to make it ourselves in software.  But this latest piece, the TCP header, shows that we want to connect to the HTTP port on the destination machine and that we're initiating a new connection, indicated by setting the `SYN` flag.  If all goes well at the end of this exercise, we'll get a `SYN+ACK` packet back from Marius Fabre's web server meaning, "We're ready to chat".

[an earlier article]: http://newartisans.com/blog_files/tricks.with.iptables.php

### Step 2.4: Making the HTTP protocol layer

We need a final layer containing the actual HTTP request which says, "Can I look at your home page?"  The format of such an HTTP protocol request looks something like this, and is created inside your web browser:

	GET /index.html HTTP/1.0
	

The `RETURN` and `LINEFEED` elements here are shown for emphasis, instead of just printing whitespace.  They refer to the "\r\n" characters, also known as "carriage return, line feed".  There must be exactly two of them to end the request.

Heres how to create this protocol snippet with Scapy:

	>>> packet = packet / Raw("GET /index.html HTTP/1.0\r\n\r\n")
	>>> packet.show()
	###[ Ethernet ]### ...
	###[ IP ]### ...
	###[ TCP ]### ...
	###[ Raw ]###
	           load= 'GET /index.html HTTP/1.0\r\n\r\n'
	>>>

Here's what the whole thing looks like rolled together:

<span class="mt-enclosure mt-enclosure-file"><a href="http://www.newartisans.com/images/network-stack.tiff">network-stack.tiff</a></span>

------

### Step 2.5: Honoring the three-way handshake

Sadly enough, I can't just send this packet as it is, because we can't send along an HTTP protocol layer on top of a plain old `SYN` packet.  That's because the TCP connection hasn't been fully established yet.  So instead I'll write all the logic into script which uses Scapy to establish the connection, send the initial HTTP payload, and print out the responses from the server.  Here's that script:

	#!/usr/bin/env python

	import sys
	sys.path.append('/usr/local/bin')

	from scapy import *

	conf.iface='en0'                # en0 is my Ethernet card
	conf.verb=0                     # don't be verbose

	myether = '00:16:cb:a1:ce:3a'
	gwether = '00:18:f3:fc:24:a0'   # DSL router's Ethernet addr
	hostip  = '212.100.249.230'

	packet = (Ether(src=myether, dst=gwether) /
	          IP(dst=hostip) /
	          TCP(dport=80, flags='S'))

	resp = srp1(packet)  # send raw packet, listen for 1 reply

	if not resp or not resp.getlayer(TCP) or \
	   resp.getlayer(TCP).flags != 0x12: # SYN+ACK
	    print "Packet returned was not a SYN+ACK response:"
	    resp.show(); sys.exit(1)

	# Respond to the SYN+ACK with an ACK packet.  This completes the
	# TCP "three-way handshake", so that we are new connected and can
	# communicate.

	packet = (Ether(src=myether, dst=gwether) /
	          IP(dst=hostip) /
	          TCP(dport=80, flags='A', ack=resp.seq+1, seq=1))
	sendp(packet)

	# We can immediately begin talking by sending the intial HTTP
	# request, asking for their index.html page.

	packet = (Ether(src=myether, dst=gwether) /
	          IP(dst=hostip) /
	          TCP(dport=80, flags='PA', ack=resp.seq+1, seq=1) /
	          Raw("GET /index.html HTTP/1.0\r\n\r\n"))
	sendp(packet)

	sniff(filter="tcp and host %s" % hostip,
	      prn=lambda x: x.show())

### Response from the server

When I run this I saw a bunch of packets coming back in response.  Only the one with `PSH+ACK` flags contains the answer I care about.  Here's what it looked like after I ran it:

	###[ Ethernet ]###
	  dst= 00:16:cb:a1:ce:3a
	  src= 00:18:f3:fc:24:a0
	  type= IPv4
	###[ IP ]###
	     version= 4L
	     ihl= 5L
	     tos= 0x0
	     len= 518
	     id= 22085
	     flags= DF
	     frag= 0L
	     ttl= 51
	     proto= tcp
	     chksum= 0x5faf
	     src= 212.100.249.230
	     dst= 192.168.1.10
	     options= ''
	###[ TCP ]###
	        sport= http
	        dport= ftp_data
	        seq= 3089467956L
	        ack= 29L
	        dataofs= 5L
	        reserved= 0L
	        flags= PA
	        window= 5840
	        chksum= 0xba9d
	        urgptr= 0
	        options= []
	###[ Raw ]###
	           load= 'HTTP/1.1 403 Forbidden\r\nDate: Thu, 25 Oct 2007
	03:02:09 GMT\r\nServer: Apache/2.0.46 (Red Hat)\r\nContent-Length:
	297\r\nConnection: close\r\nContent-Type: text/html;
	charset=iso-8859-1\r\n\r\n\n\n<title>403 Forbidden</title>\n
	\n<h1>Forbidden</h1>\n<p>You don\'t have permission to access
	/index.html\non this server.</p>\n<hr />\n<address>Apache/2.0.46
	(Red Hat) Server at www.cetp.asso.fr Port 80</address>\n\n'

There are several things to note about this return packet:

1. The destination address in the Ethernet frame is the MAC address of my Ethernet card.

2. The IP protocol header is addressed to my local IP address.  This actually got rewritten when it hit my DSL router using NAT technology, but that's too much to go into here.

3. The TCP protocol header is coming from a *source port* of 80 (http), using a destination port of `ftp_data`.  `ftp_data` just means "some random, high-numbered port", and is used for most return traffic.

4. The flags in the returning TCP header are `PA`, which means `PSH+ACK`.  The `PSH` flag (for "Push") says that I should examine the payload data immediately.

5. The HTTP response came back in the payload!  It's telling me that I don't have permissions to access the page I requested; which is right, because this site happens to use the page `site/index.html` as its entry-point, not `index.html` (I know this from actually loading it in the web browser and seeing where it took me).

### The tools I used

Figuring all of this out took some time, but not forever thanks to some wonderful packet analysis tools: [tcpdump][] and [Wireshark][], which I used in combination to capture and analyze packets.  Here's how I ran `tcpdump` to capture the info about my HTTP connection:

	$ sudo tcpdump -s 0 -w /tmp/tcpdump.out -i en0 tcp port 80

After visiting the Marius Fabre home page in my browser, I cancelled the `tcpdump` command by hitting `Control-C`.  Then I loaded up the data in Wireshark so I could look at all the packets and their headers, all nicely formatted and broken down for me:

	$ wireshark -r /tmp/tcpdump.out

## Step 3: Sending the packet

At the end of step 2 we ended up with an open connection to the remote server.  But I want to step back for a moment and see exactly how the initial packet got there: the original TCP `SYN` packet which began the HTTP conversation.

If you remember, I created an Ethernet frame for my packet which directed the first packet from my computer to my DSL router.  I then transmitted this packet using my Ethernet card.  From there, the packet went to a five-port Ethernet switch that both my computer and my DSL router are plugged into.

Now, sending the packet to the switch is no problem.  I have an Ethernet cable plugged into my computer and there's only one thing on the other end: the switch.  So *anything* I sent from my Ethernet card is going to end up at the switch.  The question is, how does the switch know where to send it next?  How does it get back out of the switch, and over to my DSL router?

Most Ethernet switches learn over time the Ethernet MAC address for anything plugged into them.  They keep this information cached in their own memory stores, so that my switch knows: the MAC address of my computer, and that it's plugged into port 2; and the MAC address of my DSL router, and that it's plugged into port 1.  When it received my packet -- blasted at it through the Ethernet cable -- it looked up the destination MAC address in its little in-memory table and realized that it should pass it on via port 2, straight down yet another fixed wire that's plugged directly into my DSL router.

The packet has now reached the DSL router, the destination address of the Ethernet frame I created.  But wait!  Things can't stop there.  Although the destination MAC address of the Ethernet frame was pointed at the DSL router -- in order to get it through the switch -- the router's IP address is not same as the destination address in the IP header.

[tcpdump]: http://www.tcpdump.org/
[Wireshark]: http://www.wireshark.org/

### The final address

In short, every packet has two destination addresses:

1. The "first hop" destination, or the Ethernet MAC address written into the Ethernet frame.  This address must always be known to whatever my Ethernet cable is plugged into.  Most of the time this is an Ethernet switch, so it's the switch's job to carry my packet on to its destination -- which must also be plugged into the switch, or another switch connected to that one.

2. The "ultimate" destination, which is a 4-byte IP address written into the IP header.  This is the main job of the IP protocol: to make sure that the packet doesn't "stop" until either a) it has reached its final destination, or b) it's exceeded the number of hops specified by its TTL field (it's "Time To Live").

So when my packet reaches the DSL router, the Ethernet frame has completed its job, but the IP header has not.  In order to keep the packet alive, the DSL router looks at the TTL field in the IP header.  Has it reached zero yet?  If not, it decrements the TTL field, and then changes the Ethernet frame so the source points to itself and the destination points to the next hop.

For a home DSL router, the "next hop" is almost always an Internet gateway at the local ISP.  This is a machine, located not very far from you, to which you are connected via a DSL connection.  If you want to know more about packets move over DSL in particular, check out this [Wikipedia article][].  But since that subject is way beyond the scope of this discussion, we'll just assume it's like a giant Ethernet switch that routes packets from its many DSL subscribers to the main ISP gateway.

Assuming the DSL cloud honored the modified Ethernet frame's new destination address, it has now transported the packet over the phone lines and into the ISP's gateway machine.  This machine checks the destination address in the IP header, and realizes, "Nope, that's not me."  So it must find out which machine to send the packet to next.

[Wikipedia article]: http://en.wikipedia.org/wiki/Digital_Subscriber_Line

### Routing tables

All computers and routers have in memory a "routing table".  This is true of your local machine, of your DSL router, and of the gateway machine at your local ISP.  The routing table list the "next destination" for IP packets to take if they are not intended for the machine who received them.  Let's take a peek at the routing table on my own laptop as an example:

	Hermes:/tmp/trunk $ netstat -nr -f inet
	Routing tables

	Internet:
	Destination        Gateway            Flags    Refs      Use  Netif
	default            192.168.1.1        UGSc       17       11    en0
	127.0.0.1          127.0.0.1          UH         19   250048    lo0
	192.168.1          link#4             UCS         1        0    en0
	192.168.1.1        0:18:f3:fc:24:a0   UHLW       17        0    en0
	192.168.1.10       127.0.0.1          UHS         0        0    lo0

What we see here is that if I send a packet to the address `212.100.249.230`, none of the "specific" entries in my routing table will match.  If it were a `192.168.1.x` address, the third line in my routing table would cover that.  But since it matches none of them, the "default" entry is chosen.  This default entry is configured to send the packet to `192.168.1.1`, which is the IP address of my DSL router.  In order to send the packet there, it uses the destination Ethernet address shown on line 4 of the table, with the destination IP address of the final host (`212.100.249.230`).  By using the Ethernet address of the router, and the IP address of the final host, this tells the router that the packet should "break on through" to the other side.

The DSL router has a similar table, and the ISP's gateway has a similar table.  And so it goes, from one machine to another, each one rewriting the Ethernet frame and decrementing the TTL field as the packet moves onward, until one of the machines who receives the packet says, "Hey, that destination address in the IP header belongs to me!"  When that happens, our packet will have found its new home.

Most implementations start out the packets coming from your machine with a TTL of 64, meaning that this process can repeat across 64 machines before the Internet gives up on it.  Another thing to note is that not every routing table will have a single destination for a given IP address.  Some systems that deal with heavy load know multiple paths to a given destination, and will route your packet in different ways based on congestion and other factors.

Say, for example, that from the final US server to the first French server there is an option of using either the Trans-Atlantic Cable or a satellite linkup.  The cable is faster, but the satellite has more bandwidth.  So if the Cable happens to be relatively free right now, the packet will go that way; but if the Cable is too busy, it will go the satellite route.  Either way, the same logic that we covered above applies, with the packet being transformed at each step so it can reach the next hop.  The only thing you'd notice from a user perspective is that the satellite linkup has much worse latency.  That would be experienced as a slow response from an overseas web server while clicking the links.

## Conclusion

This has been a brief story of what happens to a TCP packet as it makes it way to the example web server at Marius Fabre.  In fact, here's the exact path it took for me, from here in Grenada, over the Atlantic, to the south of France.  I'm going to use Scapy to generate this output using a TCP `traceroute`, which does so well at mapping out things like this:

	Hermes:/tmp/trunk $ sudo scapy
	INFO: Using session [/Users/johnw/Library/Caches/scapy/session]
	Welcome to Scapy (v1.1.1 / f88d99910220)
	>>> ans,unans=traceroute('www.marius-fabre.fr')
	>>> ans
	
	>>> ans.graph(target="> /tmp/graph.svg")

This graph yielded the following path, which apparently took my packet through parts of the Carribbean, and the UK, after leaving Grenada:

<span class="mt-enclosure mt-enclosure-image"><img src="http://www.newartisans.com/images/ping-www.marius-fabre.fr.png" width="274" alt="ping-www.marius-fabre.fr.png" height="1388" class="mt-image-center" style="text-align: center;margin: 0 auto 20px" /></span>

And away it goes!

