---
title: Linux DHCP and Windows DNS
---

I feel a need to blog about this today because it took several days to figure out, but the solution was trivial.

The scenario: my company has a Windows 2003 Domain Controller running DHCP, DNS and Active Directory services.  We use an Untangle box as our gateway to the Internet.  All of this works just great for Windows machines on the network, where everyone can use names like "host" to refer to each other's machines.

<!--more-->
However, the Linux boxes until now have been second-class citizens.  They are able to get IP address via DHCP, but Windows knows nothing about their hostnames.  Nearly all of our Linux boxen run CentOS 5, which is to say, Redhat 5 (RHEL).

The solution, it turns out, is two-fold:

 1. On the Windows Domain Controller, go to the admin page for "Active Directory Users and Computers".  Under "Users" for your domain, create a new user named something like "dhcp4dns".  Pick a random password.

 2. On the same machine, go to the admin tool for DHCP and right-click on your domain and select Properties.  Click on the DNS tab and check everything, while also selecting "Always dynamically update DNS A and PTR records".  Then click on the Advanced tab and its Credentials... button.  Here enter the details for the user you created in step 1.

 3. This step is for CentOS: For every Linux box, edit the file `/etc/sysconfig/networking/devices/ifcfg-eth0` (or whichever interface faces your local network).

Add the following line to that file, replacing *hostname* with your unqualified hostname:

    DHCP_HOSTNAME=_hostname_

Now just reset networking on the Linux box:

    # ifdown eth0 ; ifup eth0

Voila, your Windows server should now see the Linux box's name just like everyone else on the network.

