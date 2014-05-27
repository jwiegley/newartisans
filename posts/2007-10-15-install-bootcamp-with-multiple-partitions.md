---
title: Install BootCamp with multiple partitions
description: desc here
tags: 
date: [2007-10-15 Mon 03:10]
category: Uncategorized
id: 193
---

Just as a note to others who were dismayed by the inability to have multiple partitions and to use BootCamp at the same time, here are instructions for how to do so. I gathered these from various sources, and it worked great for me, so hopefully you'll have success with it too.

<!--more-->
1. Install the BootCamp software on your computer. BUT DON'T RUN IT.

2. Using the Finder, find the BootCamp executable under your `/Applications` directory. Use the option to "Show Package Contents", and browse into the `Contents/Resources` directory. There you should find a `.dmg` file, a disk image.

3. Run Disk Utility and drag this `.dmg` file into the pane on the left-hand side.

4. Select the image in Disk Utility, and click on the Burn icon. Burn it to a CD and then keep this CD handy.

5. Now boot your machine with the OS/X installation disc. Press "c" so that it boots from the install Disc.

6. Under the Utilities menu, pick Disk Utility.

7. Now, select your "Macintosh HD" volume, and then click on "New Image". You'll need an external hard drive for this point, because it's going to take a lot of space to backup your machine.

8. Save the image to your external drive, and then go get some coffee. This is going to take some time.

9. Once you have your external image, I highly recommend rebooting your machine and then mounting (and thus verifying) and browsing around in that new image to make sure everything looks right.

10. Now that you're certain you have your data nicely backed up, reboot back to the installation disc (holding down "c" at the chime again), and again run the Disk Utility.

11. Now pick your main hard drive (not just the Macintosh HD volume), and click on the Partition tab.

12. Click on your single partition and choose to split it into 3 parts. Allocate the parts how you like. I used 25G for the system and applications, 90G for User data, and 31G for Windows. This was on a 160G drive.

13. MAKE SURE THE WINDOWS PARTITION IS LAST. You should have the System first, then your second partition, then Windows. I read that this is a requirement, although I never tested anything different.

14. Name the first partition "Macintosh HD". If you don't there is a small error you'll get while trying to restore (although it's nothing serious, but it's a bit of a shock).

15. Go ahead and format the third partition as MSDOS File System too.

16. Now click Partition. This will wipe out your data and repartition the disc.

17. Now select your new Macintosh HD volume, and click on the Restore tab. In the "Where to restore from", click on "Image..." and pick the disk image from your external drive that you made earlier. Then, drag the "Macintosh HD" volume onto the "Volume to restore to" textbox. Then click restore. This will take a serious amount of time, so be prepared to do something else.

18. When all is done, reboot your machine and make sure you can get back to OS/X. Everything should function identically to before, except that you now have three volumes mounted when you boot up.

19. Now put in your Windows XP SP2 install CD and reboot, holding down the "c" key. It will boot the setup process, and ultimately show you a list of partitions. DO NOT CHANGE THE PARTITION TABLE FROM SETUP. If you do, you will have to start this whole process over again. (This happened to me).

20. Anyway, ignore the inexplicable Unpartitioned space that you'll see in this table, and pick the "C:" drive to install to. This should be your 31G Windows partition that you made before.

21. I ended up formatting this partition as a FAT volume, so that I could view it from OS/X. You can use NTFS, but then you'll have to use CDs or external media to transfer files between the two systems. Up to you.

22. Follow through the install procedure and install Windows. Whenever Windows restarts, you'll have to hold down the OPTION key so that you can pick your Windows drive to reboot from.

23. When Windows is all done installing, and has booted up, put in your BootCamp disk and let it go through its installation procedure. Once that's done, you can reboot Windows and have nice graphics and sound and fancy trackpad input, etc.

24. That's it! You now have three partitions and Windows running nicely. Just hold down Option on boot whenever you want to startup Windows.

I use Windows via BootCamp just for games, since it offers Direct3D support. For most other windows stuff, I find that Parallels and VMware Fusion both are very quick and extremely handy. I have my VMware setup so that I can start my BootCamp partition either directly, or in a virtual machine (for those long downloads when I'd prefer to be doing other stuff in OS X besides).

