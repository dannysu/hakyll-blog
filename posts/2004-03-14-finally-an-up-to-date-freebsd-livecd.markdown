---
date: 2004-03-14 06:56:05
title: finally! an up-to-date FreeBSD LiveCD!
tags: FreeBSD
---
it's called FreeSBIE

Kevin is trying to set up DUMMYNET at ScanSoft Montreal so they can simulate
network conditions for testing.

I told him about thewall, which was able to load 3COM driver needed by the
computers in the lab. However, there's one problem: DUMMYNET wasn't compiled
into thewall's kernel by default :( so what do you do? I thought about
compiling my own version of thewall, and so I did.
[[download]](/files/bsd/thewall.incl.dummynet.flp) :) you can use the same
driver.flp from thewall's website

While browsing on freebsdforums.org, I found the news about FreeSBIE! It's a
LiveCD project (Meaning you can boot the OS from CD and use it right away w/o
installation!). There is another LiveCD project that's like... FBSD 4.6 or
something? Even though you can make your own, it's still quite a bit of
trouble. FreeSBIE, on the other hand, is running on the newest FreeBSD 5.2.1!

on Linux side you have Knoppix and Gnoppix, NOW I can say FreeSBIE for FreeBSD
systems! Default WM is mixture of XFCE and GNOME stuff I believe! It looks
awesome! :D

ok so that's the good news... let me rewind back to thewall for a bit. thewall
is based on picoBSD, and according to the author (of thewall), thewall won't
build on FreeBSD 4.6 and up. :( due to floppy size restrictions... I wasted a
lot of time trying b/c I was using 4.9-RELEASE.

In the end, I had to find the ISO for 4.5-RELEASE to be able to build it
successfully for a 1.44 floppy disk. well, tried downgrade from 4.9-RELEASE but
that didn't go so well. make buildworld failed...

What does this mean when people want to find FreeBSD floppy disk solutions???
They'll need to turn to older fbsd versions from now on?
