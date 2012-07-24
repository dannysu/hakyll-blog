---
date: 2004-03-20 11:01:09
title: Accelerated-X Summit Series v2.2.2
tags: Linux Unix QNX
---
Accelerated-X is a commercial X Windows System made by Xi Graphics (xig.com)

I knew that there are commercial X alternatives to XFree, but I didn't know any
of the names. Last night I found out about xig.com, so I decided to give their
product a try.

The installation was very easy. They provided RPMs for all the things required.
The only thing I had to do was compile xsvc since I'm using 2.6.x kernel... You
don't need to mess around with recompiling kernel to get 3D to work. All I did
was install the things necessary, ran the Xsetup program to choose what
hardware I have and voila! glxgears was giving me consistently over 1400 FPS.
:)

I loved it at first try since it's so easy to set up. However, I am not sure if
you can change the cursor with Accelerated-X... that was the only feature that
seems to be missing. the Xsetup program is very useful and supports quite a bit
of hardware! Even though I didn't try, I think Xsetup allows you to set up
multiple pointer devices easily! That means I can probably set up my Wacom
tablet quite easily too.

Only down side to Accelerated-X was that it will automatically close after
25mins if you don't have a license. I couldn't find one, and I don't want to
pay nearly $100 for the Platinum edition. :(

I removed some GL stuff when installing Accelerated-X, so I reinstalled XFree
afterwards. I wanted to compare the glxgears output, and guess what I found?
with XFree, my FPS is around 1000... which is lower than the 1400 I was getting
from Accelerated-X &gt;:(
