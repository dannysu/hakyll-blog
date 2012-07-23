---
date: 2006-04-09 17:57:37
title: GTK+ Mac OS X port
tags: Apple
---

This means we'll get tighter integration of apps!

Although still a work-in-progress, some code has already been checked in to GNOME CVS. At this time I can get simple apps running without any problem:

![](http://www.dannysu.com/wp-content/uploads/2011/03/gtkmac.png)

More information can be found at Imendio AB's website: [http://developer.imendio.com/wiki/Gtk_Mac_OS_X#Community](http://developer.imendio.com/wiki/Gtk_Mac_OS_X#Community)
Here's the announcement: [http://mail.gnome.org/archives/gnome-announce-list/2005-November/msg00044.html](http://mail.gnome.org/archives/gnome-announce-list/2005-November/msg00044.html)

Having to run GTK apps using Apple's X11 environment has always bugged me, and I didn't know any better. Although the apps work, they are isolated in this special environment that's separate from the rest. For example, I can't do the normal Command+Tab keys to cycle through them.

The GTK application that I use the most is [GIMP](http://www.gimp.org/). I tried to get it working but no luck yet There are whole bunch of FIXME's and things left to code in the project, but I think it's looking very promising. I will likely shift my focus to this project now too. My aim would be to get GIMP running flawlessly. To get to that stage, I'd need to do quite a bit of hacking and also help contributing to the project effort to implement some of those FIXMEs.
