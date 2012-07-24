---
date: 2004-10-31 10:30:32
title: lack of systray support
tags: Linux Unix QNX
---
the number of programs lacking this is pissing me off... refering to
thunderbird and evolution

I was playing around w/ [Devil's Pie][1] yesterday and today... then an idea
strike me!

why can't we have a generic notification area app?  

take devil's pie code that can set WM hints like hide from tasklist & pager,
and then minimize it, also take eggtray.c & eggtray.h from GAIM project... put
them together and there you have it: a program that'll take any application,
hide it and show a systray icon for it.

some of these days when I don't have a million things to do, I'll implement
this.

  [1]: http://www.burtonini.com/blog/computers/devilspie
