---
date: 2004-03-30 15:15:05
title: xcin chinese input
tags: Linux Unix QNX
---
In order to type chinese to friends... you gotta set up XCIN or similar stuff

so one of the reasons why windows is so good is that I can name my files in CJK
languages... I can also play my mp3s in foobar2000 viewing CJK text

In the process of finding how I can name my files in different languages and
see them correctly instead of ?'s, I came across XCIN.

You can use XCIN to type chinese under linux.
It has many input methods and since it's developed by tw ppl... (I think, since
xcin.linux.org.tw is the site) It has "phone" input :)

I don't know pinyin or anything
ㄅㄆㄇ is what I know :D

I knew I want cjk and nls flags at the start already, so I compiled everything
(glibc, etc) w/ these flags enabled in gentoo... all I had to do was emerge
xcin and set some environment variables.

setenv LC_CTYPE zh_TW.big5
that's right, I use tcsh, not bash... still a FreeBSD fan at heart
and that's it! I can do shift + space to get a nice little input window pop up

oh! actually you gotta start xcin& when startx as well...

better yet! you can have zh_TW.UTF-8 locale if you download firefly patch!
I got it off taiwan gentoo forum :D go taiwan!
[http://forums.gentoo.org.tw][1]

  [1]: http://forums.gentoo.org.tw
