---
date: 2007-01-21 21:01:44
title: Native GIMP on OSX
tags: Apple
---
I mentioned earlier about GTK+ running natively on OSX without the use of X11.
I was successful in getting a simple "hello world" application running, but I
didn't get GIMP working. Today I went back and compiled GIMP, fixed a compile
error and there it is! GIMP running natively:

[![](https://imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg4LEgVpbWFnZRj9p7ADDA)](https://imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg4LEgVpbWFnZRj8lbwBDA)

The image showing on the screen is actually a photo I took of Pat Helland's
office at Amazon.

You can see that the dock has no X11 running (Yes, I know that X11 could be
hidden, but just take my word for it that X11 is not running).

There are still some quirks to be fixed, but this paves for a tighter
integration of GTK apps on OSX.

I'm not sure what apps other than GIMP would be useful though. I'd say GAIM if
this was for Windows, but there's Adium already.
