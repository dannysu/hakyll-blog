---
date: 2003-06-25 18:06:57
title: enable numlock
tags: FreeBSD
---
In order to have numlock enabled automatically after your window manager loads,
follow the steps below. This works for any WM, even though KDE provides this
functionality already.

1. save the code below as numlock.c

<pre class="brush:c">
#include  
#include  

int main(void) 
{ 
    Display* disp = XOpenDisplay(NULL); 
    if (disp == NULL) return 1; 
    XTestFakeKeyEvent(disp, XKeysymToKeycode(disp, XK_Num_Lock), True, 
    CurrentTime); 
    XTestFakeKeyEvent(disp, XKeysymToKeycode(disp, XK_Num_Lock), False, 
    CurrentTime ); 
    XCloseDisplay(disp); 
    return 0; 
}
</pre>

1. Build it:

<pre class="brush:bash">
gcc -I/usr/X11R6/include -L/usr/X11R6/lib -o setnumlock numlock.c -lX11 -lXtst
</pre>

1. you can then move the resulting file "setnumlock" to one of the paths in
   your PATH env. variable

1. edit .xinitrc or .xsession and add a command for setnumlock
