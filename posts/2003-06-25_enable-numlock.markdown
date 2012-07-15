---
title: enable numlock
---
In order to have numlock enabled automatically after your window manager loads,
follow the steps below. This works for any WM, even though KDE provides this
functionality already.

1. save the code below as numlock.c

~~~
#include 
#include

int main(void)
{
    Display* disp = XOpenDisplay(NULL);
    if (disp == NULL) return 1;
    XTestFakeKeyEvent(disp, XKeysymToKeycode(disp, XK_Num_Lock), True, CurrentTime);
    XTestFakeKeyEvent(disp, XKeysymToKeycode(disp, XK_Num_Lock), False, CurrentTime);
    XCloseDisplay(disp);
    return 0;
}
~~~

2. compile it

~~~
gcc -I/usr/X11R6/include -L/usr/X11R6/lib -o setnumlock numlock.c -lX11 -lXtst
~~~

3. you can then move the resulting file “setnumlock” to one of the paths in your
   PATH env. variable

4. edit .xinitrc or .xsession and add a command for setnumlock
