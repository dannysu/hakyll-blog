---
date: 2003-10-08 19:20:06
title: My first GTK/GNOME program
tags: FreeBSD
---
I wrote my first program on FreeBSD!!!

I miss ObjectBar on windows... it's a program that allows me to popup a window
showing me all currently running programs and a set of shortcuts that I often
use

this is extrememly powerful and adds to my productivity

I looked around on google to see if GNOME panel can show/hide depending on
keyboard shortcuts. I don't think it can at this stage... unless I modify the
code and compile I guess...

well, with that in mind, I began my first programming project on FreeBSD. My
goal is to write a program using GTK/GNOME libraries to create what I want.

and I did! I started by learning how to use "glade" to create GUI, then I
studied the code to figure out what functions to call. After I have the GUI
part figured out, I need to know how to show currently running programs. This
is already written for GNOME, so I extract the source code, took parts from
window-list.c, and modified them for my needs. I'm in the final stage of
finishing up things... my only concern now is that I'm not very sure about the
memory management... not sure if I need to free space used by GNOME library and
things
