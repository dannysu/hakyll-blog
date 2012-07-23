---
date: 2004-05-29 09:19:11
title: Genetic Algorithm in action
tags: Linux/Unix/QNX
---

I recently discovered acovea - an optimization tool to test different GCC flags

acovea uses Genetic Algorithm to find a good set of GCC CFLAGS. Why bother?
Well, since there are 62 different optimization flags available in GCC, to test all of them requires 2^62 different combinations.
That is 2^62 = 4611686018427387904!!!  :eek: 

With genetic programming, you won't necessarily find THE BEST combination. However, genetic programming can improve over time just as natural evolution. In the end, you can find a optimal solution to your problem.

my acovea results: [here](/files/gentoo/acovea.tar.bz2)
