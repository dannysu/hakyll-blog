---
date: 2011-08-28 02:05:27
title: Project Euler Problem 1
tags: Mad Coding, CPython, Project Euler, Python
---
Found out about [Project Euler](http://www.projecteuler.net) from my friend Shu
Wu's tweet. I figured I'll also use it to help me learn Python and have fun
solving problems. One of the cool things is that they give you an image to show
off your progress. At the time of writing, I've solved 10 problems so far.

![](//projecteuler.net/profile/dannysu.png)

The first problem is: [Add all the natural numbers below one thousand that are
multiples of 3 or 5.][1]

Below is my initial naive solution which ran fast enough for the small (999)
limit. However, there are more efficient solutions.

```python
import time

target = 999

start = time.time()
sum = 0

for i in range(3, target + 1, 3):
    if i % 5 != 0:
        sum += i

for i in range(5, target + 1, 5):
    sum += i

end = time.time()
print sum, "took:", end - start
```

Looking at my friend [Shu's solution on GitHub][2] exposed me to two things:
	
  1. using set() to eliminate duplicates
  1. combining two lists, as returned by range(), together using + operator

Using the set class makes his solution slightly faster than mine. If I'm not
mistaken, a quick browse through the [CPython set class source code][3] reveals
that it uses a dictionary underneath, and the dictionary is [implemented in C
using hashtable][4].  So by using set(), one could eliminate the more expensive
% operations done in the loop.

The fastest method (by orders of magnitude) which is shown on the problem
overview on Project Euler requires one to make further observations about the
problem to spot patterns. Always a good thing to do before jumping into the
naive approach.

My naive solution: 0.000167846679688 seconds  
Same naive solution with % operation taken out: 0.000125169754028 seconds  
Shu's solution: 0.0001540184021 seconds  
Project Euler Overview solution: 4.05311584473e-06 seconds

  [1]: http://projecteuler.net/index.php?section=problems&id=1
  [2]: https://github.com/shuwu83/ProjectEulerSolutions/blob/master/p1.py
  [3]: http://hg.python.org/cpython/file/0072a98566c7/Lib/sets.py
  [4]: http://hg.python.org/cpython/file/0072a98566c7/Objects/dictobject.c
