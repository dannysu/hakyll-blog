---
date: 2015-05-12 21:51:30 PDT
title: Do Translate and Scale Animations at the Same Time
tags: Google, Android, programming, mobile, Mad Coding
---
Here's a neat problem I ran into while trying to animate things on Android. I
wanted to animate moving a View from one place to another. On top of that, I
also wanted to change the size of the View by scaling it.

# Animation Involving Just Translation (Moving)

[![][1]][1]

A pure translation animation like the image above is simple. To know what values
to feed to .translateX() and .translateY() you simply take delta for x
coordinates and the delta for y coordinates.

```java
myView.animate()
    .translateX(x2 - x1)
    .translateY(y2 - y1)
```


# First Attempt at Combining Translation and Scaling

When you want to move a View and scale it at the same time, then you might find
that the result is not what you want if your first attempt is the same as my
first attempt below:

```java
myView.animate()
    .scaleX(w2 / w1)
    .scaleY(h2 / h1)
    .translateX(x2 - x1)
    .translateY(y2 - y1)
```

The reason the View doesn't end up at the correct place that you'd expect is
because the scaling operation shrinks the View at the center of the View. What
you actually get is something like the following:

[![][2]][2]

The light blue box is where the box would end up without scaling. The darker
blue is with scaling. In the above image, the scaling factor is 0.6. i.e. w2 /
w1 = 0.6

The reason the result is as the above image shows is because scaling shrinks or
expands based on the center of the View.


# The Correct Way

So what do you do if you need to animate things to one of the corners? For
example if you want to achieve something like the image below:

[![][3]][3]

What you need to do is to apply additional translation to compensate for the
difference due to scaling. In the above example, I need to adjust x coordinate
by reducing it by `w1 * (1 - scaling factor) / 2`.

`w1` is the View's original width. Since the new width is `w1 * scaling factor`,
the light blue region on both sides combined is `w1 * (1 - scaling factor)`. In
my example, that's `w1 * 0.4`. However, since scaling is done with respect to
the center, on either side the adjustment is just half. So you adjust the x
coordinate by `w1 * (1 - scaling factor) / 2`.

That means the code would be something like this:

```java
myView.animate()
    .scaleX(w2 / w1)
    .scaleY(h2 / h1)
    .translateX(x2 - x1 - w1 * (1 - (w2 / w1)) / 2)
    .translateY(y2 - y1 - h1 * (1 - (h2 / h1)) / 2)
```

  [1]: https://media.dannysu.com/scale.translate1.png
  [2]: https://media.dannysu.com/scale.translate2.png
  [3]: https://media.dannysu.com/scale.translate3.png
