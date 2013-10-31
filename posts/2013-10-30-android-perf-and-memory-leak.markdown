---
date: 
title: Fixing AvidRegister Performance and Memory Leak Issues
tags: Start-up, AvidTap, HyperDrive, Android, AvidRegister
---
Thanks to my co-founder Geoff, we just released a new version of
[AvidRegister][1] to the [Play Store][2]. Geoff fixed a performance issue that
was slowing down our app, and he also fixed a memory leak issue we've been
getting OOM crash reports on. The result is that the app is super snappy.
Feedbacks from our customer have been that they love everything except the speed
of the app. This fixes it and now our app is incredible!

<br>

## **Performance of Mobile Apps**

We've been cranking out feature after feature such that we hadn't spent the time
to make sure our app is as snappy as it could be. One of the earlier
conversations I had with my co-founders was whether moving up to dual-core or
quad-core tablets would speed things up. At the time I insisted that just
because a device has more cores doesn't mean it'll be faster. We needed to have
time to tackle perf.

As Windows Phone users can attest, the OS runs smoother on older hardware than
Android on the same hardware before Google finally fixes things up in Jelly
Bean. I knew from experience that a lot of the performance issue is software
related. That's why sometimes you can see significant improvement between final
verison of a software versus the beta version.

One can think about it this way, the mobile app works by having a main UI thread
that processes the main message queue and does most things. Adding more CPU
cores to the mix doesn't give you the perf gain you might think because it's
still a single UI thread design. We could even draw parallels to node.js where
there is a single thread and you better not block things. Adding more CPU cores
to the server doesn't automatically get faster as those cores are not used.

Certainly if each CPU core is faster by itself, then it'll help. Also, if you
could offload things to do in a separate thread than the UI thread, that'll also
help.

<br>

## **Profiling in Android**

The performance issue in our case was that a particular code path is noticeably
slower. Geoff added tracing as documented on [Android developer website][3].
Basically he added `Debug.startMethodTracing("label");` at the beginning of a
click handler, and then added a `Debug.stopMethodTracing();` once the processing
is done.

Afterwards, `traceview` was used to analyze where the time was spent and Geoff
noticed that a significant amount of time was spent on our grid view that was
unexpected. Upon removing the code that was doing excessive updating to the grid
view, everything went super snappy. Our slowest single-core tablet now runs the
app flawlessly and confirms that it's not the hardware!

<br>

## **Investigating Memory Leak in Android**

For our first release of AvidRegister I squeezed in [ACRA][4] for sending out
crash reports. This has proven to be very valuable. It's one of those things
that are not a deal breaker if we miss it, but if somebody just suck it up and
get it done then everyone benefits.

We've seen some OOM crash reports from one of our stores recently, and we had to
investigate what's going on. Did we introduce something in one of the releases?
With garbage collection in Java, how are we running out of memory? It's Java so
it's not like circular reference won't be cleaned by the garbage collector.
Well, as Geoff digged into it using the [Memory Analyzer (MAT)][5] it turns out
to be something to do with [Handler][6] holding references.

In one of our Activity, we use Handler as a way to schedule tasks to be done at
certain interval later. However, in all but one case we forgot to
removeCallback() on the Runnable we were using. That was the one which was
causing Handler to keep holding onto the Activity after the Activity's life
cycle has ended. Reading the [Handler][6] documentation, you'll see that the
Handler is a per-thread thing that will live longer than the Activity if the
thread is the UI thread.

The lesson here is to watch out for things that will live on for a while and
make sure they don't hold reference to things you expect to be cleaned up. E.g.
Watch out for static vars, your singletons, and Handler of course. If you can't
perform an explicit cut off of the reference, then perhaps [WeakReference][7] is
what you should look at.

  [1]: http://avidregister.com
  [2]: http://avidregister.com/download
  [3]: http://developer.android.com/tools/debugging/debugging-tracing.html
  [4]: https://github.com/ACRA/acra
  [5]: http://www.eclipse.org/mat/downloads.php
  [6]: https://developer.android.com/reference/android/os/Handler.html
  [7]: https://developer.android.com/reference/java/lang/ref/WeakReference.html
