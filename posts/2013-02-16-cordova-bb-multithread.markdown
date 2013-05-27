---
date: 2013-02-16 03:37:44
title: Multithread Issues in BB7 Cordova
tags: Mad Coding, PhoneGap, Cordova, BlackBerry, Multithread
---
I've come across several instances where developers don't really understand how
to do multithread programming properly. While it's fairly easy to put together a
mobile app with prototype quality that looks the part, if things are not done
properly you can get into some pretty nasty stability issues. My post on
[reference counting on iOS][1] is an attempt to share some of that knowledge.
Well.ca's iOS app suffered random crashes due to memory leak in its early days
until I fixed it. This post will examine BlackBerry 7's Cordova implementation
and some of the issues I came across.

## **The Problem**

While using Cordova to build BB7 app, I was able to get the app functionally
working but I kept having stability issues. The app would use the file system
and save files to it. However, once in a while I would find that the directory
I'm saving to becomes locked and cannot be deleted even though I'm no longer
using it. The directory remains locked until I pull the battery and reboot.

## **Multithread Issue**

The symptom points to resources not being properly freed up. At first I didn't
know what's wrong and blamed the issue on the crappy BlackBerry OS. However, as
I examined Cordova source code, it became clear that the root cause is due to
Cordova not freeing things properly in multithread situation.

First, the way Cordova on BB7 execute plugin functions is by creating a new
thread to do the work in, and later call back to javascript engine with
results. You can see this in the [PluginManagerFunction.java][2] file inside
invoke():

<pre class="brush:c">
if (async) {
    // Run this async on a background thread so that JavaScript can continue on
    Thread thread = new Thread(new Runnable() {
        public void run() {
            // Cordova does stuff in native code
            // Calls plugin's execute()
        }
    }
    thread.start();
}
</pre>

This should be similar to how Node.js works. It's restricted in that the
javascript engine must be single-threaded and you need to do
serialization/deserialization to communicate. There's no problem with this, the
real issue arises due to Cordova plugin's lack of proper cleanup.

For example, the [FileTransfer plugin][3] will perform download() inside the
newly created thread and open a bunch of file handles:

<pre class="brush:c">
private PluginResult download(String source, String target) {
    HttpConnection httpConn = null;
    FileConnection fileConn = null;

    // Do stuff

    try {

    }
    catch (Throwable t) {
        // ...
    }
    finally {
        if (httpConn != null) {
            httpConn.close();
        }
        if (outputStream != null) {
            outputStream.close();
        }
        if (fileConn != null) {
            fileConn.close();
        }
    }
}
</pre>

This might seem right by itself. Any opened file handle should be freed up in the
finally clause. However, one needs to remember that this download() function is
invoked by plugin's execute() function, which is invoked inside the newly
created thread.

Below is Cordova's application termination sequence:

<pre class="brush:c">
  exitApp:function() {
      // Call onunload if it is defined since BlackBerry does not invoke
      // on application exit.
      if (typeof window.onunload === "function") {
          window.onunload();
      }

      // allow Cordova JavaScript Extension opportunity to cleanup
      manager.destroy();

      // exit the app
      blackberry.app.exit();
  }
</pre>

The manager.destroy() call will go through each plugin and allow the plugin to
finish what it needs to do. However, the plugins don't properly clean up after
themselves in this situation. The destroy() call doesn't wait for any
in-progress threads to exit or finish, the threads get interrupted and the
finally clause becomes useless thus leading to intermittent issue with file
locking.

## **Fixing It**

I'm unfortunately not as familiar with Java's way of doing things, but in C++
and Win32 world you typically will solve this issue by using events and signal
the thread to exit early. My attempt to fix the issue in Cordova is by checking
isAlive() on the created threads and "signal" them to exit cleanly by using a
volatile boolean.

Here's a [link][5] that explains the volatile keyword usage. The page has an
example how signaling a thread to exit early which I replicated below:

<pre class="brush:c">
public class StoppableTask extends Thread {
  private volatile boolean pleaseStop;

  public void run() {
    while (!pleaseStop) {
      // do some stuff...
    }
  }

  public void tellMeToStop() {
    pleaseStop = true;
  }
}
</pre>

The way I fixed the file locking issue is by modifying destroy to do something
like the following:

<pre class="brush:c">
public void destroy() {
    // Set the volatile boolean so that all threads will see this signaling to
    // exit
    pleaseStop = true;
    
    if (thread.isActive()) {
        // Give running threads a chance to exit cleanly
        Thread.sleep(50);
    }
}
</pre>

There's more to the fix than what I showed above, but that's the gist of it.
Whenever you create a new thread, think about the ending condition. Kind of like
whenever you start writing a recursive function, you think about the ending
condition. You can't do without it otherwise you're bound to hit random issues
that are hard to reproduce because of timing.

  [1]: /2012/07/30/automatic-reference-counting-on-ios/
  [2]: https://github.com/apache/cordova-blackberry/blob/master/framework/ext/src/org/apache/cordova/api/PluginManagerFunction.java
  [3]: https://github.com/apache/cordova-blackberry/blob/master/framework/ext/src/org/apache/cordova/http/FileTransfer.java
  [4]: https://github.com/apache/cordova-blackberry/blob/master/javascript/cordova.blackberry.js
  [5]: http://www.javamex.com/tutorials/synchronization_volatile_typical_use.shtml
