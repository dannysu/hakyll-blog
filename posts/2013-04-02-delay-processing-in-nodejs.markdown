---
date: 2013-04-02 23:39:15
title: Delay Processing In Node.js
tags: Mad Coding, Start-up, AvidTap, Node.js
---
**Scenario**

There have been scenarios where I wanted to offload some processing to be
done in a separate thread. One example is when I want to geocode certain
addresses but don't need the result of the geocoding in order to return results
to the client. There is no point to wait for the geocode request to another
server in order to return results for current request. In this situation it
would have been great to just offload that task, which is also IO-bound.

There are several ways of accomplishing this: `child_process`, `setImmediate`,
`process.nextTick`, and additional modules.

**Update:** Actually, some realization later, for IO-bound tasks you don't
really need to delay using these techniques. Once processing hits IO regular
event loop applies. For CPU-bound tasks `child_process` and `webworkers` will
help.

<br>
**child_process**

There is the built-in [child_process][4] module, which provides ability to spawn
other processes like many other programming languages. This is the solution I
currently use for [AvidTap][5]. It works and allows me to return results without
waiting on non-essential things. However, it's also not pretty and has more
layers to jump through when reading code.

The code roughly looks like this:
<pre class="brush:c">
    var spawn = require('child_process').spawn;
    spawn('node', [path.resolve(__dirname, '../tools/script.js'), 'some args']);
</pre>

Ugly and all of a sudden I have to worry about file paths which have different
semantic compared to when using require(). Enter process.nextTick.

<br>
**process.nextTick**

While looking for another solution, I came across this article on
[howtonode][3], which talked about [process.nextTick][8]. Since the task I want
to delay process also happens to be one that's IO-bound, `nextTick` would work
great.

`nextTick` plays within the rules of the single event loop and single
threaded-ness of javascript. The great thing this has over the `child_process`
method is that I can use closure and get access to the context. The downside is
that I can't do anything CPU intensive here just like everwhere else.

Example:
<pre class="brush:c">
var saywhat = 'hello';
process.nextTick(function() {
    console.log(saywhat);
});
</pre>

`nextTick` makes delaying things easier and nicer to read, and after node v9
there is `setImmediate` too, which should work for IO-bound tasks as well.

<br>
**setImmediate**

[setImmediate][6] is similar to nextTick, but differs in the order it gets
executed in the event loop. The doc explains the details. Using it is similar to
`nextTick`:

<pre class="brush:c">
var saywhat = 'hello';
setImmediate(function(value) {
    console.log(saywhat);
    console.log(value);
}, "world!");
</pre>

In my scenario, it doesn't matter which one. I just want certain IO-bound task
to be performed some time later. There's no recursive call either. If I have
CPU-bound task, then I'll probably explore the webworkers route.

<br>
**WebWorkers for Node.js**

The [webworker-threads][1] module enables you to use [webworker API][2]. This
would work for CPU intensive tasks. The downside of course is that it's
basically a separate environment (so no closure) and you have to communicate via
strings.

<pre class="brush:c">
var Worker = require('webworker-threads').Worker;
var worker = new Worker(function() {
    onmessage = function(event) {
        console.log("hello " + event.data);
        self.close();
    };
});
worker.postMessage("world!");
</pre>

<br>
So these are the ways I know of in terms of delay doing some non-essential
processing in Node.js. Next time I won't be picking up the `child_process` tool
because it's not the best thing for my current needs.

  [1]: https://npmjs.org/package/webworker-threads
  [2]: http://www.whatwg.org/specs/web-apps/current-work/multipage/workers.html
  [3]: http://howtonode.org/understanding-process-next-tick
  [4]: http://nodejs.org/api/child_process.html
  [5]: http://avidtap.com
  [6]: http://nodejs.org/api/timers.html#timers_setimmediate_callback_arg
  [7]: https://github.com/joyent/node/pull/3709
  [8]: http://nodejs.org/api/process.html#process_process_nexttick_callback
