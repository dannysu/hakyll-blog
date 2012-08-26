---
date: 2012-08-26 07:11:10
title: iOS Smooth Infinite Scroll
tags: Apple, Mad Coding, iOS
---
When implementing infinite scroll, you want to fetch more data before user
reaches the end of a list and also update the display to show the new cells. In
iOS there are different [Run Loop Modes][1] which will affect the resulting user
experience. `NSDefaultRunLoopMode` is the default mode for most operations.
However, this run loop mode doesn't allow operations to execute while user is
interacting with the application. On the other hand `NSRunLoopCommonModes`
allows operations to execute while user interaction is happening. The problem
with using `NSRunLoopCommonModes` in infinite scroll is the same as problems in
dealing with event loops. If you perform operation that takes a while in event
loops then other events won't be process until the first operation is done. What
this means for user interaction is that user will sense unresponsiveness in the
UI if there are long running operations. The way to solve that is the same as
event loops.

When dealing with event loops in the main thread, you typically will delegate
long running operations to a separate thread so that messages don't stop getting
processed. I have found that when trying to implement infinite scroll, you
should use `NSRunLoopCommonModes` but make sure to only do small updates each
time to ensure user interaction remains responsive. For my application,
performing small updates such as adding one more cell to UITableView is
sufficient to ensure user interaction doesn't get interrupted. Once you get the
event that user interaction and animation have stopped, then a complete update
to flush every pending update can be done. Doing small update plus complete
update when interaction is done allows you to implement very smooth infinite
scroll that doesn't ever show spinners if the network and your server can keep
up providing data. i.e. It's a true infinite scroll.

  [1]: http://developer.apple.com/library/ios/#documentation/Cocoa/Conceptual/Multithreading/RunLoopManagement/RunLoopManagement.html
