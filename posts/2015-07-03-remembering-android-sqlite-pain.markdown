---
date: 2015-07-03 15:32:27 PDT
title: Making Android SQLite Less Painful to Use
tags: Mad Coding, Google, Android, SQLite, Java, AvidTap, AvidRegister, SQLiteFutures
---
2 years ago (2013) when we were first working on AvidRegister, our Android
Point-of-Sale, we used SQLite to store customer purchases and other data locally
on the tablet.

[![][4]][4]

We had to solve some issues with using SQLite that left me surprised things are
still this bad in 2013. I extracted the helper classes I wrote back then and
published it as [SQLiteFutures][3] on GitHub. At some point I'll figure out how
to throw it up on Maven. With this post, I'll look back at some of the things
we had to do at the time that prompted me to write SQLiteFutures.


# Enforce Single SQLiteOpenHelper

AvidRegister stores purchases locally on the device and then synchronizes with
the server. With this design the Point-of-Sale is operational even if Internet
is not working. However, doing so means that cashier might be interacting with
the screen while the syncing service in the background is doing something.

After the product has been out used in actual stores for a while, we were
receiving multiple crash reports noting `SQLiteDatabaseLockedException` had
occurred.

After some research Geoff realized that we should be using a singleton pattern
for our **SQLiteOpenHelper** subclass. In SQLiteFutures, this is enforced
because SQLiteFutures is a singleton and accepts a SQLiteOpenHelper at
initialization.


# DB Access on the UI Thread

Initially we did all our DB queries on the UI thread and a background thread
for syncing. We ran into perf issues if cashier is trying to ring up a sale
while the device is trying to sync with our server.

SQLiteFutures solves this by always executing DB queries in a background thread.
It'll return a FutureTask back so that if you do decide to do things
synchronously you can enforce that easily. However, the default behaviour is
that things are asynchrous. Doing so makes it easy to not block UI thread by
default.


# Remember to Close Cursor

While not something that SQLiteFutures solves, but is worth mentioning as
something we ran into. We had too many mistakes not remembering to close Cursor
instances.

At the time I looked to see if there's something like the C# [using
statement][1] and IDisposable. Unfortunately there was none at the time.
Although Java 7 added [try-with-resource][2], you couldn't use it with Android.
However, nowadays it seems to be supported on SDK version 19+ according to
comments on StackOverflow.

I haven't tried it yet, but this kind of syntax would be much nicer:

```java
try (Cursor cursor = database.query(...)) {
    return cursor.getInt(0);
}
```

  [1]: https://msdn.microsoft.com/en-us/library/yh598w02.aspx
  [2]: http://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html
  [3]: https://github.com/dannysu/sqlitefutures
  [4]: //imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg4LEgVpbWFnZRj58-oDDA
