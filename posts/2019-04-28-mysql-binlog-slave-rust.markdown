---
date: 2019-04-28 12:39:03 PDT
title: MySQL binlog slave in Rust
tags: MySQL, binlog, Rust, rustlang
description: MySQL binlog slave in Rust
---
One of the things that was a bit magical to me was how various systems
downstream of the database got their data. Things like indexing, caching,
ElasticSearch, or other data pipelines were a bit of a blackbox to me.

At my previous startup, we used ElasticSearch to power the bank selection
autocomplete feature. However, because the data rarely changes, we could
rebuild the ElasticSearch index every time. If the data changes a lot or the
data is larger, then rebuilding each time wouldn't scale.

After digging a little bit, I decided to implement code to connect to MySQL as
a slave and synchronize binlog from it. Preliminary search showed me that
various systems at many companies are built on binlog. I also decided to use
this opportunity to learn a little bit of Rust. The result is my fork of
rust-mysql-simple [here][1].

## Connect to MySQL as Slave

The [rust-mysql-simple][2] project already has logic for connecting to a MySQL
database. What I needed to do is to implement the [COM_REGISTER_SLAVE][3] and
[COM_BINLOG_DUMP][4] MySQL commands.

I added `register_as_slave` and `request_binlog_stream` functions by following
the MySQL documentation for what bytes to send. I also used [go-mysql][5] as
reference. Apparently I should send bytes in little endian order.

Implementing the functionality isn't too difficult, but I did get tripped up by
[Rust's ownership model][6] for a bit. Once I got past that though, I can see
how Rust helps prevent memory problems. When I used to work in C++, memory
ownership was always something to be very careful about. Especially if you were
working in a multithreaded environment.

I didn't implement handling everything MySQL sends in binlog for replication,
but what I've implemented allowed me to see how these systems can be connected
together. This was my main objective.

## Facebook Wormhole

Connecting my knowledge now back to my experience at Facebook, I see that
internally we have a system called [Wormhole][7] that acts as a pub/sub system
to distribute DB changes to other systems.

It's not trivial to build, but the overall concept makes sense once you
understand how the "magical" stuff roughly works.

## Other Systems in the Industry

While I was researching, I came across several other similar pipelines in the
industry. For example, Uber's [Schemaless storage system][8], LinkedIn's
[Databus][9], and Alibaba's [Canal][10].

  [1]: https://github.com/dannysu/rust-mysql-simple/tree/binlogsync
  [2]: https://github.com/blackbeam/rust-mysql-simple
  [3]: https://dev.mysql.com/doc/internals/en/com-register-slave.html
  [4]: https://dev.mysql.com/doc/internals/en/com-binlog-dump.html
  [5]: https://github.com/siddontang/go-mysql/
  [6]: https://doc.rust-lang.org/book/ch04-00-understanding-ownership.html
  [7]: https://www.facebook.com/notes/facebook-engineering/wormhole-pubsub-system-moving-data-through-space-and-time/10151504075843920
  [8]: https://eng.uber.com/schemaless-part-one/
  [9]: https://github.com/linkedin/databus
  [10]: https://github.com/alibaba/canal
