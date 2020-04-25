---
date: 2016-12-29 20:33:25 PST
title: Using Node-RED for RSS feeds
tags: Node-RED, Huginn, IFTTT, Zapier, Flow, RaspberryPi, RPi
description: Using Node-RED to filter, transform and produce RSS feeds
---
[Huginn][3] is an open-source software similar to [Yahoo Pipes][14], [IFTTT][4],
[Zapier][5], or [Microsoft Flow][2]. [Node-RED][1] is another similar
open-source software but it's aimed more at IoT applications.

I was using Huginn for about a year for the purpose of ingesting data and
outputting RSS feeds. This week I swapped out Huginn and started using Node-RED
instead. So far it's working out well and I'm loving the new setup. This post
will highlight the nodes I coded for Node-RED to work it for my purpose, but
first a little bit of background.

<br/><a name="toc"></a>

**Table of Contents**

 - <a href="#why">Why Huginn or Node-RED?</a>
 - <a href="#reasons">Reasons for Replacing Huginn</a>
 - <a href="#usage">Using Node-RED</a>
     - <a href="#rss">Outputting RSS</a>
     - <a href="#filter">Better Filtering</a>
 - <a href="#rpi3">Node-RED on Raspberry Pi 3</a>

<br/><a name="why"></a>

# Why Huginn or Node-RED?

Almost every news update I read is through RSS feeds synchronized to my phone.
RSS allows me to have a superior reading experience, and the synchronization
feature of the app on my phone allows me to read offline during my commute.

Currently the service I use for synchronization is [BazQux][11]. Other options
I've used in the past include [Newsblur][10], and self-host options such as
[Miniflux][12] and [Coldsweat][13]. Once my BazQux subscription runs out, I'm
probably going to investigate self-host options again.

In order to read everything via RSS feeds, either the news source needs to
provide the RSS feed or I need to produce the RSS feed myself. For example,
Twitter doesn't provide a RSS feed, so I wrote some code that runs on AppEngine
and converts tweets into a RSS feed. It's a much better way to read Twitter in
my opinion. I also apply additional filters on top of the Twitter feed, which
is where Huginn and Node-RED comes in.

Huginn and Node-RED allows me to automate the filtering process and automate
the steps needed to take a data source that doesn't have RSS feed and produce
one. Another example of where I do this is the [Asuswrt-Merlin Changelog][8].
The changelog is just a blob of text. I use Huginn and Node-RED to periodically
retrieve the text and split it up into RSS feed entries.

<br/><a name="reasons"></a>

# Reasons for Replacing Huginn

I don't want to take anything away from Huginn. It's an awesome open-source
software and I love it. That said, its main downside for me is its [hefty
requirements][6] to run it well. The 2GB recommended configuration is more
expensive than I'd like.

Initially I used a $30/year OpenVZ VM that I found through LowEndTalk. It
didn't have enough resources to run Huginn well. I then paid for a $65/year
OpenVZ VM that had 6GB RAM and that's what I ended up using for Huginn for the
past year. For comparison, a Droplet with 2GB RAM on DigitalOcean would cost
$240/year. Even though DigitalOcean uses KVM, that's just way too much for my
taste.

In contrast Node-RED can run on a Raspberry Pi with less than 2GB RAM. I
learned about Node-RED a little while ago from [a Hacker News thread][9], and
it's been on my target ever since.

One other advantage for Node-RED is its better debuggability. I can hook up
debug nodes to examine messages flowing through each part of the pipeline more
easily.

<br/><a name="usage"></a>

# Using Node-RED

The problem with Node-RED for my usage is that it's mainly aimed at IoT
applications. Huginn felt more tailored for my purpose of transforming and
producing RSS feeds.

In the past year, I worked on implementing the missing pieces needed for
Node-RED to service my needs.

<br/><a name="rss"></a>

## Outputting RSS

The most critical deficiency of Node-RED is the inability to produce RSS feeds.
Node-RED comes with the *feedparser* node that can consume RSS feeds, but there
isn't anything to produce a RSS feed. I wrote [node-red-contrib-rss][16] in
order to rectify this gap. You can install it with `npm install
node-red-contrib-rss` in your `~/.node-red` directory.

<br/>

Below are the nodes I use to produce the Asuswrt changelog RSS feed:

[![][17]][17]

<table><tr><td><small>The timestamp node periodically kicks off a HTTP request to fetch the
changelog. The *pre* and *parse* nodes then transforms the data into individual
entries. Finally the *node-red-contrib-rss* node produces the RSS feed and then
the file node saves to disk.</small></td></tr></table>

<br/>

The node-red-contrib-rss configuration looks like the screenshot below:

[![][18]][18]

<table><tr><td><small>You basically configure the metadata for the RSS feed, and define how
properties of the incoming messages in the pipeline maps to RSS
entries. The node expects the incoming *msg.payload* to be an array of
objects.</small></td></tr></table>

<br/><a name="filter"></a>

## Better Filtering

The built-in node used for filtering messages in the pipeline is rather
primitive. I wanted the ability to deal with incoming message payload being an
Array, but the built-in Switch node can't do that. The
[node-red-contrib-filter][19] was created for this need. I also added the ability
to direct messages to an arbitrary number of outputs. My filter node is
powerful and does everything the built-in one can do plus more.

<br/>

Below is the configuration for filtering Twitter messages:

[![][20]][20]

<table><tr><td><small>This flow periodically retrieves a Twitter feed, then uses the filtering
node to dump stuff I don't care about, and finally produce the resulting RSS
feed.</small></td></tr></table>

<br/>

The configuration for the filter node:

[![][21]][21]

<table><tr><td><small>Showing an example of filtering out DeviantArt's membership nag. I don't
need to see that on Twitter. I already pay for points or pay for art on a need-to
basis, and I don't browse DeviantArt enough to care about the Core Membership
perks.</small></td></tr></table>

<br/><a name="rpi3"></a>

# Node-RED on Raspberry Pi 3

Node-RED comes bundled with Raspberry Pi, and I've always liked the idea of
having a Raspberry Pi at home to do whatever I want it to do. I bought a RPi 3
from [MCM Electronics][7] for $35 + tax + $7.99 shipping.

I'm currently using only 170MB of RAM on the RPi, so you can see that
Node-RED's requirements isn't high at all. I could have used the $30/year
cheapo VM with Node-RED too, but the RPi setup is roughly the same cost and
will save money as the time goes on. It also has the advantage of sitting in a
trusted environment (my home) rather than some OpenVZ VM by some random host
company. Should I decide to store more sensitive data in the future, the RPi
setup is much better.

  [1]: http://nodered.org
  [2]: https://flow.microsoft.com
  [3]: https://github.com/cantino/huginn/
  [4]: https://ifttt.com
  [5]: https://zapier.com
  [6]: https://github.com/cantino/huginn/blob/master/doc/manual/requirements.md
  [7]: http://www.mcmelectronics.com
  [8]: http://asuswrt.lostrealm.ca/changelog
  [9]: https://news.ycombinator.com/item?id=11015147
  [10]: https://www.newsblur.com/
  [11]: https://bazqux.com/
  [12]: https://miniflux.net/
  [13]: https://github.com/passiomatic/coldsweat
  [14]: https://en.wikipedia.org/wiki/Yahoo!_Pipes
  [15]: http://placeholder
  [16]: https://github.com/dannysu/node-red-contrib-rss
  [17]: https://media.dannysu.com/nodered.asuswrt.png
  [18]: https://media.dannysu.com/nodered.feed.options.png
  [19]: https://github.com/dannysu/node-red-contrib-filter
  [20]: https://media.dannysu.com/nodered.twitter.png
  [21]: https://media.dannysu.com/nodered.filter.node.png
