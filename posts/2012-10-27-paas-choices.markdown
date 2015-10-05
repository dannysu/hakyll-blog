---
date: 2012-10-27 23:12:00
title: Excellent Selection of PaaS Choices
tags: Mad Coding, AppFog, Heroku, Google App Engine, GAE, CloudBees, Azure
---
DreamHost has been my web host for the longest time, but it's time to change.
Nowadays we have many PaaS choices or IaaS, so that in my situation DreamHost
doesn't make sense anymore. Next year I plan to move my monthly spending on
DreamHost to some of the choices mentioned below.

PaaS also allows me to quickly get a service running for my startup without
dealing with installations.

Note that the pros & cons listed are from the perspective of my usage level.
YMMV.
<br>
<br>

## **Hosting Static Content**

I was using wordpress which requires PHP and MySQL, but my blog is not dynamic
in nature so things like Jekyll and Hakyll really made sense for me. There are
many ways you can host static content for free or at a low cost.

Currently my blog is hosted with GitHub Pages which is free. There's also
[NearlyFreeSpeech.net][2] that costs very little. Also, many of the PaaS
providers are great for static blogs too. I got my blog on AppFog very easily
and I've read about people using Heroku as well. More on those PaaS providers
later.
<br>
<br>

## **DreamHost Pros and Cons**

Using DreamHost still makes sense in some situations. I'm using
[Amazon Route 53][3] for my DNS and that costs $0.51/month for 1 domain.
With DreamHost you can have unlimited domains without paying extra. Below are
the pros and cons for my usage.

Pros:

- Lots of storage ("unlimited")
- 50GB cloud backup included
- Unlimited number of domains
- "unlimited" bandwidth
- cron
- Filesystem access
- MySQL
- ssh
- subversion

Cons:

- Restricted choice of software
- Costs $7.95/month

I'm using 13GB of my "unlimited" storage mainly due to my old online photo
album. DreamHost also gives you 50GB backup storage space. You can use duplicity
for encrypted offsite backup with it, so that's pretty sweet. You also get cron,
filesystem access, and mysql. PaaS providers sometimes don't give you all of
that.

If your app is storage heavy, DreamHost is still the better choice. Where it
falls short for me is that for $8 a month I cannot use the latest and greatest
software. To use Ruby and Sinatra I had to mess around only to have an old version
working. Similar situation for Rails and Python I believe. If I want to run
Node.js or Clojure I also have to tinker with it somehow.

Since I already moved my photo storage to SkyDrive (25GB) and all of my projects
elsewhere, it's time to say goodbye to DreamHost.
<br>
<br>

## **Windows Azure**

I only played with Azure briefly. I was still at Microsoft when [Azure
Sandbox][15] was introduced internally on the **The Garage** email list. Before
then there's no way to run your app for free forever. It seems that it's still
that way with the 90-day trial.

Azure Sandbox is a great idea, and I used it for my [daily Bing wallpaper
app][4]. However, I quickly found out that with the quota they gave me
I can't even keep the service running 24/7. I'm not going to pay for a hobby
project, so I ended up going with Google App Engine.
<br>
<br>

## **Google App Engine**

Google App Engine is pretty good. Its [free quota per app per day][5] is great
for personal projects.

Pros:

- Plenty of storage space between Datastore (1GB) and Blobstore (5GB)
- Supports Go
- cron

Cons:

- Limited to Java, Python, Go
- Have to use Datastore/Blobstore
- No accessible filesystem

Source code for my daily Bing wallpaper service is on [github][5]. It makes use
of GAE cron to fetch new wallpapers daily. I'm only using 2% of the 1GB storage
and 2% of the allocated daily instance hours, so I can keep this running
forever for free.

My [imagedatastore project][7], written in Go, is also hosted on GAE. I'm using it
to host any images I need to display on my blog. It uses Blobstore and Datastore
so I'm not going to run out of storage at all.

If you're interested, [GAE Storage][8] is something I wrote that allows a dead
simple place to throw data to. I have a modified ZeroBin version that stores
data in GAE using this project instead of needing filesystem access.

App Engine allows me to host 3 of my projects for the price of free.
<br>
<br>

## **Heroku**

Heroku is another PaaS provider where I have projects hosted. All of my
projects only need 1 web dyno, so all these projects can be hosted for
free.

Pros:

- Polyglot (I used Python, Clojure, and Node.js)
- Ability to run commands inside a shell
- Push updates via git
- Looking at logs is easy

Cons:

- Instance suspends when not used so first request after idle takes a long while
- Very little space provided (25MB for PostgreSQL)
- No filesystem
- Custom domain costs money now
- No cron inside the same app

In order to have RSS feed containing actual article content, I have a project
that parses a twitter feed and then go fetch the linked websites' content.
Written in Python and uses Readability module to parse data. Source code on
[github][9]. The one downside of Heroku is that I don't have a cron to
periodically update the feed. I use GAE to trigger updates instead. I got a
custom domain set up for this before Heroku started charging for custom domains.

I also have a Clojure project hosted on here, and the prototype Node.js service
for the startup goes here as well. Both very easy to update with a simple git push.
I like that I can run `heroku run bash` and run helper scripts for these
prototypes.

Again, 3 projects for the price of free. Plus I get to use Clojure and Node.js.
Can't beat that. I think getting Haskell going is a bit more involved though,
somebody please make it easier for Haskell.
<br>
<br>

## **AppFog**

Heroku charging for custom domain drove me to check out AppFog that I signed up
a while ago and never used. After playing with it I really like it! I moved my
[EOL pinterest interface][10] to AppFog now.

Pros:

- Custom domain
- Direct file system access! 500MB!
- Up to 2GB memory
- Ruby, Python, PHP, Java, Node.js, Erlang

Cons:

- Have to push the entire folder each time, not source control intelligence 
- Looking at logs is a strange experience

The one lingering project that I still had on DreamHost was the EOL project. The
eol.org server is unfortunately not very fast. Also, in order to not put much
load on their server, I cache all the results I get from them for 24 hours to
avoid re-fetch. I'm using disk caching so I wouldn't be able to move to GAE or
Heroku without rewriting the caching part. But AppFog to the rescue! You get
actual filesystem access so I got EOL project up and running in no time!

Another quick project I used AppFog for is my blog. GitHub was down recently and
brought down my blog as well. I quickly put up a Node.js server on AppFog to
serve my blog content. It's dead easy with node-static. See source code on
[github][11].

I love AppFog for all these things I was able to do. One downside for me so
far is that every time I update my app I have to upload the whole thing again.
Also, for some reason the command line tool to view logs didn't work
reliably for me.

So that's 1 project on AppFog plus my backup blog for the price of free. Awesome!
<br>
<br>

## **CloudBees**

Pros:

- Jenkins as a Service!

Cons:

- Limited to Java
- None yet (Not enough experience with it)

I found CloudBees while searching for hosted build service. I put up the
[clj-philosopher][12] code on there and used Jenkins to automate running the
tests. CloudBees is unique in this aspect of providing a Jenkins service. I have
not used their Java offering.
<br>
<br>

## **The New World**

When I first played with Node.js I used my EC2 free tier and that was pretty
easy. However, with all these PaaS choices it's even easier to get things up and
running.

The best choices I found for storing stuff is GAE and AppFog. I haven't played
with CouchDB on [cloudant][13] or other NoSQL providers with free tier like
[cassandra.io][14] and [mongohq][16] yet, so that's hopefully to come. Anything
free for Riak or Redis?

Using a combination of different PaaS providers, I now have all of my web facing
projects hosted at places for free. At this time the only cost I have is from
Route 53 at $0.51/month. Once the period I already paid at DreamHost ends I'll
be looking to spend the money elsewhere. Maybe for paying to get more features
at some of the providers.

  [1]: http://haskell.org
  [2]: https://www.nearlyfreespeech.net
  [3]: http://aws.amazon.com/route53/
  [4]: /2011/08/22/bing-mobile-wallpaper/
  [5]: https://cloud.google.com/pricing/
  [6]: https://github.com/dannysu/bingwp7
  [7]: https://github.com/dannysu/imagedatastore 
  [8]: https://github.com/dannysu/gae_storage
  [9]: https://github.com/dannysu/hackernews-feed
  [10]: /2012/07/02/pinterest-like-interface-for-encyclopedia-of-life/
  [11]: https://github.com/dannysu/appfog-blog
  [12]: https://github.com/dannysu/clj-philosopher
  [13]: https://cloudant.com
  [14]: http://cassandra.io
  [15]: http://www.microsoft.com/en-us/download/details.aspx?id=26206
  [16]: https://www.mongohq.com
