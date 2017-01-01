---
date: 2012-02-11 15:42:55
title: Hacker News &gt;100pts feed
tags: Mad Coding
---
Today I wrote a quick parser using [Scrapy][1] to grab contents from the Hacker
News website for [news having over 100 points][2]. My main motivation for this
is so that I can more easily read news while I don't have Internet on my phone.
The old RSS feed I was using showed only the title and doesn't have the content
of the webpage being discussed. Therefore, I whipped out my Scrapy and Python
and coded this up. I also made use of the [readability-lxml package][3] to
strip unnecessary HTML.

You can access the feed via [http://feeds.dannysu.com/hackernews100.atom][4]

I also want to give a shout-out to people at Mozilla for the [new developer
tools][5]. I just discovered the new way you can inspect HTML elements just by
hovering or clicking around this week. Unlike in Chrome where you have to go
through the HTML code just to match the code up to what you're seeing visually.
Having the new developer tool made scraping and verifying things much easier.

**UPDATE:** Source code added to [my github][6]

  [1]: http://scrapy.org/
  [2]: http://news.ycombinator.com/over?points=100
  [3]: http://pypi.python.org/pypi/readability-lxml
  [4]: http://feeds.dannysu.com/hackernews100.atom
  [5]: http://blog.mozilla.com/blog/2012/01/31/firefox-adds-powerful-new-developer-tools/
  [6]: https://github.com/dannysu/hackernews-feed
