---
date: 2012-02-11 15:42:55
title: Hacker News &gt;100pts feed
tags: Mad Coding
---

Today I wrote a quick parser using [Scrapy](http://scrapy.org/) to grab contents from the Hacker News website for [news having over 100 points](http://news.ycombinator.com/over?points=100). My main motivation for this is so that I can more easily read news while I don't have Internet on my phone. The old RSS feed I was using showed only the title and doesn't have the content of the webpage being discussed. Therefore, I whipped out my Scrapy and Python and coded this up. I also made use of the [readability-lxml package](http://pypi.python.org/pypi/readability-lxml) to strip unnecessary HTML.

You can access the feed via [http://dannysu.com/feeds/hackernews100.atom](http://dannysu.com/feeds/hackernews100.atom)



I also want to give a shout-out to people at Mozilla for the [new developer tools](http://blog.mozilla.com/blog/2012/01/31/firefox-adds-powerful-new-developer-tools/). I just discovered the new way you can inspect HTML elements just by hovering or clicking around this week. Unlike in Chrome where you have to go through the HTML code just to match the code up to what you're seeing visually. Having the new developer tool made scraping and verifying things much easier.



**UPDATE:** Source code added to [my github](https://github.com/dannysu/hackernews-feed)
