---
date: 2012-07-02 20:38:06
title: Pinterest-like interface for Encyclopedia of Life
tags: Mad Coding, endless scroll, eol, infinite scroll, JQuery, knockout.js, pinterest, responsive design, twitter bootstrap
---
**UPDATE 2:** Source code on [github][2].

**UPDATE:** Also see how I [optimized the memory usage of this][1].

I found an awesome site couple weeks ago that allows people to check out living
things on Earth and collect ones they like: [Encyclopedia of
Life](http://eol.org). It even has "collections" feature where you can put
anything you find on the site into your own collection. You can also copy
things you find in other people's collection. Very, very cool!

[![](/images/eol.org_-150x150.png)](/images/eol.org_.png)

Browsing around the website, I found that EOL has paginated interface for
viewing their 1.5 million (and growing) images. I'm disappointed by the lack of
better interface for me to consume the vast number of images. Pagination was so
6 or 7 years ago. Therefore, I went ahead and built a better one.

Checkout my Pinterest-like infinite-scrolling interface for browsing EOL images
at [http://eol.dannysu.com](http://eol.dannysu.com).

[![](/images/eol.pinterest-150x150.png)](/images/eol.pinterest.png)
[![](/images/responsive-150x150.png)](/images/responsive.png)
[![](/images/eol.modal_-150x150.png)](/images/eol.modal_.png)

<small>(All photos in the screenshots are from EOL. In particular, the awesome
looking bird in the last screenshot is from [Almir Cândido de
Almeida](http://www.flickr.com/photos/almircandido/4744381560/) whose photo is
on EOL.)</small>

Built using [knockout.js](http://knockoutjs.com/), [Twitter
Bootstrap](http://twitter.github.com/bootstrap/), [jQuery](http://jquery.com/),
HTML5 local storage, and some <del>PHP</del> Ruby & Sinatra on server side.
There's infinite scroll as well as responsive design that dynamically re-layout
the cells based on browser width. Note that the collect feature doesn't
integrate with EOL website at the moment. I had implemented it initially, but
the feature is broken on EOL website right now so I went ahead and switched to
storing favourites on my own DB.

I love it! It's awesome to be able to create. Well, now that I'm done creating,
I'm back browsing awesome looking animals!

  [1]: /2012/07/07/infinite-scroll-memory-optimization/
  [2]: https://github.com/dannysu/eol-infinite-scroll
