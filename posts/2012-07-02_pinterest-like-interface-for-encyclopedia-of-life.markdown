---
title: Pinterest-like interface for Encyclopedia of Life
tags: Mad Coding, endless scroll, eol, infinite scroll, jQuery, knockout.js, pinterest, responsive design, twitter bootstrap
---
**UPDATE:** Also see how I [optimized the memory usage of this][1].

I found an awesome site couple weeks ago that allows people to check out living
things on Earth and collect ones they like: [Encyclopedia of Life][2]. It even
has "collections" feature where you can put anything you find on the site into
your own collection. You can also copy things you find in other people's
collection. Very, very cool!

<a href="http://www.dannysu.com/wp-content/uploads/2012/07/eol.org_.png"><img class="alignright size-thumbnail wp-image-508" title="eol.org" src="http://www.dannysu.com/wp-content/uploads/2012/07/eol.org_-150x150.png" alt="" width="150" height="150" /></a>Browsing around the website, I found that EOL has paginated interface for viewing their 1.5 million (and growing) images. I'm disappointed by the lack of better interface for me to consume the vast number of images. Pagination was so 6 or 7 years ago. Therefore, I went ahead and built a better one.

Checkout my Pinterest-like infinite-scrolling interface for browsing EOL images at <a href="http://eol.dannysu.com" target="_blank">http://eol.dannysu.com</a>.
<a href="http://www.dannysu.com/wp-content/uploads/2012/07/eol.pinterest.png"><img class="alignnone size-thumbnail wp-image-509" title="eol.pinterest" src="http://www.dannysu.com/wp-content/uploads/2012/07/eol.pinterest-150x150.png" alt="" width="150" height="150" /></a> <a href="http://www.dannysu.com/wp-content/uploads/2012/07/responsive.png"><img class="alignnone size-thumbnail wp-image-510" title="responsive" src="http://www.dannysu.com/wp-content/uploads/2012/07/responsive-150x150.png" alt="" width="150" height="150" /></a> <a href="http://www.dannysu.com/wp-content/uploads/2012/07/eol.modal_.png"><img class="alignnone size-thumbnail wp-image-511" title="eol.modal" src="http://www.dannysu.com/wp-content/uploads/2012/07/eol.modal_-150x150.png" alt="" width="150" height="150" /></a>
<small>(All photos in the screenshots are from EOL. In particular, the awesome looking bird in the last screenshot is from <a href="http://www.flickr.com/photos/almircandido/4744381560/" target="_blank">Almir Cândido de Almeida</a> whose photo is on EOL.)</small>

Built using <a href="http://knockoutjs.com/" target="_blank">knockout.js</a>, <a href="http://twitter.github.com/bootstrap/" target="_blank">Twitter Bootstrap</a>, <a href="http://jquery.com/" target="_blank">jQuery</a>, HTML5 local storage, and some <del>PHP</del> Ruby &amp; Sinatra on server side. There's infinite scroll as well as responsive design that dynamically re-layout the cells based on browser width. Note that the collect feature doesn't integrate with EOL website at the moment. I had implemented it initially, but the feature is broken on EOL website right now so I went ahead and switched to storing favourites on my own DB.

I love it! It's awesome to be able to create. Well, now that I'm done creating,
I'm back browsing awesome looking animals!

  [1]: /2012/07/07/infinite-scroll-memory-optimization/
  [2]: http://eol.org
