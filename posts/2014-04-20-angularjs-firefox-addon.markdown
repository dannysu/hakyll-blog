---
date: 2014-04-20 22:34:38
title: Using AngularJS to develop Firefox addon
tags: Mad Coding, Firefox, AngularJS
---
I was trying to update my [hash0-firefox][2] addon to use the newer [hash0][3]
version I've been working on. I switched to using AngularJS in the newer version
instead of jQuery Mobile.

However I ran into an issue where AngularJS code gets into an infinite $digest
loop when used inside Firefox addon. I filed an [issue on github][1] and tracked
down a solution.

I don't know the AngularJS innards to know why, but if the platform AngularJS
runs on doesn't have proper history API, then it seems to get into the $digest
loop.

I found that inside the code, when `history.replaceState` was called, Firefox
addon was giving me the following error:

<pre>
Message: [Exception... "Failure"  nsresult: "0x80004005 (NS_ERROR_FAILURE)"  location: "JS frame :: resource://jid1-1qqtvsfpouxjea-at-jetpack/angularjs-firefox-seed/data/angular-seed/bower_components/angular/angular.js :: Browser/self.url :: line 4409"  data: no]
</pre>

I started to dig into AngularJS code to find out why that is. I found a
workaround by adding to an if-statement to stop AngularJS from using history
API.

<pre class="brush:js">
    history: !!($window.history && $window.history.pushState && !(android < 4) && !boxee && !$window.addon),
</pre>

You add `!$window.addon` after `!boxee` inside the angular.js file and that
allowed my addon to function correctly afterwards.

  [1]: https://github.com/angular/angular.js/issues/7171
  [2]: https://github.com/dannysu/hash0-firefox
  [3]: https://github.com/dannysu/hash0
