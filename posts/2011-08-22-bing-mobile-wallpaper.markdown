---
date: 2011-08-22 21:26:10
title: Bing (Mobile) Wallpaper
tags: Mad Coding, Microsoft, Bing, JQuery, PHP, SVG, WP7
---
I was on the dev team that wrote the Bing search & maps application for the
original release of Windows Phone 7. Even before WP7 was released to the world
I had been interested in using those beautiful Bing pictures as wallpaper for
my phone. You can often find gems like these:

![](http://bing.dannysu.com/images/en-us/2011-08-20.jpg)
![](http://bing.dannysu.com/images/en-us/2011-08-16.jpg)

Initially I used the WP7 SDK to develop an app to do that and unveiled it to my
team close to the release of WP7 and just around when 'Mango' release is
starting. However, since I sometimes miss checking for new images I wanted to
archive them as well. I quickly set up cron and some PHP scripts to archive and
display Bing pictures from 8 different countries on a webpage. Since 'Mango'
was a pretty tough release in terms of the schedule I didn't get to revisit my
simple webpage until now.

For the update, I used jquery to add a flip animation similar to that in
'People' tile or the 'Me' tile on Windows Phone 7. I didn't used to display the
credits, so after flipping the image over I now display the credits to show
what the image is about. You can either tap on a particular page or wait for
the random flip that occurs automatically. For the jquery animation I used
[RAWR! Flip example by Clay Lua][1]. For the back arrow graphic, I used the
vector graphic bundled with the WP7 SDK and embedded it as a SVG.

Visit it on your Windows Phone 'Mango' devices @
[http://bing.dannysu.com/](http://bing.dannysu.com/).

Note: I also tested on my iPhone but the back button and tap to flip back to
image aren't working. I'll have to look into that some other time. Haven't
tried it on my Android phone yet.

  [1]: http://hungred.com/wp-content/uploads/2009/04/demo-rawrflip.html
