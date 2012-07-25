---
title: MangaStream for Windows Phone 7
---
MangaStream for Windows Phone 7 is an app that I wrote during my free time so
that I can read the content on mangastream.com on the go. I started working on
the app during the down time I had between the original Windows Phone 7 release
and the release codenamed 'Mango'. Since finishing the 'Mango' release, I've
updated it to make use of new features in 'Mango' SDK. While I updated the app
to use the 'Mango' SDK, I also refactored my code to conform to the MVVM
pattern by fully utilizing data binding and ripping out code-behind from my
pages.

Features:

- Offline caching of images and metadata so that the app is functional without internet
- Jump list (similar to People hub in WP7)
- Page transition animations (utilizing Silverlight Toolkit)
- Storing data in SQL CE using LINQ to SQL (new in WP7 'Mango' release)
- Fast App Switching feature (new in WP7 'Mango' release)
- Background Agent to check for new manga releases (new in WP7 'Mango' release)
- Background Transfer (new in WP7 'Mango' release)
- Toast and Live Tile notification when new manga is released (new in WP7 'Mango' release)

You can find the [source code on GitHub](http://github.com/dannysu/mangastream).

**Screenshots:**

[![](/images/MangaStream1.png)](/images/MangaStream1.png)
[![](/images/MangaStream2.png)](/images/MangaStream2.png)
[![](/images/MangaStream3.png)](/images/MangaStream3.png)
[![](/images/MangaStream4.png)](/images/MangaStream4.png)
[![](/images/MangaStream5.png)](/images/MangaStream5.png)
