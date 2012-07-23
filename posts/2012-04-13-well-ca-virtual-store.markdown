---
date: 2012-04-13 00:55:12
title: Well.ca Virtual Store
tags: Apple, Google, Mad Coding, Microsoft, PhoneGap, SASS
---

Well.ca's Virtual Store launched on April 2nd, 2012. I wrote smartphone apps for 4 mobile platforms and the supporting server-side work to make this happen. You can check out the time-lapse video here: [http://well.ca/virtualstore](http://well.ca/virtualstore)

In this post I want to look back at the things I learned through this project. E.g. PhoneGap, SASS, app certification, etc.




## **PhoneGap**


So how was it done? It was a one man army (i.e. me) on the software side to produce apps for 4 mobile platforms plus I had to work on the server side stuff as well. Given the time constraint, I ultimately chose to use [PhoneGap](http://phonegap.com/).

When I first started on the project, PhoneGap was still maturing. For example, initially PhoneGap did not offer an API for downloading file and save to phone's local storage. I found a [PhoneGap Downloader plugin for Android](https://github.com/phonegap/phonegap-plugins/tree/master/Android/Downloader), but I had to write downloader code for iPhone and BlackBerry. However, later on closer to the actual launch, PhoneGap version 1.4.1 included downloader API and I switched over to that instead.

Challenges using PhoneGap:



	
  * Debugging javascript on the phone is difficult - During testing, there was a bug found that was a javascript error which caused the entire app to stop working. However, since this only happened while using the phone and I had no easy way to reproduce the issue on laptop browser it was difficult to debug.

	
  * PhoneGap still maturing while I was developing

	
  * Inconsistent API - Local storage API has different meaning for PERSISTENT type and TEMPORARY type depending on the mobile platform. I had to read PhoneGap code to know what to use. (Update: [PhoneGap 1.6.0](http://phonegap.com/2012/04/11/phonegap-1-6-released/) might have unified inconsistency between the platforms somewhat)

	
  * Can't do smooth animation using jQuery

	
  * Animation using CSS3 was not completely smooth either

	
  * HTML5 support for form input types are very incomplete on mobile browsers

	
  * Support for CSS varies between mobile browsers even if they share WebKit underneath but probably different versions of it (E.g. fixed position renders differently on BlackBerry)

	
  * Performance varied greatly between phones. Some phones were quick to process javascript so the app performs closer to native app, but on other phones you can really feel the lag and delay.

	
  * jQuery mobile alpha releases had terrible performance on my PhoneGap apps, but the v1.0 release made noticeable improvement (I ended up choosing to use [Zepto.js](http://zeptojs.com/) instead)

	
  * Dependent on BlackBerry WebWorks, but version 2.2 was basically unusable. I wasn't able to support BlackBerry OS6+ until they released 2.3 earlier this year.

	
  * Very difficult to make the app look and feel native

	
  * If a hardware or platform feature isn't available, then you have to roll your own and therefore dive into native app code anyway

	
  * Media query support varies (E.g. BlackBerry always report pixel density as 1.0)

	
  * Still have to write platform specific code (E.g. I had to write the most workarounds for BlackBerry because different versions of the OS work differently)




Advantages of using PhoneGap:

	
  * Once the app is written, it takes about a week to support a new platform that uses WebKit based browser (i.e. Android, BlackBerry, and iPhone)

	
  * Code sharing across platforms - Well.ca uses git as source control and I use git submodules to share the common app code

	
  * No simulator needed - I can develop on my laptop using Safari by using stub functions whenever I accessed hardware features

	
  * The browser is good at dealing with complex layout via HTML

	
  * Allow people to reuse web development skills

	
  * Don't need to think about memory management at least for the size of my app





## **SASS**


I read this [tutorial](http://designbycode.tumblr.com/post/1127120282/pixel-perfect-android-web-ui) on getting pixel perfect HTML apps for Android. Basically making use of media query and determine which CSS to use. Since I have to support all sorts of screen sizes with varying pixel density depending on the phone, this is what I did to make the app look good on all phones. To write the CSS, I used [SASS](http://sass-lang.com/) to help me.

SASS is great because it allows me to reuse CSS across different screen sizes by making use of [mixin](http://sass-lang.com/docs/yardoc/file.SASS_REFERENCE.html#mixins) and [numeric operations](http://sass-lang.com/docs/yardoc/file.SASS_REFERENCE.html#operations).

The one painful thing about SASS is its variable scope. Every variable is automatically global unless you restrict it in a mixin. This was annoying and contrary to how programming languages work.

Also, writing CSS using SASS can lead to larger resulting CSS if you're not conscience about what it's really doing.




## **App Certification**


App certification is a pain on all mobile platforms except Android where there is no checking at all. As a developer I like the fact there is no validation at all, but as a consumer I think a balance can be made because Android's method doesn't make the platform safer for users.



Apple App Store:



	
  * It took on average 7 business days from submission to being reviewed

	
  * Validation done during certification varies a lot (App might get rejected for 1 reason and next submission it passes)

	
  * No forced capability validation

	
  * Cannot add new capability to an existing app

	
  * If you're really under time constraint, you can request for an expedite review.




BlackBerry App World

	
  * Worst submission process in my experience

	
  * Allows you to submit without selecting supported device and have to wait couple business days to find out

	
  * Still wants me to select supported devices even though I already selected the minimum OS to support

	
  * Rejection reason is very minimal and doesn't even tell you how to reproduce or what phones were used during testing

	
  * No clear way to respond to rejection

	
  * Certification process ran into a BlackBerry OS bug that caused apps to not run (I had to find a RIM contact from co-worker to get the app in)




Windows Phone

	
  * Did not allow in-app collection of billing information (Didn't have this issue on other platforms)

	
  * Very nice rejection report telling you repro steps, what devices were used, OS version, etc

	
  * Capability detection for WebBrowser has gotchas, but I like that there is actual capability detection. Developer can't lie.





## **Windows Phone**


I love my Windows Phone, so I really wanted to include it in the launch. I basically asked the CEO if I can make it happen whether we can include it for launch. With some hustle and some help from my past teammates, I was able to make it happen!

Since Windows Phone doesn't use WebKit based browser, reusing existing code for it turned out to be hard. I tried for a little while to get PhoneGap and my existing app working on it but didn't get anywhere. I swapped out Zepto.js and replaced with jQuery, but I was still running into road blocks. Since I was already familiar with Windows Phone development, I developed the Windows Phone app using Silverlight instead. That itself was a learning experience. I found out that trying to write a performant and complex web layout on a list view on a phone is really really hard. Avoid doing complex layout in the list view or else you'll suffer performance issues. Interestingly this wasn't as big a deal when I was still working at Microsoft because I was able to code in C++ using internal framework. However, using Silverlight it's a bad idea to have a type selector for list items. Using profiling tools you'll find that the layout cost from doing so really kills the perf.




## **BlackBerry**


With WebWorks v2.3, I was finally able to support BlackBerry OS6+. The main motivation behind supporting BlackBerry is because Canada has a large population that still use BlackBerry. However, my development experience with BlackBerry is quite negative. You can see on RIM website that over 80% of the people still use OS5 or OS6 and both are very incompetent phones. Upon each app install you have to restart the phone. It took minutes from beginning of app download to being able to use an app whereas it takes average 15 seconds on the iPhone. Even the simulators have different development tools available depending on the device and OS version. Even the simulated camera functionality worked differently between simulators.




## **Server-Side Work**


To support the mobile apps, I worked on exposing product information via a REST interface. To enable checkout through the mobile app, I had to refactor the terribly designed zen cart code to make the relevant portion re-usable. I won't go into details, but this is another instance of when I decided to take the pain to refactor because by doing so I allowed mobile app to perform checkout, enabled automated testing to be done, and also enabled another internal project to do checkout that I can't talk about at this time.


## **
The Launch**


****The night before the actual launch, I went to Union station in Toronto to meet up with VP of Marketing, PR agency, videographer and the workers who'll put up the posters. The whole process of putting up the posters was documented in the time-lapse video on the virtual store landing page. I arrived around 6:30PM and didn't leave until past 11PM.

The actual launch day was also interesting. I arrived at Union station at 7AM and worked from a coffee shop that had free wifi. Well.ca's VP of Marketing, PR agency and photographers were busy capturing professional photos for the launch. As soon as I got newer photos I put them up on the landing page.

After lunch, I worked from PR agency's office where the CEO, COO, marketing team were all there handling responses from the launch. The Globe and Mail's Marina Strauss wrote an [article on the launch](http://www.theglobeandmail.com/report-on-business/virtual-shopping-gets-real-in-toronto-subway-station/article2389896/). The Marketing Mag also [covered the event](http://www.marketingmag.ca/news/marketer-news/well-ca-opens-virtual-store-in-toronto-49725) and I found out that marketing people are really excited about showing up on Marketing Mag.

At 6PM, the CEO invited other people to come down to check out the virtual store. Afterwards, the team went to Marché restaurant at Brookfield Place to celebrate. VP of Marketing was on radio the next day and the CEO was on Canada AM on TV next morning!




## **Responses**


Responses to the virtual store were largely positive. People were excited to see innovation happening in Canada.

A review from BlackBerry App World:


> Scanned a QR code at their virtual store and my laundry detergent showed up at my door in 2 days. Amazing. Hopefully this inspires more pop up stores!


Most people get the concept and really liked it. On twitter the words "cool" and "field trip" were often used in talking about the virtual store.

That said, there were other types of responses and I can categorize all the responses into 4 groups:



	
  1. People who get it and thought it was cool

	
  2. People who didn't get it and wondered by they would leave home only to scan a QR code for products delivered later

	
  3. People who thinks it's a poor copy of the concept used in Korea

	
  4. People who are afraid of technology taking over the world or losing jobs





## **Conclusion**


It was a fun launch and people were mostly excited about the virtual store. I learned a bunch of new things throughout this process. However, for me it's a love & hate relationship with PhoneGap. It's a technology that's still maturing and definitely an option for people with limited resources or time. It might even be the preferred option because of ability to reuse web development skills. However, developing using platform's SDK might still be the way to go to provide the best experience for that platform.



A screenshot of the virtual store landing page:

[![](http://www.dannysu.com/wp-content/uploads/2012/04/Virtual-Store-Well.ca-Canadas-online-health-beauty-and-skin-care-store-Free-Shipping-225045-150x150.png)](http://www.dannysu.com/wp-content/uploads/2012/04/Virtual-Store-Well.ca-Canadas-online-health-beauty-and-skin-care-store-Free-Shipping-225045.png)
