---
date: 2013-05-02 22:05:09
title: Iterating at Light Speed with Cordova/PhoneGap
tags: Mad Coding, AvidTap, Start-up, Cordova, PhoneGap, HTML5
---
I have a love & hate relationship with hybrid web apps. I have some experience
in building apps using Cordova/PhoneGap, and I covered some of the pros and cons
in a previous post about [Well.ca's virtual store app][1]. Despite the numerous
deficiencies when building a mobile app using web technology, it turned out to
be one of the right decisions we made for my start-up [AvidTap][2].

It's not that we don't have expertise to build native apps or don't want to. I
also happen to have experience building native apps for iOS, Android, and
Windows Phone. However, at the company's infancy there's such limited resources
that we don't want to delay getting to our MVP. Both Facebook and LinkedIn tried
HTML5 and reverted back to making native apps, and I think for getting the
absolute best experience that's the right thing to do. However, for AvidTap at
this stage hybrid is a better choice.


# Rapid Iteration with Over-The-Air Updates

For our app, I built an update system that allows AvidTap push out delta
updates to any user. The update system allows the server to inform clients when
there is a new version and allows us to actually do A/B testing in the app.
We're constantly working on the app and pushing out tweaks 2 to 3 times a week.

Being able to do OTA updates means that we can test out our hypothesis rapidly.
If we have a question that needs to be answered, we can quickly implement and
gather the information we need to make decisions on. Being able to push out OTA
update also means that we can fix issues faster. As an example, we recently
deployed AvidTap along with our patent-pending box at [Superfood Eateries][3].
Once we put the product out there and saw people using it, we quickly realized
that the code selection list UI we implemented was confusing even though it was
meant to help. We were able to quickly push out an update that fixed the
usability in a day.

  [1]: /2012/04/13/well-ca-virtual-store/
  [2]: http://avidtap.com
  [3]: http://www.superfoodeateries.com
