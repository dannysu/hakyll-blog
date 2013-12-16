---
date: 2013-12-15 18:53:30
title: New AvidTap app update based on Cordova 3.1
tags: Start-up, AvidTap, Android, iOS, iPhone, Cordova, PhoneGap, KitKat, BLE, Bluetooth LE, Bluetooth Smart, iBeacon
---
Here at [AvidTap][5] we've been preping a new set of native app updates based on
[Cordova][1] 3.1.0 since Oct. I recently unleashed both the Android and iPhone
apps last week. This new update brings the Bluetooth LE / iBeacon support I
talked about before. Watch out for us beginning to roll that out to stores in
the new year.

During the time when we were updating our project to Cordova 3.1, Google
released Android KitKat. Fortunately, [Cordova 3.1 supports this new Android
version][2]. We also wanted to update the app for iOS 7 support.

Coming from Cordova version 2.2 and jumping to version 3.1 is a big leap. v3.1
brings many new things. In particular I love the fact Cordova is consolidating
more and more of the different platforms together via the creation of
[cordova-cli][3], the new plugin system, and all the effort in a common js base.

I love the speed at which Cordova is improving. There has been tons of
improvements since I wrote the Well.ca virtual store app. There has been tons of
improvements just in the year after I started AvidTap alone. AvidTap's release
speed for [AvidRegister][6] is amazing too. We average 3.61 days between
releases to Play Store. Talk about rapid improvements.

<br>

## **New Plugins**

The new plugin system is great and will make things more plug & play. For our
release, I wrote a bunch new plugins based on the new format. These plugins
include:

- Bluetooth LE / iBeacon
- card.io
- NFC
- Push notification
- Porting twitter plugin to new format
- Porting WebIntent plugin to new format
- Porting existing Email Composer plugin to new format
- Making Facebook plugin work in new version

<br>

## **iOS 7 Status Bar**

On iOS 7 the app can take over the area underneath the status bar too. There are
many suggestions on the Internet for how to fix that in MainViewController.m.
However, I found that they don't work for AvidTap's app. I mainly saw issues
whenever we have some sort of plugin that navigates out of the app and come
back. The following code is what I wrote to simulate the 20px status bar and
have it work correctly in all situations including when navigating out of the
app and come back.

<pre class="brush:objc">
- (void)viewDidLoad
{
    [super viewDidLoad];
    // Do any additional setup after loading the view from its nib.
    
    // On iOS 7, simulate a status bar taking 20 px space like in earlier iOS versions.
    if ([[[UIDevice currentDevice] systemVersion] floatValue] >= 7) {
        self.view.backgroundColor = [UIColor blackColor];
        [[UIApplication sharedApplication] setStatusBarStyle:UIStatusBarStyleLightContent];
        self.webView.frame = CGRectMake(0, 20, self.webView.frame.size.width, self.webView.frame.size.height - 20);
    }
}
</pre>

That said, there's now a [new plugin for iOS status bar][4], so probably try that
first. At the time I did the upgrade there was no such plugin. I will
investigate upgrade to that plugin in the future.

<br>

## **App Store Approval**
Apple seems to have made improvements to app approval speed since I last
submitted an update. The AvidTap update was approved in about 3-4 days. That's
way way better than the 2 weeks I had to wait for Well.ca's virtual store app.

  [1]: http://cordova.apache.org
  [2]: http://cordova.apache.org/announcements/2013/11/15/kitkat.html
  [3]: https://github.com/apache/cordova-cli
  [4]: https://shazronatadobe.wordpress.com/2013/10/15/cordova-ios-and-ios-7-support/
  [5]: http://avidtap.com
  [6]: http://avidregister.com
