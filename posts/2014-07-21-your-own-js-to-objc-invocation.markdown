---
date: 2014-07-21 00:30:00 PDT
title: Make Your Own js-to-ObjC Invocation Code
tags: Start-up, AvidTap, Kash, AvidRetail, Cordova, PhoneGap
---
The decision to go native versus hybrid for app development depends on your
team's ability, whether you need rapid iteration, and the timeframe you get to
work with. With AvidTap, I [discussed before][1] how I fully utilized the
hybrid approach and made an app that can be updated over-the-air on my own
schedule. With Kash, my team made the opposite decision and decided to go
native. We made this decision because we have more resources on hand, so we
wouldn't be speeding up the time to MVP by going hybrid. Having a native app
allows us to have a much more polished experience and consistent speed across
all devices.

Going with the native approach doesn't mean you can't sprinkle hybrid
approach in your app. We did that for Kash in order to mitigate the issue of
waiting for App Store review.


# App Store Review Time

Currently it takes about 7 days from submission to approval. It used to be
about 4 days. This lead time might be ok for established companies, but for a
startup that's quite deadly. Because of this, I had to implement certain
strategies to ensure I can iterate on Kash without always waiting for Apple.
The first approach is by delegating tasks to an UIViewController that contains
an UIWebView. The second approach is by using [TTTAttributedLabel][2] and have
whatever messaging and action I want to be sent from server.


# Invoke Native Code from Javascript

For the Kash app, I made use of the hybrid approach for experimenting with
post-purchase engagement ideas we have. Everything in the payment flow is done
in native except when triggered by server to do certain things using UIWebView.

Pulling in the full Cordova framework would be overkill. I adapted from
Cordova's invocation method and added my own semantic for how the custom scheme
is used to pass parameters. Below is the code I used to invoke native
functionality from javascript.

```javascript
var execIframe = null;

var createExecIframe = function() {
    var iframe = document.createElement("iframe");
    iframe.style.display = 'none';
    document.body.appendChild(iframe);
    return iframe;
};

sendToNative = function(cmd, data) {
    execIframe = execIframe || createExecIframe();
    // Check if they've removed it from the DOM, and put it back if so.
    if (!execIframe.contentWindow) {
        execIframe = createExecIframe();
    }

    var url = "kash://invoke/cmd/" + encodeURIComponent(cmd);
    if (data) {
        url += '/data/' + encodeURIComponent(JSON.stringify(data));
    }
    execIframe.src = url;
};

// Then whenever you want to invoke native functionality, call:
// sendToNative('the command', {arg1:"value1"});
```

Then in your UIViewController that contains the UIWebView, you'll want to
implement the following UIWebViewDelegate function. Inside it you check for
whether the request should be handled specially or not.

```objc
- (BOOL)webView:(UIWebView *)webViewInstance shouldStartLoadWithRequest:(NSURLRequest *)request navigationType:(UIWebViewNavigationType)navigationType {
    NSURL *url = [request URL];
    
    if ([[url scheme] isEqualToString:@"kash"]) {
        // Parse URL to figure out the command and data (if any)
        return NO;
    }
    return YES;
}
```

By using the code shown and implementing special commands to be invoked from
javascript, it allowed us to test a bunch of theories in short amount of time.
Being able to test as many hypothesis as possible in a given time is very
crucial.


# Using TTTAttributedLabel

One down side of using UIWebView is that it isn't as fast as showing something
native. You have to wait for the webpage to load and there is more data to
load. To ensure users have a great experience using Kash, I also made use of
Mattt Thompson's [TTTAttributedLabel][2].

TTTAttributedLabel allows me to send JSON from server like the one shown below.
It allows me to control what the user sees entirely from server-side while
maintaining the speed of a native app.

```javascript
var modals = [];
var modal = {
    msg: "A Title\n\nWrite something in the body",
    styles: [],
    buttons: []
};
modal.styles.push({
    apply_to: {
        from: 0,
        length: 'A Title'.length
    },
    font_style: 'bold',
    font_size: 25,
    color: constants.white_colour
});
modal.buttons.push({
    label: 'Button Label',
    action: 'close',
    bg_color: constants.default_btn_bg,
    fg_color: constants.white_colour
});
modals.push(modal);

// I return an array of "modals", which tells the native code what message to show.
// Native code reads the JSON and then use TTTAttributedLabel to show parts of the text differently.
```

  [1]: /2013/05/02/iterating-at-light-speed/
  [2]: https://github.com/mattt/TTTAttributedLabel
