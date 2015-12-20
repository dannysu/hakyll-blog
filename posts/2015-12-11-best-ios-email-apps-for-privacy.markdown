---
date: 2015-12-11 08:37:09 PST
title: Best iOS Email Apps for Privacy
tags: iOS, Apple, iPhone, iPad, Email, Gmail, FastMail, Dispatch, Spark, Outlook, Boxer, CloudMagic
---
**UPDATE**: FastMail wrote about how they [sanitise emails][32] to protect
their users. I encourage all email app developers to read it and do it
properly, while maintaining good layout and readability.

---

**tl;dr** The only apps that can protect your privacy are the built-in Email
app, FastMail, or Gmail (not Inbox).

One of the first things I did after getting the iPhone 6 last year was to try
to find an email app. Unfortunately no 3rd party client met my criteria at the
time. With the recent announcement of [Mailbox shutting down][4], people have
mentioned Spark as an alternative. Curious whether Spark will finally meet my
criteria, or whether any of the other apps have been improved, I set out to
re-examine them all.

# My Criteria

I'm a happy [FastMail][2] user, and I was really glad when they released their
[iOS app][3] a month or two after I bought my phone. Prior to that
every single email app I tried, other than Gmail, failed at protecting my
privacy. Although on my laptop I can avoid having my viewing habits tracked by
blocking images, every iOS email client I tried failed the
[emailprivacytester.com][1] test. This test, made by Mike Cardwell, sends an
email to you and lets you know what signals your app reveals when you open the
email.

For me it is a dealbreaker if an email client cannot pass this test. Whatever
nice features the app might have become pointless if it cannot pass this
test.

Another minor wish of mine is support for aliases. Wildcard emails is one of
the things I love about FastMail, so an email client that can deal with aliases
properly is also a very nice plus for me.

Here are my criteria:

- Must work with IMAP
- Must pass emailprivacytester.com test when viewing email
- Must also pass emailprivacytester.com test when replying to email
- Would be good if it handles aliases
- Would be good if replying to an email sent to an alias, the email client uses
  the alias automatically

How did the top apps fare? Let's find out.

# Mailbox

**[Mailbox][6]** only supports Gmail and iCloud, but I wanted to see if I was using
Gmail whether it'd be acceptable. It does handle Gmail aliases, so that's good.
However, I didn't find any way for me to block images by default. Because of
this it fails the emailprivacytester.com test upon viewing an email:

[![][5]][5]

<font color="green">&#x2713; Supports Gmail aliases</font>  
<font color="red">&#x2717; Fails emailprivacytester.com test upon viewing
email</font>

# Spark

**[Spark][7]** have been mentioned as an alternative to Mailbox. Overall it's a
very nice email client. In the **Settings** screen, there is even a **Privacy**
section where you can block images by default. It passes the
emailprivacytester.com test when viewing email, but sadly fails the test when
composing a reply.

[![][15]][15]

It's really, really not cool that random javascript someone sends me can execute
at will.

Here's the full test result:

[![][8]][8]

<font color="green">&#x2713; Supports aliases</font>  
<font color="green">&#x2713; Auto detects alias to use</font>  
<font color="green">&#x2713; Allows blocking images by default</font>  
<font color="green">&#x2713; Can still easily show images on-demand</font>  
<font color="green">&#x2713; Passes emailprivacytester.com test when viewing
email</font>  
<font color="red">&#x2717; Fails emailprivacytester.com test when replying to
email</font>

# Dispatch

**[Dispatch][9]** is not free and costs $6.99. It's actually a very nice email
client and I bought it last year to test it out. However, I was disappointed. I
believe I submitted a feature request to ask for privacy fixes, did anything
change? Nope. What's shocking is that even after disabling image loading, it
still fails emailprivacytester.com test. It doesn't just fail, it fails because
it allows javascript execution! C'mon! I paid for this.

[![][16]][16]

It's unclear whether the ability to execute javascript would lead to security
issues. It'll probably depend on how the email app is written and what the
javascript has access to. But at the very least the javascript can signal
whatever it wants.

[![][10]][10]

<font color="green">&#x2713; Supports aliases</font>  
<font color="red">&#x2717; Doesn't auto detect alias to use</font>  
<font color="red">&#x2717; Fails the emailprivacytester.com test even after
disabling the "Load Remote Images" setting</font>

# Outlook (Acompli)

Microsoft's acquisition of Acompli turned it into the **[Outlook][11]** email
app. It looks like a nice app in general, but lacks features I'd like and fails
the privacy test:

[![][12]][12]

<font color="red">&#x2717; No setting to block images by default</font>  
<font color="red">&#x2717; Fails emailprivacytester.com test upon viewing email</font>  
<font color="red">&#x2717; No support for aliases</font>  

# Boxer Pro

After testing out the free Boxer version for a while, I bought **[Boxer
Pro][13]** (currently $4.99). I liked the app and features, but once I tested
its privacy protection I had to ditch it. While it passes the privacy test when
viewing an email, as soon as you try to compose an email it's game over.
Although I might be able to forgive the meta refresh leak, its inability to
show images on demand after blocking it by default is really annoying. Also,
its HTML email cleaning code isn't as nice as FastMail's resulting in hard to
read emails after enabling the protection.

[![][17]][17]

[![][14]][14]

In other news, I found out that [VMware bought Boxer][31] just couple months
ago. I sure hope they don't shut it down after a while like Mailbox. There seems
to still be updates coming out, so I hope they'll still fix things up.

<font color="green">&#x2713; Supports aliases</font>  
<font color="green">&#x2713; Auto detects alias to use</font>  
<font color="green">&#x2713; Allows you to set to "High" privacy/security setting</font>  
<font color="red">&#x2717; No easy to show images on demand afterwards</font>  
<font color="green">&#x2713; Passes emailprivacytester.com test when viewing
email</font>  
<font color="red">&#x2717; Fails emailprivacytester.com test when replying to
email</font>  

# CloudMagic

I tried **[CloudMagic][21]** after trying Boxer and Dispatch. Unfortunately it
didn't fare very well either. It's lacking in almost all the basics I want to
have.

[![][22]][22]

<font color="red">&#x2717; No aliases</font>  
<font color="red">&#x2717; Doesn't seem to have a way to block images by default</font>  
<font color="red">&#x2717; Fails emailprivacytester.com test for viewing email</font>  

# Cannonball

I didn't try **[Cannonball][23]** last year, but found it while searching
through AppStore. It seems to have many good reviews, so I decided to give it a
go. Problem is that it's also lacking in almost all the basics I want. It fails
the privacy test both when viewing email and when composing a reply. There are
so many violations including javascript execution. Combined with the fact that
it doesn't support aliases means I quickly ruled out this one.

[![][18]][18]
[![][19]][19]

[![][24]][24]

<font color="red">&#x2717; No aliases</font>  
<font color="red">&#x2717; Doesn't seem to have a way to block images by default</font>  
<font color="red">&#x2717; Fails emailprivacytester.com test for viewing email</font>  
<font color="red">&#x2717; Fails emailprivacytester.com test for replying to an
email</font>  

# myMail

Many people seem to like **[myMail][30]** too. I remember that I tried this out
last year but don't remember much about it. It looks like they stored my login &
password in iOS Keychain because the data persisted after the app was
uninstalled. I tested it out again but didn't spend much time with it. There
doesn't seem to be a way to block images by default, so my testing with it was
pretty short. It's also lacking aliases support.

[![][29]][29]

<font color="red">&#x2717; No aliases</font>  
<font color="red">&#x2717; Doesn't seem to have a way to block images by default</font>  
<font color="red">&#x2717; Fails emailprivacytester.com test for viewing email</font>  

# FastMail

As I mentioned, I'm pretty happy with [FastMail's iOS app][28]. It doesn't have any
dealbreakers for me and FastMail really lives up to the "Fast" in their name. If
you ever had the experience of having to manually refresh Gmail, then you'll
know what I mean by fast because I never have to do that with FastMail.

FastMail does have its downsides though. I do wish there's a snooze feature like many
other email apps, but I can live without it.

<font color="green">&#x2713; Can block images by default</font>  
<font color="green">&#x2713; Can still easily show image on demand</font>  
<font color="green">&#x2713; Passes emailprivacytest.com test for viewing email</font>  
<font color="green">&#x2713; Passes emailprivacytest.com test when replying to email</font>  
<font color="green">&#x2713; Supports aliases</font>  
<font color="green">&#x2713; Auto detects alias to use</font>  

# Yahoo Mail

I have a @ymail.com account lying around that I never use. I thought I'd take
[Yahoo Mail][25] out for a spin and see what happens. XSS is what happens.

[![][20]][20]

[![][26]][26]

<font color="green">&#x2713; Can block images by default</font>  
<font color="green">&#x2713; Can still easily show image on demand</font>  
<font color="green">&#x2713; Passes emailprivacytest.com test for viewing email</font>  
<font color="red">&#x2717; Fails emailprivacytest.com test when replying to email</font>  

# Gmail

The official **[Gmail][27]** app is really good too. Checks all the boxes. I use
FastMail, so I don't use Gmail except for work.

<font color="green">&#x2713; Can block images by default</font>  
<font color="green">&#x2713; Can still easily show image on demand</font>  
<font color="green">&#x2713; Passes emailprivacytest.com test for viewing email</font>  
<font color="green">&#x2713; Passes emailprivacytest.com test when replying to email</font>  
<font color="green">&#x2713; Supports aliases</font>  
<font color="green">&#x2713; Auto detects alias to use</font>  

# Inbox

We use Google Apps at work for email, and I just found out that you can enable
Inbox for Google Apps now. The interface is nice and helps you to get to Inbox
Zero. Unfortunately, no way to block images by default means it's off my list.
What's also annoying is that once you enable Inbox for your organization there
is no way of turning it off.

<font color="red">&#x2717; Doesn't seem to have a way to block images by default</font>  
<font color="red">&#x2717; Fails emailprivacytester.com test for viewing email</font>

# Built-in Mail App

If I remembered right, before FastMail iOS app was released I used the built-in
Mail app because it passes the privacy test. However, one annoying thing is
that once you block images by default, there's no easy way to show images on a
per email basis.

Other than that it also supports aliases as **retemirabile** commented below.
The setting is rather hidden, but it does auto detect the correct alias to use
which is nice. Go to `Settings -> Mail, Contacts, Calendars`, select the
account you want to add an alias for, tap on `Account`, tap on `Email`.

<font color="green">&#x2713; Can block images by default</font>  
<font color="green">&#x2713; Passes emailprivacytest.com test for viewing email</font>  
<font color="green">&#x2713; Passes emailprivacytest.com test when replying to email</font>  
<font color="green">&#x2713; Supports aliases</font>  
<font color="green">&#x2713; Auto detects alias to use</font>  
<font color="red">&#x2717; Cannot show image on demand</font>  

# Conclusion

As the tests have shown, if you don't want to reveal your email viewing
activities on iOS, you have 3 options:

1. Use the built-in Mail app (provided that you don't care about being able to
   show images for particular email easily)
1. Use Gmail but not Inbox (if you have Gmail, or if you want to pipe emails to
   Gmail)
1. Use FastMail (if you have FastMail, or if you want to pipe emails to
   FastMail)

If you want to have more options like I wish I do, please help by contacting the
developers and let them know. I'm going to send this page to them in hope that
they'll take action.

I would happily pay for an email app, and in fact I already bought a couple. But an
email client that can't protect my privacy is not something I want to use.

So for now, I pipe all emails that I have from my @gmail.com and @live.com emails
to FastMail and use the FastMail iOS app. FastMail allows you to configure SMTP
servers such that when I send emails using the FastMail interface, it'll still
use gmail's server if the from address is @gmail.com.

  [1]: https://emailprivacytester.com
  [2]: http://www.fastmail.com
  [3]: https://blog.fastmail.com/2014/11/12/fastmail-app-for-ios-and-android-now-available/
  [4]: https://blogs.dropbox.com/dropbox/2015/12/saying-goodbye-to-carousel-and-mailbox/
  [5]: https://imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg4LEgVpbWFnZRi53bkBDA
  [6]: https://itunes.apple.com/us/app/mailbox/id576502633?mt=8
  [7]: https://itunes.apple.com/us/app/spark-smart-email-app-for-work/id997102246?mt=8
  [8]: https://imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg4LEgVpbWFnZRi5-KcEDA
  [9]: https://itunes.apple.com/us/app/dispatch-email-meets-gtd-textexpander/id642022747?mt=8
  [10]: https://imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg0LEgVpbWFnZRi5iAUM
  [11]: https://itunes.apple.com/us/app/microsoft-outlook/id951937596?mt=8
  [12]: https://imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg4LEgVpbWFnZRi5mvkBDA
  [13]: https://itunes.apple.com/us/app/boxer-for-gmail-outlook-exchange/id561712083?mt=8
  [14]: https://imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg4LEgVpbWFnZRjJ6PkBDA
  [15]: https://imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg4LEgVpbWFnZRjZtvoBDA
  [16]: https://imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg4LEgVpbWFnZRi55rMCDA
  [17]: https://imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg0LEgVpbWFnZRjJ1gUM
  [18]: https://imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg4LEgVpbWFnZRjK6PkBDA
  [19]: https://imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg4LEgVpbWFnZRjJq7oBDA
  [20]: https://imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg4LEgVpbWFnZRjJva4DDA
  [21]: https://itunes.apple.com/us/app/cloudmagic-email/id721677994?mt=8
  [22]: https://imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg4LEgVpbWFnZRjZ-boBDA
  [23]: https://itunes.apple.com/us/app/cannonball-email/id701582906?mt=8
  [24]: https://imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg0LEgVpbWFnZRiJp30M
  [25]: https://itunes.apple.com/us/app/yahoo-mail-free-email-and-news/id577586159?mt=8
  [26]: https://imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg4LEgVpbWFnZRjatvoBDA
  [27]: https://itunes.apple.com/th/app/gmail/id422689480
  [28]: https://itunes.apple.com/us/app/fastmail-email-calendars-contacts/id931370077?mt=8
  [29]: https://imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg4LEgVpbWFnZRjJtLQCDA
  [30]: https://itunes.apple.com/us/app/mymail-free-email-app-for/id722120997?mt=8
  [31]: http://www.getboxer.com/boxer-joins-vmware/
  [32]: https://blog.fastmail.com/2015/12/20/sanitising-html-the-dom-clobbering-issue/
