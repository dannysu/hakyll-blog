---
date: 
title: "Your binary is not optimized for iPhone 5" Issue
tags: Mad Coding, Apple, Start-up, iOS, iPhone, XCode
---
It took me a while to find the solution to this, so I'm blogging to
hopefully help other people find it. If you are getting the following error
message during Apple App Store validation, then see the solution below.

## **Validation Error**

<blockquote>
Your binary is not optimized for iPhone 5 - New iPhone apps and app updates
submitted must support the 4-inch display on iPhone 5 and must include a
launch image with the -568h size modifier immediately following the
&amp;&lt;basename&amp;&gt; portion of the launch image's filename. Launch
image must be PNG files and located at the top-level of your bundle, or
provided within each .lproj folder if you localize your launch images. Learn
more about iPhone 5 support and app launch images by reviewing the 'iOS
Human Interface Guidelines' at
'[https://developer.apple.com/.../MobileHIG/IconsImages/IconsImages.html#/...][2]'
and the
'iOS App Programming Guide' at
'[https://developer.apple.com/.../iPhoneOSProgrammingGuide/App-RelatedResources/App-RelatedResources.html#/...][3]'.
</blockquote>

<br>

## **Solution**

I found the solution in a github issue discussion [here][1]. Credits to [David
Casali][4].

I'm mirroring the solution David posted here:
![](http://imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg0LEgVpbWFnZRjZswEM)

This issue was tricky because I had all the right images and dimensions as per
Apple documentation (Default.png, Default@2x.png, Default-568h@2x.png). I put
them into the LaunchImage asset catalog, but couldn't figure out why it still
fails to validate.

  [1]: https://github.com/Simbul/baker/issues/1201
  [2]: https://developer.apple.com/library/ios/documentation/UserExperience/Conceptual/MobileHIG/IconsImages/IconsImages.html#//apple_ref/doc/uid/TP40006556-CH14-SW5
  [3]: https://developer.apple.com/library/ios/documentation/iPhone/Conceptual/iPhoneOSProgrammingGuide/App-RelatedResources/App-RelatedResources.html#//aple_ref/doc/uid/TP40007072-CH6-SW12
  [4]: https://github.com/folletto
