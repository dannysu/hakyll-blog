---
title: MSNP11 Implementation for Adium
---
[Gaim][1] (now named pidgin) is an open source project that provides support
for many instant messaging protocols. [Adium][2] is a nicer GUI wrapper for
libpurple on Mac OS X. MSNP11 refers to version 11 of the MSN protocol.

Being frustrated that I cannot view people's personal messages while using
Adium, I started hacking Gaim source code to upgrade its MSN protocol
implementation. I successfully wrote a patch that allows gaim to work with
MSNP11. I then modified Adium source code to tie the two together.

The result of my work can be seen through the following screenshots:

![](https://imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg0LEgVpbWFnZRi8kX8M)

![](https://imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg4LEgVpbWFnZRj7p7ADDA)

My patch for MSN module is roughly 1,000 lines of code.  
MSN module within Gaim is roughly 18,000 lines of code.

[MSNP11 Documentation][3]

  [1]: http://www.pidgin.im/about/
  [2]: http://adium.im/
  [3]: http://msnpiki.msnfanatic.com/index.php/MSNP11:Changes
