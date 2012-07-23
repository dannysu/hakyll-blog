---
date: 2006-08-20 21:15:31
title: Security for programs designed to run on Windows Mobile
tags: Microsoft, Security
---

Every developer and software designer should have security in mind when designing and programming software. Software security must be considered both at code level (buffer overflow, heap overflow, etc) and at the design level.

I came across this article: [http://msmobiles.com/news.php/5474.html](http://msmobiles.com/news.php/5474.html). It talks about mistakes that some Windows Mobile programs made.

Things like storing password in plaintext, or using weak encryption methods like ROT-N, which is just a method that replaces a letter with the Nth letter down the alphabet sequence. Choosing a method for encryption or authentication is a design level issue.

The article goes on to talk about some code level issues as well. Some programs written for Windows Mobile can have their encryption mechanism bypassed by removing a registry key. This is a flaw in the implementation of the program possibly due to incorrect error handling.

An alarming news to me is that Microsoft Money for Windows Mobile 2006 uses a proprietary method for encrypting password!!! I thought Microsoft's Secure Software Development process will not even let this software release! I'm going to track down on this issue.
