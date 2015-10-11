---
date: 2006-12-31 22:02:29
title: Brian's Website Audit
tags: Security
---
First post with the new *Security* tag

Yesterday I got sidetracked when going through my website and making sure that
the site is not obviously vulnerable to attacks. I ended up wondering if
Brian's website has the common bugs that I'm trying to prevent on my own
website. Guess what I found out.

I found 2 bugs and couple bad designs from security point of view.

I seriously hope that I don't have critical bugs on my own website, and I hope
to check everything soon. I will educate myself and design software that is
more secure.


# XSS Vulnerability

The first problem with Brian's website is a XSS vulnerability in his Count
Challenge PHP page. When I was going through his pages, the first thing I try
to do is feed the arguments bad data. His count.php takes an argument num from
the URL and assumes that it is a numeric value. When I used "hehe" as the value
to num and saw that it appeared on the webpage, I knew I found a XSS bug. I
then proceed to inject javascript code onto his page and basically display any
content I wish. See image:

![](/images/brianxssvulnerability.png)

Good thing he wasn't using the argument for database interaction, otherwise I
would be trying to inject SQL statements.

So why is this important seeing that I must send out an URL with the injected
code for this to display what I want? Imagine this kind of bugs appear on
popular sites like Google, eBay, or MySpace. Imagine this being used to spread
malware or being used in phishing attacks. Can the average computer user
identify that the URL has injected code?

Do a search for "google xss" and be scared... be very scared... the fact that
somebody can send an email to your gmail and have their code execute is scary.


# I scored 10/10 on Brian's questionnaire!

The 2nd bug is a fun one. I remember back when Johnny used to try out Brian's
questionnaire and try to score as high as possible. Ah the fun memories.

Again, I supplied invalid values to arguments that the questionnaire uses and
discovered that I can basically get any score I wish!

If you know all about Brian and scored 10/10, then you will get a secret
phrase. I forgot whether Johnny ever got 10/10, but it was fun to see that
anybody could score 10/10.


# Bad Design

The last thing I tried to tackle on Brian's website was with the way he
displays contents on his site. He uses a single index.php page and provide
value to page argument to specify exactly what to show.

An example:  
http://www.brianswebsite.com/index.php?page=contact.htm

Having your user supply the exact location of the page to display is just a bad
idea.

Through this I was able to find out the directory name where Brian stores all
of his HTML files. It was a guess, but I didn't use any information I know from
being his friend. This means some outsider could just as easily guess the
directory name.

Another problem is with his image gallery. It displays directory content and
subdirectory names. The application essentially allows you to navigate certain
parts of his website. If not done properly, this combined with knowledge about
his file structure could lead to even more information disclosure.
