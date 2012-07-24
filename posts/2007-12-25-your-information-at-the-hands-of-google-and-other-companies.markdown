---
date: 2007-12-25 15:12:27
title: Your information at the hands of... Google and other companies
tags: Google, Security
---
I'm severely behind on my RSS feeds as I have already mentioned.  Today I woke
up and decided to head to digg.com to check out what's new.  I saw DavidAirey's
entry on how his information was stolen because of a Gmail bug:
[http://www.davidairey.co.uk/google-gmail-security-hijack/][1]

Everybody is at risk nowadays with information stored by Amazon, by Google, by
Microsoft, etc.  You don't need to look hard to know that no company has
perfect record.  It's not like if someone works for a particular company, the
engineer somehow automatically have perfect grasp of writing secure code.
Besides, how can one have perfect grasp when security issues are evolving
everyday?

What's interesting about Airey's blog is that he pointed to GNUCITIZEN which is
a blog that I subscribe to.  However, I have over 75 unread entries and one
them is the very important one that this blogger became the victim of.  I could
have been a victim as well.

Here is the blog entry that described the attack:
[http://www.gnucitizen.org/blog/google-gmail-e-mail-hijack-technique/][2].
Gmail is vulnerable to Cross-Site Request Forgeries (CSRF).  CSRF is a way to
send requests to other sites through the victim.  In this case, a malicious
website sends requests to Gmail to create filters using the victim's
authenticated sessions.  The attacker doesn't need to know your login
information, the attacker simply needs you to be already logged in.

Quoting pdp: _"In an age where all the data is in the cloud, it makes no sense
for the attackers to go after your box. it is a lot simpler to install one of
these persistent backdoor/spyware filters. game over! they don’t own your box,
but they have you, which is a lot better."_

Microsoft is also doing many projects involving Web 2.0.  Engineers need to be
educated about these kinds of attacks so they can design to prevent them.  The
knowledge needs to be in their genes.

My TODO list is to read
[http://www.gnucitizen.org/blog/for-my-next-trick-hacking-web20][3] after my
current personal project, to take more trainings offered by Microsoft related
to security, to understand how to prevent CSRF when I plan to add some
Silverlight to my site.  What's yours?

I also read: [http://shiflett.org/articles/cross-site-request-forgeries][4]

  [1]: http://www.davidairey.co.uk/google-gmail-security-hijack/
  [2]: http://www.gnucitizen.org/blog/google-gmail-e-mail-hijack-technique/
  [3]: http://www.gnucitizen.org/blog/for-my-next-trick-hacking-web20
  [4]: http://shiflett.org/articles/cross-site-request-forgeries
