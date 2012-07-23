---
date: 2004-03-06 14:14:08
title: why oh why? why fbsd?
tags: FreeBSD
---

fbsd simply rules for server environment

the only reason I'm not using fbsd is because there's better hardware support in Linux. Things like my wacom tablet only has Windows & Linux drivers :(

While the above reason prevents me from using fbsd for desktop purpose... for some others, fbsd is enough and has very good features tailored to servers.

So have you ever wanted to simulate ADSL link to the moon? FreeBSD has what's called DUMMYNET.
DUMMYNET is originally aimed to simulate network delay, packet loss, etc for network protocol testing.
Since then it has also been used by normal users and servers to manage bandwidth.

DUMMYNET is built into fbsd kernel and is used through fbsd's ipfw (IP Firewall) command.

While I think Linux has similar stuff from NIST or something, it's pretty outdated...
That is, there's no version for 2.6 kernel that I'm using. It may work but I'm not sure. However, one thing I do know is that, FreeBSD's ipfw & dummynet is part of the kernel. This means that they will be there even if you update to the newest -CURRENT. :)

another + for FreeBSD :D
