---
date: 2013-12-15 20:08:43
title: New happenings at AvidTap
tags: Start-up, AvidTap, Testing, Robotium
---
A quick update on what's happening at [AvidTap][1]. We're busy delivering tons
of awesome features for our Saladworks launch next year. We're moving to a new
office location in the new year that'll give us more space.


# New Hire

We've made our latest hire to bring #6 onboard. When I decided that I was
going to start a company, one of the things I wanted to do was to hire people
better than me. For no other reason than being on a team of smart people will
elevate my game. Back in university, I was really fortunate to share a house
with many individuals who are better and smarter than me. I was also very
fortunate to partner with many awesome people for AI course and CS real-time
course. Being around that environment challenged me to do better and set my
standards high.

We have an awesome team right now and I hope to make more great hires in the
future. Everyone has their strong suit and complementary skills are needed to
execute.


# Testing

Within the past month we've made great strides to catch up on our testing
infrastructure for AvidRegister. Due to initial time pressure we didn't have any
automated testing in place. Everything has been manual and adhoc. This is less
and less sustainable if we want to keep the quality up. Our test matrix is very
time consuming to go through since there's testing needed for new AvidRegister
version with new API version, old AvidRegister upgrade scenario and old
AvidRegister version with new API.

On the other hand, our API project test coverage is great. I made sure to get
that correct right off the bat. Large refactoring is easy because of tests in
place. I can also simulate basically anything that the system might do easily.

For AvidRegister testing, I looked at a variety of things. I tested out
[monkeyrunner][2], the new [Espresso][3] framework, and [Robotium][4]. I settled
on Robotium due to a combination of speed and maturity.

In order to enable end-to-end testing, I also re-factored common API calls in
the AvidRegister web dashboard to be re-usable. What this means is that I can do
things the same way that the web dashboard does, but do it inside a test.

Putting these things together the dev team now has the ability to fully automate
end-to-end everything from logging in on AvidRegister on tablet to validation on
web dashboard.

Getting this in place is crucial for the quality of the product and for
efficiency to do more with less resources.

  [1]: http://avidtap.com
  [2]: https://developer.android.com/tools/help/monkeyrunner_concepts.html
  [3]: https://code.google.com/p/android-test-kit/wiki/Espresso
  [4]: http://code.google.com/p/robotium/
