---
date: 2012-02-04 13:53:17
title: Well Designed Software and Testing
tags: Mad Coding, Microsoft, Andrew Hunt, Chris Hartjes, David Thomas, Martin Fowler, Michael Feathers, software design, Software quality, Well.ca
---
One of the things I've been doing at Well.ca is to lead by example. I started
the weekly tech talks, wrote tests for the tickets or features I worked on, and
recently I compiled all of that and gave a talk on testing. I'm very glad to
have worked at Microsoft and particularly have been surrounded by teammates &
mentors who helped me get better.

It's frustrating at times when people don't understand the value of good design
and testing. They're perfectly happy to keep fighting fires and think that is
doing good work. Nobody forced me to write unit tests at Microsoft, but I saw
the value and have experienced first hand how writing tests gives me more time
to innovate.

In my tech talk to the team, I talked about why one would want to write tests.


> "Any fool can write code that a computer can understand. Good programmers
> write code that humans can understand." - Martin Fowler

One of the things people don't seem to realize is that attempting to write
tests actually helps you with software design. Having to write tests means that
you cannot have software that's so intertwined that you can't easily extend,
re-use, refactor, or test. If you want to see badly designed software, simply
check out [zen cart](http://www.zen-cart.com/). The software is a mess with a
lot of dependencies built in such that it's impossible to test or reuse code
for other scenarios.

Anybody can hack together some prototype code, but it takes skill and
experience to build well designed software.

I gave some more quotes in my talk to illustrate my point:

> "Test ruthlessly and effectively"  
> - "The Pragmatic Programmer" by Andrew Hunt and David Thomas

> Legacy code = code without tests  
> - "Working Effectively with Legacy Code" by Michael Feathers

The rest of my talk involved demonstrating how I have been writing tests at
Well.ca (with PHPUnit, QUnit, etc) to show my teammates that it can be done and
makes things better.

I'm currently leading the warehouse software at work, and I've got two junior
developers on the project with me. I've been stressing to them to think about
design before coding and write tests. The people who worked on the code before
don't think about design and don't attempt to break things up into logical
units. The end result is multiple logical components all modifying the same
database table and code duplication is rampant. Of course, tests are nowhere to
be seen and are done entirely via manual tests through the browser. It's no
wonder that the progress has been slow and each new feature involves fighting
fires. That's where I come in and hope to change the culture around to do
better.

Last month I attended a Guelph PHP User Group meeting and Chris Hartjes was the
speaker for that time. He talked about the future of PHP as well as things
covered in his book [The Grumpy Programmer's Guide To Building Testable PHP
Applications][1]. During the talk I found that he and I share a lot of common
beliefs about good software design and testing. During the meeting I told the
group how I've been bringing up Well.ca's code quality by introducing
[Phabricator](http://phabricator.org/) to them. I also learned from Chris about
PHP BDD framework [Behat](http://behat.org/). I was lucky to win a copy of his
book and have just recently finished reading it. It's a great read and gives me
pointers to other things I can check out and possibly introduce to Well.ca.

  [1]: http://www.littlehart.net/atthekeyboard/2012/01/03/grumpy-testing/
