---
date: 2013-01-13 22:57:06
title: Node.js Test Frameworks
tags: Start-up, Node.js
---
So far working on my own start-up has been interesting. Plans and approaches
change as we learn more and talk to more people. Exactly what we're building
have also changed during the course of the last several months, but I'm happy to
say that we're closer to launch now. With this post I want to highlight the role
of some test frameworks used with Node.js that got us here.

I believe that testing is essential in any environment, and I'm surprised there
is still debate about it. Everybody thinks coding without source control is
ridiculous. I think coding without *consciously* decide whether to test is equally
ridiculous. It should be part of your arsenal to shipping code fast. Bring it
out when it makes sense.

When I wrote the APIs that our mobile app calls, I made sure to seek out the
testing frameworks available for Node.js. This is a core part of what the
company would be based on, so it made sense. I first started with [Vows.js][2]
and that worked fine for a while. However, I ran into issues when trying to
cascade some of the tests. Aside from that issue though, I really liked the test
framework.

Later at the suggestion of my co-founder Geoff Flarity, I checked out
[Mocha][1]. It's another test framework but I found Mocha works much better than
Vows.js. Furthermore, it's so cool to be able to pick out report style you want
to use. I can nest my tests just fine unlike Vows.js. With Mocha, I also use
[Should.js][3] to make assertions.

By trying to write tests for my code, it forced me to separate authentication
from actual business logic. It also forced me to separate web server logic from
core business logic. The separation of web server logic from business logic
means that I'm able to reuse code in my command line scripts with no effort at
all. It also means that testing anything is effortless. I'm able to set up the
simulation environment by piecing together components without being tied up in a
giant ball of mud. The separation of authentication from business logic is one
that made even more impact.

During one of the founder meetings we'd hold at my co-founder Kaz Nejatian's
house, we made a strategic decision on what we'd build exactly. The decision
made there meant that the way I was doing authentication from the mobile app
needed to change. The Node.js APIs would also need changing. However, because I
already have tests in place and already forced myself to separate
authentication, making the switch wasn't a big deal. Because the tests are in
place I'm able to adapt to changes in the business faster and don't waste time
on getting things to work properly again.

I can already see how I will benefit even more from the componentized design
arising from trying to write test. Based on feedback from talking to some VCs,
we're once again going to be tweaking the authentication piece a bit to test out
a different business model. This would be in parallel with our old plan, but in
the span of couple months tests have already proven to be useful. As we learn
more about how to sell and what people want, I expect even more changes to come
and we'll need to react rapidly. I know the code is ready. Bring it on!

  [1]: http://visionmedia.github.com/mocha/
  [2]: http://vowsjs.org/
  [3]: https://github.com/visionmedia/should.js/
