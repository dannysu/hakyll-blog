---
date: 2016-01-11 00:10:00 PST
title: Free Uptime Monitoring with Google App Engine
tags: Google App Engine, GAE, uptime, Mad Coding, StatusCake
---
Pingdom recently sent out an email notification telling users that their free
monitoring tier will no longer feature 1-minute check interval. That's too bad
because I've been using the free tier for my personal blog. When I needed
something for my company I was happy to pay Pingdom for the higher tier.

The new free tier from Pingdom only allows for 5-minute check intervals. Pretty
standard compared to what others are offering. E.g. StatusCake.com

I don't need the 1-minute intervals for personal blog, but I do like it.
However, it really doesn't make sense for me to pay $14/month for personal
stuff. For that amount of money, I could get at least 3 servers that can do
anything I want aside from monitoring.

I had a bit of time on Sunday morning so I coded up something quick that can be
deployed to Google App Engine and run for free. See [GitHub][1] for source
code.

You basically update `app.yaml` to reflect your project name and then create a
`env.py` containing all the variable values you want to set. For a list of env
variables, see `main.py`.

The `cron.yaml` tells Google App Engine to query `/check` endpoint every minute.
That endpoint will check the target site and notify you via email if it goes
down or if it comes back up. There are no other exposed endpoint so it should
only use up 24 of the 28 allowed instance hours. All the other quota limitations
are way higher than what's needed for this super simple service.

This won't check the site from multiple regions but for personal sites it's
just right.

  [1]: https://github.com/dannysu/uptime-gae
