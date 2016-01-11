---
date: 2016-01-11 00:10:00 PST
title: Free Uptime Monitoring with Google App Engine
tags: Google App Engine, GAE, uptime, Mad Coding
---
Pingdom recently sent out email notification that their free monitoring tier
will no longer feature 1-minute check interval. That's too bad since I've been
using their free service for personal sites, and so when I needed something for
the company I was happy to pay them for higher tier.

However, it really doesn't make sense for me to pay $14/month for personal
stuff. For that amount of money, I could get at least 3 servers that can do
anything I want aside from monitoring.

I had a bit of time on Sunday morning so I coded up something quick that can
deploy to Google App Engine and run for free. See [GitHub][1] for source code.

You basically update `app.yaml` to reflect your project name and then create a
`env.py` containing all the variable values you want to set. For a list of env
variables, see `main.py`.

The `cron.yaml` tells Google App Engine to query `/check` endpoint every minute.
That endpoint will check the target site and notify you via email if it goes
down or if it comes back up. There are no other exposed endpoint so it should
only use up 24 of the allowed 28 instance hours. All the other quota limitations
are way higher than what's needed for this super simple service.

  [1]: https://github.com/dannysu/uptime-gae
