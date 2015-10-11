---
date: 2013-07-01 10:23:28
title: Introducing syncray (Backup to S3 or Manta)
tags: Mad Coding, S3, Amazon S3, Manta, Joyent Manta, DreamHost, DreamObjects, backup, cloud storage
---
**UPDATE:** Changed name to syncray, which is also cool!

My backup strategy involved just using external HD to put encrypted backup.
However, it's been bugging me that I didn't have an offsite backup for
some things that aren't on my SkyDrive. That's why I wrote [syncray][1] over the
past week whenever I had some time.


# syncray

syncray ~~csync (aka Cloud Sync)~~ is a script that encrypts data
locally, then synchronizes to one of the cloud storage providers. At this time
it supports [Amazon S3][3] (or services with compatible API such as
[DreamObjects][4]) as well as [Joyent Manta][2].

I was going to use [s3cmd][5] but it wasn't able to do both sync and encrypt at
the same time. To get what I wanted, I wrote syncray to keep track of what's been
uploaded using an extra metadata file. This way I can have the best of both
worlds: locally encrypted data and still only upload the files that changed
instead of the entire set.

One great benefit of writing my own script is that adding Joyent Manta support
was quite easy. The pricing for Joyent Manta is pretty good and the technology
is just awesome.


# Joyent Manta

In the beginning I was planning on dumping my data on DreamObjects because of
its lower pricing than Amazon. However, while I was writing syncray, I saw the
announcement for Joyent Manta and it blew me away.

Not only can you store data in the cloud, you can run commands directly on
those data in the cloud without pulling any data down locally first. You should
watch the [screencast][6] and it'll just become clear.

When deciding on cloud storage for images for AvidTap, I was comparing between
S3 and Azure. One of the reasons why I chose S3 is because I can hand out timed
policy allowing users to upload directly from the browser to AvidTap's S3
bucket. Unfortunately Joyent Manta doesn't support that yet, but hopefully
soon because that enables some really cool scenarios. For example, one can
develop an image server that can resize or operate on images without having to
host anything extra.

That's cool!


# Cloud Storage Pricing

syncray is hard-coded to store only 1 copy on Joyent Manta which makes its
storage cost $0.043/GB. I signed up for DreamObjects while they have the deal
for $0.04/GB. I only wish I had gotten in on DreamObjects while they had the
$0.02/GB pricing. That's only 1 cent per GB extra over Amazon Glacier and you
can access your data quickly!

In any case, both DreamObjects and Joyent Manta allows me to store data at
~$0.04 per GB which is cheaper than Amazon S3. This will be my offsite backup
strategy from now on.

  [1]: https://github.com/dannysu/syncray
  [2]: http://www.joyent.com/products/manta
  [3]: https://aws.amazon.com/s3/
  [4]: https://dreamhost.com/cloud/dreamobjects/
  [5]: http://s3tools.org/s3cmd
  [6]: http://vimeo.com/68920633
