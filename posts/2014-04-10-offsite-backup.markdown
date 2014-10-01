---
date: 2014-04-10 19:16:46
title: Setting up my file sync & offsite backup
tags: Miscellaneous, Linux Unix QNX, Dropbox, SpiderOak, BitTorrent Sync, Backupsy
---
**UPDATE:** Found out about the [encfs security audit][9] by Defuse Security. I
might have to revise the encryption I use.

**UPDATE 2:** I'm now using [duplicati][10] & Backupsy. Duplicati does
incremental backup + encrypts the data via standard gpg.
<br>

I didn't have a good offsite backup for my data for quite some time. I've played
with all sorts of file sync options such as [box.com][1], [copy.com][2],
[Dropbox][4], but they all leave my data unprotected. I then read about
[encfs][6] a while ago and it sounded great. Using EncFS, I can synchronize only
the encrypted data to any of the cloud storage providers and not have to worry
that they can read my data. So now the question is which provider to use.

<br>

## **Bad Experience with SpiderOak**

From this year's World Backup Day, I found out about [SpiderOak][3]. They were
offering a $125 / year with unlimited storage offering. I downloaded their Linux
client to check out the features and it looked great. You get actual backup
capabilities that stores old versions of anything you backup. On top of that you
also get file sync ability like Dropbox. They also do client encryption before
sending data to their server, but I didn't care about that at all since I can
only trust myself in terms of encrypting my data anyway. About 4 hours before
SpiderOak's offering expires, I signed up for an account. Unfortunately I found
that backing up data to their server to be extremely slow even on my office's
10Mbps up connection. What makes it even worse is that while
backup is happening (very slowly), file sync gets queued and becomes
useless. I ended up cancelling my account 2 days later.

<br>

## **Backupsy + BitTorrent Sync + rdiff-backup + anacron**

After SpiderOak, I figured what I really need is just a VM that has a lot of
storage. I wondered whether any existed and found [Backupsy][7]. I signed up for
the $6 / month package with 250 GB storage.

I like Backupsy because they let me install Arch Linux, which allows me to
install only things I need on the VM.

This is my file sync + backup setup now:

- Backupsy VM running Arch Linux and located somewhere outside Canada
- Locally encrypt files using encfs or Truecrypt
- Use rdiff-backup scheduled with anacron to do incremental backup to Backupsy VM
- Use [BitTorrent Sync][8] to synchronize encrypted files across machines
- Use the two external hard-drives I have at home for local backup (also
  encrypted)

This setup is not easy for non-technical user to implement but is better than
Dropbox or similar services in my opinion. I get some sort of versioning from
using rdiff-backup, and I get everything I need from Dropbox by using BitTorrent
Sync. Everything is encrypted before leaving any of my machines of course.

Since this is fairly involved, so it's not suitable for everyone. I wish there's
a way to provide the same thing to everyone, but unfortunately as long as you
don't control the encryption, you can't be sure your data is safe.

<br>

## **What if the house burns down?**

There is one last piece of the puzzle. In order to access Backupsy, I use my ssh
key to do that. What if my apartment burns down and I lose all of my
computers and external hard-drives? I need somewhere offsite to store my ssh key
as well as the encryption key used for my data.

Some people put those things in their safety deposit box. However I find that
rather annoying to do because you have to physically go to the bank to access
it. Right now my approach to that is to use the file storage on FastMail to
store those things. Again, encrypted before I put it on FastMail.

  [1]: http://box.com
  [2]: http://copy.com
  [3]: http://spideroak.com
  [4]: http://dropbox.com
  [5]: http://bitcasa.com
  [6]: https://en.wikipedia.org/wiki/EncFS
  [7]: http://backupsy.com
  [8]: http://www.bittorrent.com/sync
  [9]: https://defuse.ca/audits/encfs.htm
  [10]: http://duplicati.com
