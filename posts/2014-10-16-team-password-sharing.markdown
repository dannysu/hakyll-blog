---
date: 2014-10-16 19:42:19 PDT
title: Team Password Sharing
tags: Security, Kash, Start-up
---
At [Kash][1], we use [BitTorrent Sync][2] along with the command line utility
[pass][3] to share passwords among team members. We already use OpenPGP for
secure emails, so everyone had their PGP key generated already.

Currently the only none coder person is Kaz, but he has this setup going as
well.

<br>

## **Step 1: Get PGP Key**

Your first step is to make sure everyone has PGP key, which you probably should
do anyway so that you can send each other secure emails.

If you're on a Mac, follow instructions here:
[http://notes.jerzygangi.com/the-best-pgp-tutorial-for-mac-os-x-ever/][4]
 
If you're on Linux, follow instructions here:
[https://www.digitalocean.com/community/tutorials/how-to-use-gpg-to-encrypt-and-sign-messages-on-an-ubuntu-12-04-vps][5]

Bonus: [Mailvelope][6] is a Chrome/Firefox extension that makes sending and reading
encrypted emails in Gmail super easy.

<br>

## **Step 2: Install Synchronization Software**

We currently use [BitTorrent Sync][7], but any syncing software will do.
E.g. [AeroFS](https://www.aerofs.com/), [Dropbox](https://www.dropbox.com/),
[SpiderOak](https://spideroak.com/), [Pulse](https://ind.ie/pulse/) (formerly
SyncThing), etc.

I personally like sync solutions that don't involve someone else's central
server. Even if I use something that's only p2p, I'd still only put encrypted
stuff through it.

<br>

## **Step 3: Install pass**

Install [pass][3], which will require you to install Homebrew if you're using
a Mac.

<br>

## **Step 5: Trust Team Members' PGP Key**

After installing pass, you can now use it to manage passwords. Before you can
encrypt passwords for other people to see, you need to make sure you have PGP
public key from everyone on the team. We already have that since we use PGP for
secure email.

Also you have to mark team members' public key as trusted.

On Linux you can:
<pre><code class="bash">
gpg --list-keys
gpg --edit-key [ID of the key]
gpg> trust
Your decision? 5
gpg> save

</code></pre>

On Mac, the UI tool allows you to do that. Just look under info.


## **Step 6: Create Password and Folder**

To initialize a folder to store passwords, it's just a simple command: `pass
init recipient1 recipient2 ...`. See `pass help` for more details. The list of
recipients are the people who'll be able to decrypt files in order to see the
passwords.

Use `pass generate [something descriptive] [password length]` to start
populating the password folder with things you want to share.

Then to use it, `pass show -c [something descriptive]` to copy a password to
clipboard.

## **Step 7: Sync Password Folder**

pass creates password folder under ~/.password-store. You'll then need to sync
that folder with people on the team using whatever sync solution you settle on.

<br>

So there it is. That's how we share team passwords securely at Kash for now.
When the team gets bigger, then we'll see if this holds up. But for now, this
is all doable with free and open-source tools.

  [1]: http://www.withkash.com
  [2]: http://www.bittorrent.com/sync/
  [3]: http://www.passwordstore.org/
  [4]: http://notes.jerzygangi.com/the-best-pgp-tutorial-for-mac-os-x-ever/
  [5]: https://www.digitalocean.com/community/tutorials/how-to-use-gpg-to-encrypt-and-sign-messages-on-an-ubuntu-12-04-vps
  [6]: https://www.mailvelope.com/
  [7]: http://www.bittorrent.com/sync/download
