---
date: 2012-08-24 08:57:32
title: Introducing Hash0
tags: Mad Coding, Security
---
**Update:** I just found out that Kitchener-Wilmot Hydro's website stores
passwords in plain text. Furthermore, Kevin Burke recently found [Virgin Mobile
not doing the right thing][12]. All the more reasons to use different passwords
and don't assume big companies know what they're doing.
<br>
<br>

It's a good idea to get strong passwords including random characters, numbers,
symbols and don't reuse the same password for different account. Here's a very
good [article][5] explaining why.

With that in mind, I'm introducing [Hash0][6] - A password generator using a mix of
[PasswordMaker.org][4], [CryptoJS][7] HMAC-SHA512, [SJCL][9], and Google
App Engine. Also inspired by [ZeroBin][10]'s idea of encrypted data that only the
client can decrypt.

<br>

## **About**

Hash0 has these improvements over existing hash-based password generators:

1. Unique salt for each account to better protect the master password
1. Ease of use via synchronization of account settings such as password length,
   allowable characters, salt, etc
1. Encrypted account settings similar to storing encrypted data via ZeroBin
1. Uses HMAC-SHA512 (Update: Used with PBKDF2)

Account settings are stored in the browser's local storage and also at a
provided URL. At any new computer you need to generate password, you can grab
the account settings from your storage URL so that synchronization is taken
care of. The account settings are encrypted before sending to server so that
the server can't read any of it. Master password and generated passwords are
never saved anywhere. Except the generated encryption password which is also
stored in browser's local storage.

To begin using hash0, you first type in a password and a URL where you want to
store your account settings. The password is used to generate an encryption
password and can be the same as your master password. You can checkout my [GAE
storage][11] project for setting up a place to store account settings.

To generate password Hash0 takes several parameters:

1. A boolean flag whether to include symbols or not
1. Password length
1. A parameter (usually website domain)
1. A number to append to the parameter (allows generation of new password
   easily)
1. A salt unique for each account
1. Your master password

Password generation process:

1. Decrypt account settings using generated encryption password and provided
   URL
1. Generate a new salt if this is for a new account
1. Reuse existing account settings (length, include symbols?, number, etc) if
   any
1. (Update: PBKDF2 100 iterations of HMAC-SHA512 to generate key)
1. CryptoJS.HmacSHA512(param+number, <del>salt+master</del> key)
1. <del>100 rounds of HMAC-SHA512 based on previous output and the salt</del>
1. Conversion to desired set of allowable characters
1. Truncate to desired length
1. Encrypt account settings and store it at the provided URL

<br>

## **Background**

I went through couple phases in terms of how I deal with my passwords. In the
beginning I had just couple passwords for all my accounts. One for important
accounts and one for accounts I don't really care about. This of course isn't
secure, so I started using [KeePass][1] to store strong passwords unique for
each site. There are some downsides to KeePass though. It's written in .NET and
the UI through mono is terrible on Linux and Mac. For this reason I was never
satisfied and was looking for other solutions.

Recently I found out about [hashapass][2], [pwdhash][3], and
[PasswordMaker.org][4]. I like the simplicity of this solution, but was afraid
of simply using hash function for protecting my master password. Furthermore,
after the [LinkedIn password hashes leaked][8] I had to change the password
generation to use "LinkedIn.com2" and felt that remembering different numbers
for various sites too annoying.

  [1]: http://keepass.info/
  [2]: http://www.hashapass.com/
  [3]: https://www.pwdhash.com/
  [4]: http://passwordmaker.org/
  [5]: http://arstechnica.com/security/2012/08/passwords-under-assault/
  [6]: https://github.com/dannysu/hash0
  [7]: http://code.google.com/p/crypto-js/
  [8]: http://arstechnica.com/security/2012/06/8-million-leaked-passwords-connected-to-linkedin/
  [9]: http://crypto.stanford.edu/sjcl/
  [10]: http://sebsauvage.net/paste/
  [11]: https://github.com/dannysu/gae_storage
  [12]: http://kev.inburke.com/kevin/open-season-on-virgin-mobile-customer-data/
