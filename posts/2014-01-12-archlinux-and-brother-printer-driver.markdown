---
date: 2014-01-12 10:30:20
title: Lenovo E431 and Brother HL-3045CN Printer with Arch Linux
tags: Arch, Linux/Unix/QNX, Brother, Printer, GNOME, Lenovo, E431
---
For the past year I used my own monitor from home at work and tried to make
do with my 6-year old ultraportable whenever I had to be out and about. However,
it's now time to upgrade. I ordered a Lenovo E431 laptop and a 24" monitor
during last year's Black Friday sale.

[Ubuntu][4] has been my Linux distro after my years with [Gentoo][3]. I
installed Ubuntu 13.10 first on the laptop and found that after installation the
touchpad did not work. I then tried 12.04 LTS and found the same issue. I had
been reading up on [Arch Linux][2] and decided to give it a go at this point.

<br>

## **Arch Linux Installation**

Installing Arch is pretty easy in my opinion. Perhaps it's because I've been
through the even more difficult installation process for Gentoo back in the
days. For Arch, I found the [Beginner's Guide][5] to be quite good. I just
followed the instructions there, got my wifi up, and installed the rest of the
system.

Since I don't have the choice of Unity UI from Ubuntu, I decided to give [GNOME
3][6] another try. Turns out I love it even more than Unity! I particularly like
the [put windows extension][7] which allows me to quickly tile my windows
however I want.

Aside from the base, I also added:

- Numix theme

<pre class="brush:bash">
pacman -S numix-themes
</pre>

- Extensions: [Topicons][10], [Lock screen][9], [Frippery move clock][8]

<br>

## **Trackpoint on Lenovo E431**

My touchpad worked after installation and supports multi-finger scroll just
fine. The trackpoint (the red little nob in middle of keyboard) also worked
fine. However, the right click button and the middle click button for scroll
didn't work out of the box.

I am a vim user so I really appreicate the trackpoint and not having to move my
hand very much. I wanted to get the trackpoint fully working. Lucky someone has
done the hard work.

On Arch, you have to install [xf86-input-evdev-trackpoint][11] from AUR and
configure things similar to [LimaSierra's post][12] on the forums.

<br>

## **Brother HL-3045CN printer on Arch**

At work we have a Brother HL-3045CN printer, to get it to work I had to follow
the [wiki for cups][13] and install the Linux driver from [brother.com][1].

I did the install at work, so didn't have time to package it all nice in a
PKGBUILD yet. Nevertheless, there are the instructions.

1. Make sure you have [rpmextract][14] installed
   <pre class="brush:bash">
   pacman -S rpmextract
   </pre>
1. Download the rpm files from [brother.com for HL-3045CN][1]
1. Run rpmextract.sh on the rpm files
   (You should end up with a **opt** and a **usr** folder afterwards)
1. Move the content in those directories to the same places on /
1. Then run the cupswrapperhl3045cn script
   <pre class="brush:bash">
   /opt/brother/Printers/hl3045cn/cupswrapper/cupswrapperhl3045cn
   </pre>
1. After this you should be able to enter cups web interface and add the printer
   just fine

<br>

## **Conclusion**

I love my new Linux setup. I'm also writing this blog post on the new laptop
which means I have Haskell setup as well. In fact, I like my new Arch setup more
than my current Ubuntu 12.04 install. I think it's time to switch things up at
home as well.

  [1]: http://welcome.solutions.brother.com/bsc/public_s/id/linux/en/download_prn.html
  [2]: https://www.archlinux.org
  [3]: http://www.gentoo.org
  [4]: http://www.ubuntu.com
  [5]: https://wiki.archlinux.org/index.php/Beginners%27_Guide
  [6]: https://www.gnome.org
  [7]: https://extensions.gnome.org/extension/39/put-windows/
  [8]: https://extensions.gnome.org/extension/2/move-clock/
  [9]: https://extensions.gnome.org/extension/83/lock-screen/
  [10]: https://extensions.gnome.org/extension/495/topicons/
  [11]: https://aur.archlinux.org/packages/xf86-input-evdev-trackpoint/
  [12]: https://bbs.archlinux.org/viewtopic.php?pid=1359488#p1359488
  [13]: https://wiki.archlinux.org/index.php/Cups
  [14]: https://www.archlinux.org/packages/extra/any/rpmextract/
  [15]: https://aur.archlinux.org/packages/haskell-platform/
