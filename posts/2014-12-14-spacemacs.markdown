---
date: 2014-12-14 19:45:04 PST
title: Trying out Spacemacs
tags: Mad Coding, Vim, Emacs, Spacemacs
---
I just might convert to using Emacs + Evil after using Vim for ages. I recently
learned about [Spacemacs][1] while reading stuff on Hacker News. Spacemacs puts
together a nice Emacs package already set up for Vim users to feel pretty
comfortable. Evil mode looked interesting to me because I much prefer Vim's
editing method than a bunch of C-x or M-x that takes my hands off the home row.

<br>

## **Getting Started With Spacemacs**

[Installation][2] for Spacemacs is easy. Just follow the installation
instruction and git clone to ~/.emacs.d. On Arch Linux, installing Emacs is also
easy:

<pre><code class="bash">
pacman -S emacs

</code></pre>

<br>

I mainly do my editing in my terminal rather than using the GUI version of
Vim. To get the same thing with Emacs you would run it with the -nw parameter:

<pre><code class="bash">
emacs -nw

</code></pre>

With Spacemacs and perhaps Emacs, the command line interface looks fairly ugly
so I might start using the GUI version.

<br>

Moving around in Evil mode feels comfortable with its modal editing and keys
already set up to mimic Vim. Below I detail some of the things I used to do in
Vim and how to now do them with Emacs/Spacemacs.

<br>

## **Hard Wrap to 80 Characters**

When I write blog entries I typically will enable hard wrapping in Vim to 80
characters via:

<pre><code class="bash">
:set textwidth=80

</code></pre>

Then I might highlight lines and reformat them with <font color="red">gq</font>
if I had written things prior to setting textwidth or I need to trigger hard
wrap manually.

With Emacs, hard wrapping is called "filling". This [page][3] was helpful for me
in getting the right behaviour out of Emacs.

In Emacs, to get the same behaviour, you'll want to turn on
auto-fill-mode. Typically this is done in Emacs with:

<pre><code class="bash">
M-x auto-fill-mode

</code></pre>

With Spacemacs, <font color="red">M-x</font> is mapped to <font color="red">SPC
:</font> where <font color="red">SPC</font> means the space bar. So to turn
auto-fill on or off, you would type:

<pre><code class="bash">
SPC : auto-fill-mode

</code></pre>

Afterwards to set the textwidth in Emacs you would do:

<pre><code class="bash">
SPC : set-fill-column

</code></pre>

It'll ask you for the column width and you can type in 80, for example.

To manually trigger hard wrap, the shortcut in Spacemacs is the same as in Vim.

<br>

## **Saving File**

In Vim, I had setup shortcut so that I can type <font color="red">s a v</font>
to save a file (instead of the typical <font color="red">:w</font>). With
Spacemacs, you can do <font color="red">SPC f s</font> to do the same
thing. Equally fast and ergonomic. Fingers not moving from home row at all.

<br>

## **Narrow Region**

With Vim, I installed the [NrrwRgn][4] plugin which is actually for mimicking
the narrowing feature from Emacs. In Vim, I had my leader key mapped to comma,
so to trigger NrrwRgn I would highlight lines and then type <font color="red">,
n r</font>. After doing my editing, you just have to type <font
color="red">:q</font> out of the narrow region.

With Spacemacs, doing the same thing involves typing <font color="red">SPC n
r</font> to get the narrow region and then <font color="red">SPC n w</font> when
you're done. Pretty awesome.

<br>

## **Command-T or ctrlp.vim**

Another plugin that I've been using is the [Command-T][5] plugin. It allows me
to open files just by doing &lt;Leader&gt;t in Vim and then do a fuzzy search on the
file name.

In Spacemacs it's also pretty easy:

<pre><code class="bash">
SPC p f

</code></pre>

<br>

## **Tern**

One down side of using text editors is that you miss out on IDE features
sometimes. For example, when developing node.js projects [WebStorm][7] offers really
good autocomplete and debugging features. WebStorm's VIM plugin doesn't work all
that well though, and sometimes just stops working until I restart the IDE.

[Tern][6] is an open-source project that brings javascript autocomplete
capability to other text editors, but it never worked well for me in Vim. It
worked quite well in Emacs though since its integration with Emacs come
built-in. Below is my .spacemacs config that enables it.

<pre><code class="clojure">
(defun dotspacemacs/config ()
  "This is were you can ultimately override default Spacemacs configuration.
This function is called at the very end of Spacemacs initialization."
  ;; Load Tern
  (add-to-list 'load-path "/home/danny/.dotfiles/emacs/tern/emacs/")
  (autoload 'tern-mode "tern.el" nil t)
  (add-hook 'js-mode-hook (lambda () (tern-mode t)))
  (eval-after-load 'tern
    '(progn
       (require 'tern-auto-complete)
       (tern-ac-setup)))
)

</code></pre>

I just followed the Emacs [install instructions][8] on Tern website and git clone to
a directory of my choosing.

  [1]: https://github.com/syl20bnr/spacemacs
  [2]: https://github.com/syl20bnr/spacemacs#install
  [3]: http://johnlaudun.org/20080321-word-wrap-filling-in-emacs/
  [4]: https://github.com/chrisbra/NrrwRgn
  [5]: https://github.com/wincent/Command-T
  [6]: http://ternjs.net/
  [7]: http://www.jetbrains.com/webstorm/
  [8]: http://ternjs.net/doc/manual.html#emacs
