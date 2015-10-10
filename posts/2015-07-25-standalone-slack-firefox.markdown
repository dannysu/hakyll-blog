---
date: 2015-07-25 00:32:03 PDT
title: Standalone Slack with Firefox
tags: Slack, Firefox, Linux Unix QNX, Miscellaneous
---
I've been using [Slack][1] via Chrome Apps so I can get an app shortcut in GNOME
and have a separate window. My main browser is Firefox though, so it's always
been annoying when people share links and I have to copy & paste to see it in
Firefox. I don't have to, but I prefer it. Especially when my teammates have a
bunch of problems with video chat in Chrome and I have none of those issues
using Firefox.

Yesterday Slack via Chrome Apps stopped working. It simply opens a new tab like
a regular website everytime I open it via the shortcut. That was enough to
finally push me to look for a pure Firefox solution, and I'm sharing the steps
here.

<br>

## **Make a Desktop App**

I followed the instructions [here][2] to create a dedicated profile for Slack.
I've included the steps here that have been modified to be specifically for
Slack.

**1. First run Firefox with a specific profile (the -P parameter):**
<pre><code class="bash">
> firefox --class=Slack -P slack -no-remote https://yourown.slack.com

</code></pre>

You should then see a window like the one below:  
<a href="//imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg4LEgVpbWFnZRj56vACDA" target="_blank"><img src="//imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg4LEgVpbWFnZRj56vACDA" class="centered"></a>

**2. Click "Create Profile"**

Then click the "Next" button on the wizard that comes up. You can then give it a
name. I used "slack" for my standalone profile.

**3. Start Firefox with the Profile**

 - Select the newly created profile
 - Uncheck "Use the selected profile without asking at startup"
 - Click "Start Firefox"

After these steps, you can now run the same command as step 1 and get a
standalone web app.

<br>

## **Make an Application Shortcut**

This step is optional, but makes it easy to launch slack unless you always want to
launch it from the command line.

Create a `slack.desktop` file in `~/.local/share/applications` directory with
the following content:
<pre><code class="ini">
#!/usr/bin/env xdg-open
[Desktop Entry]
Version=1.0
Terminal=false
Type=Application
Name=Slack
Exec=firefox --class=Slack -P slack -no-remote https://kash.slack.com
Icon=chrome-jeogkiiogjbmhklcnbgkdcjoioegiknm-Profile_1

</code></pre>

For `Icon=` I just reused what Chrome Apps used. If you don't have that, I guess
just look for a slack icon yourself and put the path to the file in there.

Finally refresh GNOME to get the shortcut to show up by pressing `Alt + F2` and
then type `r` as the command.

<br>

## **Hide Toolbars**

Lastly, hiding Firefox toolbars to make things really look like a standalone
app. Again, this is optional as well, but you probably want it.

Launch Slack standalone Firefox instance, then install the [Stylish][3] and the
[Hide Tab Bar With One Tab][4] addons.

Create a Stylish style with the following content:

<pre><code class="css">
@namespace url(http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul);

#urlbar-container { display: none !important }
#nav-bar {
  display: none !important;
}

#navigator-toolbox::after {
  display: none !important;
}

</code></pre>

<br>

That's it! Standalone Slack with Firefox.

  [1]: http://slack.com
  [2]: http://www.adercon.com/ac/node/38
  [3]: https://addons.mozilla.org/en-US/firefox/addon/stylish/?src=search
  [4]: https://addons.mozilla.org/en-US/firefox/addon/hide-tab-bar-with-one-tab/?src=ss
