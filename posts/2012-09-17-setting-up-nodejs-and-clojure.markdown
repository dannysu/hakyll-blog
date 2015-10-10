---
date: 2012-09-17 12:15:04
title: Setting up Node.js and Clojure
tags: Mad Coding, Clojure, Node.js
---
I've been playing with Node.js and Clojure recently. Below are the steps to get
each of them up and running.

## **Setting up Node.js**

The easiest way I've found so far is to use nvm.

1. Get nvm (Node Version Manager)

<pre><code class="bash">git clone git://github.com/creationix/nvm.git ~/apps/nvm</code></pre>

2. Source nvm.sh in bash or zsh

<pre><code class="bash">source $HOME/apps/nvm/nvm.sh</code></pre>

3. Install the version of node you want

<pre><code class="bash">nvm install v0.8.9</code></pre>

4. Select the version of node you want

<pre><code class="bash">
nvm use v0.8.9
node --version

</code></pre>

That's it! From then on just write your js scripts and run them with node.

You can of course install from source or use package manager if that works for
you. For vagrant, there's also the [nodejs cookbook][1].
<br>
<br>

## **Setting up Clojure**

clojure.org should have a better [Quick Start][2] guide. [Some people][3] say that
Node.js is easier to set up and I agree. However, I also expect any good
developer to be able to set up their tools, especially if the benefits of the
tool outweighs the initial hurdle.

I'm using Ubuntu on my machine, and the latest package for clojure on Ubuntu
12.04 is 1.3.0. If that's fine for you then it's simply an `apt-get install
clojure1.3` to get started.

The latest version of Clojure right now is 1.4.0 and I wanted the latest, so
here's how I got it set up.

1. Install JDK  
  
Apparently JDK is already installed on my machine, but if it's not on your then
install it by:

<pre><code class="bash">apt-get install openjdk-6-jre-headless</code></pre>

2. Get a helper script to help run the java command with proper classpath  
  
I modified the Ubuntu one from clojure1.3 package to look for clojure1.4 jar and
also include \*.jar in `$HOME/apps/jars` directory. The good thing about this
script is that I can now use whatever version of clojure I want. I saved the
file as `clj`.

<pre><code class="bash">
#!/bin/sh

if [ "x$CLASSPATH" = "x" ] ; then
	extra_classpath=""
else
	extra_classpath=":$CLASSPATH"
fi

while true ; do
	case "$1" in
		-cp | -classpath)
			extra_classpath=":$2"
			shift 2 ;;
		--)
			shift
			break ;;
		*)
			break ;;
	esac
done

if [ "x$1" = "x" -a "x`which rlwrap`" != "x" ] ; then
	rlwrap="rlwrap -r -c -C clojure -f /etc/rlwrap/clojure1.4 -b (){}[],^%\$#@\"\";:''|\\"
fi

for i in `ls $HOME/apps/jars/*.jar`
do
    included=${included}:${i}
done

exec $rlwrap java -cp $HOME/apps/jars/clojure-1.4.0.jar:.:"$included""$extra_classpath" clojure.main "$@"

</code></pre>

3. Grab clojure jar from [http://clojure.org/downloads][4] and put the jar in
   `$HOME/apps/jars`

4. Run Clojure

<pre><code class="bash">clj</code></pre>

For any additional library you want to use, you can either pass the -cp argument
or simply drop the jar files into `$HOME/apps/jars`. E.g. I put
data.json-0.1.3.jar and math.numeric-tower-0.0.1.jar in there.

An alternative method is to use [Leiningen][5], but that's more useful IMO for
setting up projects. If I want to quickly write a script and run it, I prefer my
clj script.
<br>
<br>

## **Setting up VIM**

For VIM users, install [VimClojure][6] and [vim-slime][7] will make editing
Clojure code much easier. After installing VimClojure, put the following in
.vimrc:

~~~
autocmd BufEnter *.cljs set filetype=clojure
let g:vimclojure#HighlightBuiltins = 1
let g:vimclojure#ParenRainbow = 1
~~~

For vim-slime, I'm using tmux configured via .vimrc:

~~~
let g:slime_target = "tmux"
~~~

Node.js also benefits from vim-slime because you can then send javascript code
to your node REPL.

Screenshot of my setup:  
![](http://imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3JlcgwLEgVpbWFnZRjxLgw)


  [1]: http://community.opscode.com/cookbooks/nodejs
  [2]: http://clojure.org/getting_started
  [3]: http://arnorhs.com/2011/03/02/why-is-node-js-becoming-more-popular-than-clojure/
  [4]: http://clojure.org/downloads
  [5]: https://github.com/technomancy/leiningen
  [6]: https://github.com/vim-scripts/VimClojure
  [7]: https://github.com/jpalardy/vim-slime
