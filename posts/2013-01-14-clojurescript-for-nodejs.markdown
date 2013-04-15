---
date: 2013-01-14 11:35:23
title: Getting Started With ClojureScript For Node.js
tags: Start-up, Node.js, Clojure, ClojureScript, Mad Coding
---
**UPDATE:** See also [Restify With ClojureScript][3]

Here is a [video][1] of Rich Hickey introducing ClojureScript. It's interesting
that Clojure can run on different runtimes. Being on JVM definitely has some
downsides such as the longer start up time.

I've been trying to tinker with using ClojureScript for Node.js but for whatever
reason always ran into issues. Probably bad luck in terms of the versions I was
using. Today I finally got a Hello World program to compile aftering solving
some errors by updating versions, so will detail the steps in this post.

1. Grab Leiningen 2.0.0 RC2 from [github][2]
1. From command line, run `lein`. This will download leiningen jar to
   `.lein/self-installs`.
1. Go to .lein and create a profiles.clj file with the following content
<pre class="brush:clojure">
{:user {:plugins [[lein-cljsbuild "0.2.10"]]}}
</pre>
1. From command line, run `lein new cljs-helloworld`
1. `cd cljs-helloworld`
1. Edit project.clj file to add :cljsbuild parameters
<pre class="brush:clojure">
(defproject cljs-helloworld "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :cljsbuild {
              :builds [{
                        :source-path "src"
                        :compiler {
                                   :target :nodejs
                                   :optimizations :advanced
                                   :pretty-print true}}]}
  :dependencies [[org.clojure/clojure "1.4.0"]])
</pre>
1. `cd src/cljs_helloworld`
1. `mv core.clj core.cljs`
1. Edit core.cljs and replace content with:
<pre class="brush:clojure">
(ns cljs-helloworld.core)

    (defn -main [& args]
      (println (apply str (map [\space "world" "hello"] [2 0 1]))))

    (set! *main-cli-fn* -main)
</pre>
1. `cd ../..`
1. lein cljsbuild once
1. node main.js

That's it! You should see "hello world" printed out.

  [1]: http://blip.tv/clojure/rich-hickey-unveils-clojurescript-5399498
  [2]: https://github.com/technomancy/leiningen
  [3]: /2013/04/14/cljs-restify-node/
