---
date: 2013-04-14 22:52:18
title: Restify With ClojureScript
tags: Mad Coding, Node.js, Clojure, ClojureScript, Restify
---
This is a follow-up post from previous [Getting Started With ClojureScript For
Node.js][1] tutorial. This time showing how to get a [restify][3] service up &
running. Example source code on [github][2].

Setting up ClojureScript for running Node.js is the same as before. You can also
see the [README][4] for details. Below is a demonstration of restify's example
echo server written in ClojureScript.

## **javascript**
<pre class="brush:c">
var restify = require('restify');

function respond(req, res, next) {
  res.send('hello ' + req.params.name);
}

var server = restify.createServer();
server.get('/hello/:name', respond);
server.head('/hello/:name', respond);

server.listen(8080, function() {
  console.log('%s listening at %s', server.name, server.url);
});
</pre>

## **ClojureScript: src/cljs_node/core.cljs**
<pre class="brush:clojure">
(ns cljs-node.core
  (:require [cljs.nodejs :as node]))

(def restify (node/require "restify"))

(defn log [& args]
  (.log js/console (apply format args)))

(defn respond [req res next]
  (.send res (str "hello " req/params/name)))

(def server (.createServer restify))

(do
  (.get server "/hello/:name" respond)
  (.head server "/hello/:name" respond))

(defn -main [& args]
  (.listen server 8080 #(log "%s listening at %s" (.-name server) (.-url server))))

(set! *main-cli-fn* -main)
</pre>

  [1]: /2013/01/14/clojurescript-for-nodejs/
  [2]: https://github.com/dannysu/cljs-node
  [3]: http://mcavage.github.io/node-restify/
  [4]: https://github.com/dannysu/cljs-node/blob/master/README.md
