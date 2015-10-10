---
date: 2012-09-21 00:00:00
title: Experience so far in Clojure
tags: Mad Coding, Clojure, Node.js
---
**Update:** I've read up to chapter 10 of Clojure in Action now and Clojure's
STM is really cool! However, reading can only get you so far in learning. At
the end of the day actual hands on experience is needed. I found that the
book's coverage on Compojure is way too little for somebody to get into it. My
actual hands-on experience with Compojure taught me more than what the book
covers. Also, I went to a MeetUp and got some experience with STM that's really
enlightening. I blogged about it [here][11].
<br>
<br>
I started reading the book [Clojure in Action][1] that I got from a local
library. The beauty of Clojure's Lisp syntax is making me wish that I had paid
more attention to those Scheme exercises in school. I vaguely remember all the
brackets and trying to use only recursion instead of for-loops, but I didn't
fully understand why the professor was teaching us some Scheme. He probably did
say why but it was lost on me. In any case, it's great to rediscover Lisp.
<br>
<br>

## **The Beauty of Clojure and of Lisp**

In Clojure there isn't many special operators that dictate the structure of your
program. Everything is a list denoted with ()s with the first thing in the list
being the function to execute. For example, comparing a function call with an
"if-statement":

<pre><code class="clojure">
; A list with "add" being the function and two numbers in the list as arguments
(+ 1 1)

</code></pre>

vs

<pre><code class="clojure">
; A list with "if" being the function that takes 3 arguments.
; First argument is the expression to evaluate to determine true or false.
; Subsequent arguments specify what to use in case of true or false.
(if (= 1 1)
  (println "It's true!")
  (println "It's false!"))

</code></pre>

With the lack of much of a syntax and special operators, you are free to use
whatever characters you want as identifiers. E.g. you can have functions with
dashes (-), exclamation marks (!), question marks (?), etc. That's beautiful!

<pre><code class="clojure">
(if-not test then)
(zero? 0)
(awesome!)

</code></pre>

I love the minimalistic Lisp syntax. After playing with Clojure for a little bit
and with the help of VimClojure, it's no longer just a sea of brackets. That's
not to say a bit more syntax and special operator is always bad. Below are 2
examples in Haskell that are even more compact than Clojure.

#### Function Composition

<pre><code class="haskell">g . f</code></pre>

vs

<pre><code class="clojure">(comp g f)</code></pre>

#### Partial Application

<pre><code class="haskell">(1 +)</code></pre>

vs

<pre><code class="clojure">(partial + 1)</code></pre>

Despite possibly making things even more compact, Lisp opts to maintain its
code-as-data mentality. I think *Clojure in Action* explains it quite well by
using XML as example and relating to an Abstract Syntax Tree. The Clojure code
can be thought of as a better looking XML describing the program. It's written
in the same exact list representation as data thus is [homoiconic][2]. Slava
Akhmechet wrote [this piece][3] explaining Lisp with a similar approach.

The homoiconicity of Lisp allows it to provide macro feature that is consistent
with rest of the language. For example, here's the *unless* macro from the
*Clojure in Action* book:

<pre><code class="clojure">
; Gives you ability to have a function/macro that evaluates a test condition
; to determine subsequent action.
; If the test condition is false only then evaluate the given expression.
(defmacro unless [test then]
  (list 'if (list 'not test)
    then))

; Using it
(unless (even? x)
  (println "odd"))

</code></pre>

The macro is defined in the same way as anything else and feels very natural.
For the *test* condition or the *then* expression, you can give it any
expression just as you would anywhere else in Clojure. Comparing this to say C++
macros, it's a big difference. C++ macros are typically written in upper
case, and requires you to code in different way by having '\\'s everywhere to
have multiline. If you want to execute different set of statements you need to
put them into a function instead and use macro magic to call it.
<br>
<br>

## **Runs on JVM (or whatever it can compile to)**

Clojure runs on the JVM, which is both a blessing and a curse.

Being on the JVM means that you can use whatever Java libraries you have lying
around. This means that I can actually dust off [OCVolume][5] and directly use
it in a Clojure program. If you program for Android, it might also mean more
similarity between client and server much like how Node.js allows for javascript
on client and server. The same might be true for CLR languages too with
ClojureCLR. Furthermore, based on JVM means that you can interop with Java
libraries such as [clj-ml with Weka][4], [Mahout][6], or [Hadoop][7].

The downside of running on top of JVM is that your programs get hit by JVM
startup time. Also, calling Java methods look out of place:

<pre><code class="clojure">(.toUpper "lower")</code></pre>
<br>

## **Comparison to Node.js**

The only disappointment I have so far with Clojure is in its comparison of
certain tasks to Node.js. I wrote a REST service in both Node.js and Clojure to
compare them. Node.js service consistently performs faster and handles more
concurrent requests on my 1 CPU VM than Clojure + Compojure + Jetty. The
service simply takes a GET request, do some validation and then hits a
PostgreSQL DB couple times for data. The Clojure version could not handle 1000
concurrent requests on my VM while Node.js did and does it faster.

After watching this [Node.js intro video][8] featuring Ryan Dahl, I have a better
understanding why Node.js can be fast. There's very little javascript involved
in my service. Also mostly I'm measuring DB access speed and C++ vs Clojure on
JVM. For simple DB backed REST APIs, Node.js is a better choice for speed and
concurrent requests based on what I see on my VM.

Node.js isn't without its limitations though. To keep the test fair I made sure
the VM only has 1 CPU because Node.js can't use multiple cores without having
multiple independent processes. I also have tasks where it's more processing
intensive instead of IO intensive. E.g. Grabbing a bunch of data from the web,
and then spend rest of the time processing it. In this situation, you're at the
risk of blocking node's main thread.

For the issue of blocking the main thread, I think back to my experience with
mobile development and wondered why node doesn't just have a way to put some
things in its thread pool. Turns out that the limitation is because javascript
is single thread and V8 is single thread. You can't actually implement what I am
thinking in node without corrupting V8's internal state. There is the [Child
Process][9] way of spinning up a separate process to do the CPU intensive task.
That's a solution for sure, but not elegant and separates code to different
places.

Clojure on the other hand would be able to utilize multi-core much easier. Check
out this [awesome talk][10] by Tyler Jennings on how he used Clojure at Groupon.
Aside from the cool BK-Tree, Tyler made use of Clojure parallel processing by
switching from map to pmap. BAM!
<br>
<br>

## **Some Useful Functions**

While working on my dream project, I had to deal with list processing where I
need to correlate pairs of items in list. I might be able to change my algorithm
to use BK-Tree instead though, after seeing Tyler's talk. In any case, here are
some helper functions I wrote:

<pre><code class="clojure">
(defn take-n-while
  "returns a lazy sequence of successive items from coll while
  (pred (take cnt s)) returns true. pred must be free of side-effects."
  [pred cnt coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (when (apply pred (take cnt s))
        (cons (take cnt s) (take-n-while pred cnt (rest s)))
        )
      )
    ))

; Usage:
; user=> (take-n-while #(even? (+ %1 %2)) 2 [1 3 1 2])
; ((1 3) (3 1))

</code></pre>

This is like the `take-while` function except you can consume the list at any n
items at a time. My first solution involved using `partition` and
`partition-by`, but that didn't work as well as this.

  [1]: http://www.manning.com/rathore/
  [2]: http://en.wikipedia.org/wiki/Homoiconicity
  [3]: http://www.defmacro.org/ramblings/lisp.html
  [4]: http://antoniogarrote.github.com/clj-ml/index.html
  [5]: http://ocvolume.sf.net
  [6]: http://mahout.apache.org/
  [7]: http://hadoop.apache.org/
  [8]: http://www.youtube.com/watch?v=M-sc73Y-zQA
  [9]: http://nodejs.org/api/child_process.html
  [10]: http://www.infoq.com/presentations/Bootstrapping-Clojure
  [11]: /2012/10/04/clojure-stm/
