---
date: 2012-10-04 11:38:26
title: Clojure's Software Transaction Memory (STM)
tags: Mad Coding, Clojure
---
I went to a MeetUp yesterday where people tried to learn Clojure's STM
implementation and tackle the [dining philosopher problem][1]. That was a ton of
fun!

Instead of being more hands on using locks to protect resource accesses, Clojure
gives you tools to do things a different way. Clojure's STM takes the optimistic
approach to let all threads using the shared resources to proceed. When a thread
tries to commit a transaction that's when a transaction might fail and the logic
is retried. I really like how [Clojure in Action][4] explains the difference
between Clojure and other languages. In other languages that use mutable data,
you're directly modifying a memory location. Whereas in Clojure the data is
immutable and you're instead changing what version of the data you're pointing
to.

The best way to learn is to be hands on and actually practice what you preach.
The remaining sections jot down what I learned yesterday.
<br>
<br>

## **Exceptions not for Triggering STM Retries**

The solution that we ended up with at the end of the night is on Tom
Alexandrowicz's [github][3]. After I got home I stared at the grab-forks
function and thought to myself that it shouldn't be working. The solution for
grab-forks at the end of the night is as follows:

<pre><code class="clojure">
(defn grab-forks [philo-id table]
  (let [cur-forks (get-forks philo-id table)]
    (if (= [nil nil] (map deref cur-forks))
      (doseq [fork cur-forks]
        (ref-set fork philo-id))
      (throw (Exception. (str "Couldn't get forks"))))
    table))

</code></pre>

One thing we weren't sure during the MeetUp is whether throwing the Exception is
actually working in terms of triggering the retry logic in Clojure's STM. We
weren't sure whether philosophers were properly waiting for each other. As I
digged into Clojure source code some more, I determined that throwing the
Exception is wrong.

<pre><code class="c">
public static final int RETRY_LIMIT = 10000;

for(int i = 0; !done && i < RETRY_LIMIT; i++)
    {
    try
        {
        //calls your dosync logic here
        }
    catch(RetryEx retry)
        {
        //eat this so we retry rather than fall out
        }
    }

</code></pre>

As shown in the Clojure source above, you don't throw Exceptions to trigger the
retry logic because Clojure is looking for RetryEx and not the generic Exception
class. Also, as I discovered later, this is a totally wrong way to think about
Clojure's STM. So how does one make sure grab-forks don't go ahead and grab some
other philosopher's fork while it's in use?
<br>
<br>

## **STM Versioning**

Tom later posted this image of times when philosophers are eating and when
they're thinking. Orange is for eating and blue is for thinking.  
![](http://photos2.meetupstatic.com/photos/event/d/c/7/0/highres_165656432.jpeg)

I was really surprised that the solution worked because I had already determined
throwing that Exception is not the way to go. This is my favourite part of
learning when my current understanding gets challenged and then I learn
something new. The magic goes away. The answer to why this grab-forks solution
worked depends on how it's used in conjunction with other things. The
philosopher's thinking and eating cycle is implemented as follows:

<pre><code class="clojure">
(defn philosopher [philo-id numtimes log table]
	(dotimes [_  numtimes]
		(think philo-id log (rand-int 200))
		(dosync 
			(grab-forks philo-id table)
			(eat philo-id log (rand-int 200))
			(drop-forks philo-id table))))

</code></pre>

The key part is the dosync portion of the code. This solution lumped grab-forks,
eat and drop-forks all into the same transaction. If you were to separate
grab-forks and drop-forks into their own dosync, then the solution breaks.
Realizing the all consuming dosync is what make it work and combined with this
picture on STM versioning on [Neale Swinnerton's blog][2], I now have new found
understanding of STM.

Having dosync contain all the parts of philosopher eating something works
because of STM and immutable data. If between the time grab-forks and drop-forks
finishes, some other philosopher comes in and also try to eat using the same
forks, then whoever finishes eating first will be the one that'll ultimately
commit their changes. The other philosopher will simply retry the whole grab,
eat and drop logic.

What this means is that there is no need to even check whether the forks are
free to be used assuming they all start out being free to use. The reason is
because of versioning. All forks will start out at version 0. Each philosopher
will try to change the version the ref is pointing to from version 0 to version
1 (grab forks) to version 2 (drop forks). Whichever philosopher finishes first
gets to commit their version 2. All other philosophers will need to restart
because what they started out with (version 0) is no longer the latest version.
Someone else modified things during their transactions.

<pre><code class="clojure">
(defn grab-forks [philo-id table]
  (let [cur-forks (get-forks philo-id table)]
    ;;
    ;; This check for nil (represent free fork) is totally unnecessary
    ;;
    (when (= [nil nil] (map deref cur-forks))
      (doseq [fork cur-forks]
        (ref-set fork philo-id)))
    table))

</code></pre>

This is some awesome learning right here. When using ref and dosync this way,
you don't even have to think about locking or anything. You don't even have to
check whether you can use things or not. All that's required is to mark the
resources using ref and do things inside a dosync. However, there is some
problems with this approach. The transaction could potentially be fairly long
running since it encompasses the actual eating part.
<br>
<br>

## **Finer Grain Control**

Ideally each transaction should be as short as possible to avoid more chances
for retries and taking up more CPU time for the retries because STM needs to
re-execute a bunch more code. Good thing is in this case the `eat` function
simply sleeps for certain amount of time and isn't actually doing any number
crunching.

What we want is to be able to dosync just on grab-forks and drop-forks:

<pre><code class="clojure">
(defn philosopher [philo-id numtimes log table]
  (dotimes [_ numtimes]
    (think philo-id log (rand-int 200))
      (dosync 
        (grab-forks philo-id table))
      (eat philo-id log (rand-int 200))
      (dosync 
        (drop-forks philo-id table))))

</code></pre>

I attempted at a solution by modeling forks as promises instead. Grabbing a fork
is equivalent to placing a promise to deliver it back. If a promise hasn't been
delivered yet, then whoever tries to deref it will block to wait for it to be
delivered. So in this case, a philosopher will simply block waiting for a fork
to be put down before doing any more transaction retrying to obtain the fork. My
solution is on my [github][5].

The one thing I don't quite like about this is that it feels like locking again.
Perhaps there are better ways and I'll update this once I learn them. There are
other problems with the current solution as well. STM retries up to 10000 times,
so if there is really a severe starvation issue STM won't save you. This
solution also makes sure to grab 2 forks at once or fail all together. You
could still implement things in Clojure that deadlocks if philosopher grabs 1
fork at a time.

  [1]: http://en.wikipedia.org/wiki/Dining_philosophers_problem
  [2]: http://sw1nn.com/sw1nn.com/blog/2012/04/11/clojure-stm-what-why-how/
  [3]: https://github.com/tlalexan/TorontoCodingDojo-WeekEight-DiningPhilosophers
  [4]: http://www.manning.com/rathore/
  [5]: https://github.com/dannysu/clj-philosopher
