---
date: 2012-07-31 00:25:32
title: Automatic Reference Counting (ARC) on iOS
tags: Apple, Mad Coding, iOS, iPhone, ObjC, ARC, automatic reference counting, memory leak, Well.ca
---
<script type="text/javascript" src="/files/syntaxhighlighter_3.0.83/scripts/shCore.js"></script>
<script type="text/javascript" src="/files/syntaxhighlighter_3.0.83/scripts/shBrushJScript.js"></script>
<link type="text/css" rel="stylesheet" href="/files/syntaxhighlighter_3.0.83/styles/shCoreDefault.css"/>
<script type="text/javascript">SyntaxHighlighter.all();</script>
The Well.ca iPad app is now available in [Apple AppStore][4]. This is an app
that I've been working on at work with two other developers. Prior to submission
I had to fix memory leaks in the app to stop it from crashing during infinite
scroll. This post talks about my experience dealing with retain cycles and
fixing memory leaks.

&nbsp;  

## **Automatic Reference Counting (ARC)**

At the start of the project I championed the use of ARC instead of settling
with what other developers already know of retain/release in ObjC. Sure, it
means learning something new but I didn't want an environment of not learning
new things or not making use of latest best practices. There are a wealth of
resources online to learn about how to use ARC like [Mike Ash's blog][1],
[StackOverflow][2], and [the LLVM site][3].

With ARC in place, developers don't need to manually do retain and release. The
major thing to watch out for is in dealing with retain cycles and places where
weak references are applicable. Back when I was working on Windows Phone I was
introduced to the concept of strong vs weak references. In particular I had to
implement code that need to deal with weak references across multiple threads
(UI thread vs background threads such as WinINet callback). The knowledge
transferred over while developing Well.ca's iPad app.

&nbsp;  

## **Retain Cycles**

Early on during development we simply used `__strong` qualifier everywhere due
to the need to iterate faster. We focused on getting "flick to add to
cart" and other cupboard organization gestures working first. However, using
`__strong` qualifier everywhere meant that in some places we had retain cycles
and led to memory leaks.

The memory usage graph seen by Product-&gt;Profile-&gt;Alloctions in our
application showed increasing usage just by clicking around, which is a sign of
memory leak even though I expected certain things to be freed when switching
view controllers:
![](http://imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3JlcgwLEgVpbWFnZRi5Fww)

Retain cycle, or circular reference, is when two objects each maintains a strong
reference to the other:

![](http://imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3JlcgwLEgVpbWFnZRjRDww)

In code it might look like this:

<pre class="brush:objc">
@interface Object1 : NSObject
@property (strong) Object2 *obj2;
@end

@interface Object2 : NSObject
@property (strong) Object1 *obj1;
@end
</pre>

Since two objects both maintain strong reference to the other, their retainCount
will always be greater than 1 and won't be freed. The actual cycle might look a
bit more complicated than the code presented above so one needs to careful. This
is what leads to memory leak in ARC projects.

&nbsp;  

## **Weak References in iOS 5**

One way to break the retain cycle is to make use of weak references. In iOS 5,
developers can use the `__weak` qualifier. The compiler will automatically have
code to take care of zeroing out references to it when an object goes away. When
weak reference is used, the retainCount of the referenced object isn't
increased. Therefore, when the referenced object needs to go away it is free to
do so.

Take the following code for example:

<pre class="brush:objc">
@interface Object1 : NSObject
@property (strong) Object2 *obj2;
@end

@interface Object2 : NSObject
@property (weak) Object1 *obj1;
@end
</pre>

Object1 has **strong** reference to Object2, but Object2 has a **weak**
reference back to Object1. When Object1's retainCount reaches 0, Object1 memory
is freed and Object2's obj1 reference becomes nil.

This should be used when there is a parent-child relationship between objects.
Also, when there's a callback relationship such as if an object is a delegate,
then weak reference should also be used to avoid retain cycle.

&nbsp;  

## Weak References in iOS 4.3

For applications that need to support iOS 4.3, they can still use ARC but
`__weak` qualifier is not available. In that situation you have to use
`__unsafe_unretained` to avoid retain cycle if your threading requirement allows
it, or you need to implement your own weak reference functionality. One thing to
note about `__unsafe_unretained` is that it's basically a dangling pointer, so
use it only if everything can come back to main thread.

When `__unsafe_unretained` is used, the retainCount of the referenced object is
also not increased like the `__weak` case. However, there's nothing that will
automatically nil other references to it. The right way to deal with it is to
make sure when the referenced object is freed that it also zero out reference to
itself.

Take the following code for example:

<pre class="brush:objc">
@protocol Object1Delegate <NSObject>
@end

@interface Object1 : NSObject
@property (assign) id <Object1Delegate> delegate;
@end

@interface Object2 : NSObject
@property (strong) Object1 *obj1;
@end

@implementation Object2
@synthesize obj1 = _obj1;
- (void)doSomething {
    obj1.delegate = self;
}
@end
</pre>

Object2 code passing itself to Object1 as the delegate. Object1 uses `(assign)`
which has the `__unsafe_unretained` semantic to avoid retain cycle. The code as
is might cause crashes if Object1 tries to access delegate after Object2 is
freed. The correct way for Object2 to deal with this is to make sure to zero out
references to it.

<pre class="brush:objc">
@implementation Object2
@synthesize obj1 = _obj1;
- (void)doSomething {
    obj1.delegate = self;
}
- (void)dealloc {
    // Zero out references to Object2 when Object2 goes away
    obj1.delegate = nil;
}
@end
</pre>

&nbsp;  

## **Explicitly Break Retain Cycle**

Another method that I used to fix the memory leak caused by retain cycle is by
explicitly breaking the cycle from a 3rd party. Let's say we do have the
situation of retain cycle like that in the strong reference diagram above. If
there is another object whose lifetime should contain both Object1 and Object2,
then it could be used to break the cycle.

For example, in the code below Object3 maintains references to both Object1 and
Object2. When Object3 is being freed, it can break up the cycle between Object1
and Object2 thus allowing them to be freed as well.

<pre class="brush:objc">
@interface Object3 : NSObject
@property (strong) Object1 *obj1;
@property (strong) Object2 *obj2;
@end

@implementation Object3
- (void)dealloc {
    // When Object3 goes away, explicitly break up the cycle of obj1 and obj2
    obj2.obj1 = nil;
}
@end
</pre>

&nbsp;  

## **Conclusion**

ARC works great for iOS and gets rid of the need to manually dealing with
references. I remember programming for Windows Phone and making sure I have all
the AddRef/Release right. Similar to what an iOS dev needs to do with
retain/release.

The only thing then to watch out for is cycles and I've outlined the techniques
to deal with that. Well.ca iPad app's memory usage pattern after fixing retain
cycle is much better and stopped showing the increasing staircase pattern.

One other thing that I haven't talked about is what happens when objects aren't
freed or when callbacks aren't happening on main thread. That'll be a post for
another day when I do run into that issue on iOS. It was certainly something I
had to deal with on Windows Phone before.

  [1]: http://www.mikeash.com/pyblog/friday-qa-2011-09-30-automatic-reference-counting.html
  [2]: http://stackoverflow.com/questions/6260256/what-kind-of-leaks-does-automatic-reference-counting-in-objective-c-not-prevent/6388601#6388601
  [3]: http://clang.llvm.org/docs/AutomaticReferenceCounting.html
  [4]: http://itunes.apple.com/ca/app/well.ca/id516359938?mt=8
