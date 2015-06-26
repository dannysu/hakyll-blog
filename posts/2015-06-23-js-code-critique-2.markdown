---
date: 
title: Javascript Code Critique #2
tags: javascript, programming, Mad Coding, Code Critique
---
Continuing the process of looking back at some old code that I wrote years ago.
This is my 2nd javascript code critique. The first one is [here][1].

My goal is to bring [hash0][3] code up to ES5 standards and stop coding like
it's 1990s. Once that's done, then go on to bring it up to ES6.

<br/>

## **Array.prototype.forEach()**

Looking through my old code, it looks like I wasn't taking advantage of the
forEach function. I have code that looks like this:
<pre class="brush:javascript">
for (var i = 0; i < vm.mappings.length; i++) {
    vm.mappings[i].label = vm.mappings[i].from + ' > ' + vm.mappings[i].to;
}
</pre>

This works fine, but can be improved with forEach(). For one, the var i is not
just in the for-loop's scope. It also looks messy with a bunch of array accesses
with index here and there.

With forEach():
<pre class="brush:javascript">
vm.mappings.forEach(function(mapping) {
    mapping.label = mapping.from + ' > ' + mapping.to;
});
</pre>

Nice.

<br/>

## **camelCase**

When I first started writing javascript, I had just came from the world of C/C++
and PHP, so I was kind of more used to variable_name than variableName. Camel
case is the norm so I'll do that instead from now on.

So from this:
<pre class="brush:javascript">
Metadata.prototype.findConfig = function(param, partial_match) {
</pre>

To this:
<pre class="brush:javascript">
Metadata.prototype.findConfig = function(param, partialMatch) {
</pre>

<br/>

## **var hoisting**

It's [recommended][2] to declare variables at the top of their scope to make
things clear.

I had code where variable declaration is done inside an if-statement.
<pre class="brush:javascript">
Metadata.prototype.addMapping = function(from, to) {
    var mapping = this.findMapping(from);
    if (mapping === null) {
        var newMapping = {
            'from': from,
            'to': to
        };
        this.mappings.push(newMapping);
    }
    ...
};
</pre>

However, the semantic is actually different, so might as well change it to what
it is:
<pre class="brush:javascript">
Metadata.prototype.addMapping = function(from, to) {
    var mapping = this.findMapping(from);
    var newMapping;
    if (mapping === null) {
        newMapping = {
            'from': from,
            'to': to
        };
        this.mappings.push(newMapping);
    }
    ...
};
</pre>

Although in this case, I could do without the extra variable too.

<br/>

## **Closure**

I'll deviate from hash0 for a bit, and critique code I wrote 3 years ago for
AvidTap. I had function declaration within a for-loop, which caused problems.
Thankfully nowadays with linters and fully understanding, it's not an issue
anymore.

Imagine something like this:
<pre class="brush:javascript">
for (var i = 0; i < notifications.length; i++) {
    var notification = notifications[i];

    var handleTap = function() {
        app.showStore(notification.business_id);
    };

    app.onTap(
        '#notification_' + notification.business_id + '_' + i,
        handleTap;
    );
}
</pre>

What is clear now wasn't so clear when I first started. Here the for-loop and
the `notification` variable are in the same exact scope. Each handleTap()
function given to app.onTap() will keep a reference to this scope. However, as
the for-loop keeps on going, it will keep modifying what notification points to.
Remember var hoisting. The result is unexpected behaviour when you actually go
to trigger the tap event, because at the time of the tap event the notification
variable in the scope would have been changed to reference the last one in
notifications array.

<br/>

That's all I have time for code critique #2 for now, but it's great to get it
out of my system.

  [1]: /2015/06/21/js-code-critique-1
  [2]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/var
  [3]: https://github.com/dannysu/hash0
