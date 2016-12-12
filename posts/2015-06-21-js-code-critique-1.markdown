---
date: 2015-06-21 22:44:52 PDT
title: JavaScript Code Critique #1
tags: JavaScript, programming, Mad Coding, Code Critique
---
This will be the first of many code critiques I give to myself.

Learn by doing is an excellent way to gain knowledge. In fact some of the
things you can't just settle with book knowledge like running a startup,
because you'd still have zero clue how to actually do it.

However, it's also important to know that sometimes you really do need to dig
in, read the literatures or learn something at a deeper level. Just as you can
copy & paste something from StackOverflow, but you better also understand the
why and be able to explain the solution to another person. Striving for a
balance between get stuff done now (if requirement necessitates it), and learn
something deeper so you can get stuff done faster the next time.

I'm a big believer of code reviews because my peers can help teach me stuff. As
I try to patch up some of the holes in my understanding of JavaScript, I thought
it's a great time to give myself some code critiques. I'm going to use my
[hash0][1] project for this because it was first written a while ago.


# new Array() vs \[\]

For some reason I coded this constructor function with `new Array()`.

```javascript
function Metadata() {
    this.configs = new Array();
    this.mappings = new Array();
    this.dirty = false;
}
```

This can be written using array literal and be more compact:

```javascript
function Metadata() {
    this.configs = [];
    this.mappings = [];
    this.dirty = false;
}
```

Just a style thing, so moving on.


# truthy & falsy values #1

For some reason I wrote a whole bunch of code just to return whether the
localStorage has 'storageUrl' property:

```javascript
Metadata.prototype.hasStorageUrl = function() {
    if (!(storage['storageUrl']) ||
        storage['storageUrl'] == '') {
        return false;
    }
    return true;
};
```

[Storage.getItem][2] will return null if it doesn't have value for the provided
key. In this case, the key is 'storageUrl'. I guess when I originally wrote this
I didn't have a great grasp of the falsy values in JavaScript. The if-statement
checks for truthy value of `!(storage['storageUrl'])` while also checking
whether the value is an empty string. However, since `false`, `0`, empty string,
NaN, null, and undefined are all falsy values, the empty string check is
redundant. Coupled with the || operator, it's not just redundant, it'll never
run.

Since I mainly want the function to tell me if localStorage has value for
'storageUrl' or not and make sure it's not an empty string, I really just need
to return the truthy value of whatever Storage.getItem gives me.

Here's an updated version:

```javascript
Metadata.prototype.hasStorageUrl = function() {
    return Boolean(storage.storageUrl);
};
```

There. Nice and simple. Empty string returns false. null also returns false.
Other strings return true. This is exactly what I want.


# truthy & falsy values #2

I also wrote code like this:

```javascript
Metadata.prototype.findConfig = function(param, partial_match) {
    partial_match = partial_match || false;

    // ...
};
```

What was I thinking? Yeah, probably didn't quite grasp truthy value at the time.

As is the code converts partial_match to a boolean false, if it's falsy. But the
code leaves the value alone if it's truthy. So you end up with sometimes boolean
and sometimes whatever the incoming type is.

If I really wanted to ensure it's a typeof boolean, then I could either
!!partial_match or Boolean(partial_match) here.


# Array.prototype.map()

I have a function to loop over all configs and return an array of their param
property. There's another way to write the same thing with the map() function
though.

Before:

```javascript
Metadata.prototype.getAllParams = function() {
    var params = [];
    for (var i = 0; i < this.configs.length; i++) {
        params.push(this.configs[i].param);
    }
    return params;
};
```

After:

```javascript
Metadata.prototype.getAllParams = function() {
    return this.configs.map(function(config) {
        return config.param;
    });
};
```

I think the higher-order function map() version looks more concise and clear.
The for-loop version would tend to be faster though, because it isn't invoking a
function and setting up and tearing down function scope repeatedly. This
function doesn't run many times and always at the initialization of an AngularJS
controller, so it doesn't matter at all.


# Array.prototype.filter()

Similarly I have a function that uses a for-loop, which acts like a search and
returns only matching config:

```javascript
Metadata.prototype.findConfigs = function(param) {
    var matches = [];
    for (var i = 0; i < this.configs.length; i++) {
        if (this.configs[i].param.indexOf(param) >= 0) {
            matches.push(this.configs[i]);
        }
    }
    return matches;
};
```

Well, that sounds like a job for filter():

```javascript
Metadata.prototype.findConfigs = function(param) {
    return this.configs.filter(function(config) {
        return (config.param.indexOf(param) >= 0);
    });
};
```

Cool, that's more compact and perhaps more clear at a glance what the function
is doing. I'm filtering and grabbing only the ones that meet the condition
inside.

---
## 

Well, that's it for now. I have more things to add but no time to write it down
right now.

  [1]: https://github.com/dannysu/hash0
  [2]: https://developer.mozilla.org/en-US/docs/Web/API/Storage/getItem
