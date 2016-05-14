---
date: 2016-05-12 21:21:50 PDT
title: Human Thinking vs Parser Thinking
tags: Parser, Compiler, ECMAScript, JavaScript
description: How Humans Think About Code vs How Parsers Think About Code
---
Here's an interesting discussion that came up during a code review at my
company [Kash][1].

One reviewer said the following JavaScript code looks weird and was surprised
that it even works:
```javascript
   new ClassName().something()
```
and that the following coding convention is more preferable and more readable:
```javascript
   (new ClassName()).something()
```

The author of the change disagrees and wonders why there is confusion about
that. The code says to instantiate a class and then call a function of the
instance. Seems pretty self-explanatory.

I was then asked for my opinion.

At first my human brain agrees that `(new ClassName()).something()` reads more
intuitively. However, `new ClassName().something()` is obviously valid in
JavaScript grammar, so I wondered how it worked exactly. I pulled out the
[ECMAScript 1 parser][2] I wrote and figured it out. Below I'll outline the
difference between how we humans think and how parsers think about this code.

## How Humans Think

```javascript
   new ClassName().something()
```
When us humans look at the code above, we probably think in terms of two parts
and from left to right. This is because there is a space after the keyword
`new`, so we automatically want to separate into two parts.

We either group the instantiation together as part 1, and the invocation as part 2:
```javascript
//  _____________  ___________
// |   part 1    ||   part2   |
//  ‾‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾
   new ClassName().something()
```
or we see that maybe it could be interpreted as follows:
```javascript
//  ____________  _____________________
// |   part 1   ||   part2             |
//  ‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
   new           ClassName().something()
```

Since there are two ways to look at the code, our brain all of a sudden sees
some ambiguity and the brackets help avoid dealing with it.

With the brackets added, nobody is confused and seems more natural in helping
us decipher code.
```javascript
//  _______________  ___________
// |   part 1      ||   part2   |
//  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾
   (new ClassName()).something()
```

The brackets have a similar effect as when we use them in math expressions.
Kind of like `1 + (2 * 3)` might be easier to read, than `1 + 2 * 3`. There are
lots of spaces and we're trying to group things so we can comprehend it.
Although we might be tempted to read it as `(1 + 2) * 3`, instead we have to
look at the rest of the expression and understand some rules.

Looking at rest of the expression is exactly how a parser would look at things.

## How Parsers Think

Using my [parser][2] I figured out the resulting Abstract Syntax Tree (AST),
which is the result after a parser decided what the source code means.

A JavaScript parser would implement the [ECMAScript grammar][4] specification
in order to determine if a group of text is infact valid JavaScript source
code.

The process of determining what the source code means is similar to humans
reading code. i.e. one keyword or a group of text at a time

The difference is that a parser might need to finish looking at a complete
expression before knowing what the interpretation should be, but a human would
be quick to judge. When we see `new ClassName().something()` there is an urge
to immediately start separating it into logical parts with the help of the
space character.

You'll see how our human way of looking at things is different than how a
parser ultimately sees things. The parser's resulting AST is shown below:
```javascript
//  ______________________________________________________________________________________________
// |   ExpressionStatement                                                                        |
//  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
//     ↓
//  ______________________________________________________________________________________________
// |   CallExpression                                                                             |
//  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
//     ↓                                                                              ↓
//  _____________________________________________________________________________  _______________
// |   MemberExpression                                                          ||   Arguments   |
//  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
//     ↓                                                   ↓        ↓                 ↓
//  __________________________________________________  _______  ________________  _______________
// |   MemberExpression                               ||   .   ||   Identifier   ||   Arguments   |
//  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
//     ↓                                                   ↓        ↓                 ↓
//  __________________________________________________  _______  ________________  _______________
// |   NewExpression                                  ||   .   ||   Identifier   ||   Arguments   |
//  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
//     ↓          ↓                       ↓                ↓        ↓                 ↓
//  _________  ______________________  _______________  _______  ________________  _______________
// |   new   ||   MemberExpression   ||   Arguments   ||   .   ||   Identifier   ||   Arguments   |
//  ‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
//     ↓          ↓                       ↓                ↓        ↓                 ↓
//  _________  ______________________  _______________  _______  ________________  _______________
// |   new   ||   Identifier         ||   Arguments   ||   .   ||   Identifier   ||   Arguments   |
//  ‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
//     ↓          ↓                       ↓                ↓        ↓                 ↓
       new        ClassName               ()               .        something         ()
```

That was probably a lot of mumbo jumbo if you're not familiar with programming
language grammar. Fear not, what you really need to know is that the parser in
the end see the code more like this:
```javascript
//  _________________________
// |   part 2                | <- part 2 = CallExpression
// |_____________            |
// |   part 1    |           | <- part 1 = NewExpression
//  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
   new ClassName().something()
```

It sees that whole thing (represented with part 2) as what's called a
`CallExpression`, which means an invocation of something. Within that
`CallExpression`, there is a `NewExpression` (part 1) that needs to be
evaluated in order to know what we're doing an invocation on.

So how the parser ultimately represents the code is different than how humans
think. We think we need to do part 1 and then do part 2. Parser thinks there's
a part 2 and in order to figure out part 2, it needs to figure out part 1.

The difference is subtle eh?

## Is There Ambiguity?

Going back to what's ambiguous for us humans. One might ask, isn't it ambiguous
to the parser too? Could it read the same source code as follows?
```javascript
//  ____________  _____________________
// |   part 1   ||   part2             |
//  ‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
   new           ClassName().something()
```
Is there ambiguity in the language? How does that work?

Well, let's check with the [grammar][4] and see. What we want to see is if we could have `NewExpression` as the overall expression instead of
`CallExpression`. We want to answer whether the following is possible according
to the grammar:
```javascript
//  __________________________  
// |   part 2                 | <- part 2 = NewExpression
// |    ______________________|
// |   |   part 1             | <- part 1 = CallExpression
//  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  
   new  ClassName().something()
```

To understand rest of the exploration, you need to know that a programming
language grammar specifies what different things could be expanded into. I will
be exploring all possible expansions based on the grammar to see if our desired
outcome is possible.


We'll start the expansion with what we want the top level to be, a `NewExpression`:
```javascript
//  _________________________
// |   ExpressionStatement   |
//  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
//     ↓
//  _________________________
// |   NewExpression         |
//  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
```

Now the grammar tells us that a `NewExpression` could either be expanded into
`new NewExpression` or `MemberExpression`. Note that the `new NewExpression`
expansion is recursive. That's not a mistake.

Let's first see if expanding to `new NewExpression` would work.
```javascript
//  ____________________________________________________________
// |   NewExpression                                            |
//  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
//     ↓          ↓
//  _________  _________________________________________________
// |   new   ||   NewExpression                                 |
//  ‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
//     ↓          ↓
//  _________  _________________________________________________
// |   new   ||   MemberExpression                              |
//  ‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
//     ↓          ↓                       ↓        ↓
//  _________  ______________________  _______  ________________
// |   new   ||   MemberExpression   ||   .   ||   Identifier   |
//  ‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
//     ↓          ↓                       ↓        ↓
       new        ?                       .        something
```
We first did the expansion from `NewExpresion` to `new NewExpression`. Then
taking the `NewExpression` after `new` and expanding that to
`MemberExpression`.

From here, the language grammar says `MemberExpression` could be expanded to
`MemberExpression . Identifier`. Trying that we see it's not possible for this
to have `. Identifier Arguments` at the end, which is necessary to match
`.something()`.

Going back one step, the grammar also says `MemberExpression` can also become
`PrimaryExpression`. If you go through that path, you'll also see that doesn't
work in the end.

Now let's see if `NewExpression` expanding to `MemberExpression` immediately
would work.
```javascript
//  _____________________________________________________________________________
// |   NewExpression                                                             |
//  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
//     ↓
//  _____________________________________________________________________________
// |   MemberExpression                                                          |
//  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
//     ↓          ↓                                                  ↓
//  _________  _________________________________________________  _______________
// |   new   ||   MemberExpression                              ||   Arguments   |
//  ‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
//     ↓          ↓                       ↓        ↓                 ↓
//  _________  ______________________  _______  ________________  _______________
// |   new   ||   MemberExpression   ||   .   ||   Identifier   ||   Arguments   |
//  ‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
//     ↓          ↓                       ↓        ↓                 ↓
       new        ?                       .        something         ()
```
We follow the possible expansions as far as we can go and gets closer to what
we need. However, we're left with one final `MemberExpression` and it's not
possible to expand that to `Identifier Arguments`, which we need to represent
`ClassName()`.

Therefore, when encountering `new ClassName().something()`, the only way the
parser is going to interpret the code is that it is an overall
`CallExpression`. There is no confusion here.

### But What About new (ClassName().something())?

One small question remains and that's how would `new (ClassName().something())`
work? Is that allowed?

We can see how the parsing will play out below:
```javascript
//  ________________________________________________________________________________
// | NewExpression                                                                  |
//  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
//   ↓      ↓
//  _____  _________________________________________________________________________
// | new || NewExpression                                                           |
//  ‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
//   ↓      ↓
//  _____  _________________________________________________________________________
// | new || MemberExpression                                                        |
//  ‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
//   ↓      ↓
//  _____  _________________________________________________________________________
// | new || PrimaryExpression                                                       |
//  ‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
//   ↓      ↓        ↓                                                            ↓
//  _____  ___  _______________________________________________________________  ___
// | new || ( || Expression                                                    || ) |
//  ‾‾‾‾‾  ‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾
//   ↓      ↓        ↓                                                            ↓
//  _____  ___  _______________________________________________________________  ___
// | new || ( || CallExpression                                                || ) |
//  ‾‾‾‾‾  ‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾
//   ↓      ↓    ↓                                                   ↓            ↓
//  _____  ___  __________________________________________________  ___________  ___
// | new || ( || CallExpression                                   || Arguments || ) |
//  ‾‾‾‾‾  ‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾
//   ↓      ↓    ↓                                ↓    ↓             ↓            ↓
//  _____  ___  _______________________________  ___  ____________  ___________  ___
// | new || ( || CallExpression                || . || Identifier || Arguments || ) |
//  ‾‾‾‾‾  ‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾
//   ↓      ↓    ↓                   ↓            ↓    ↓             ↓            ↓
//  _____  ___  __________________  ___________  ___  ____________  ___________  ___
// | new || ( || MemberExpression || Arguments || . || Identifier || Arguments || ) |
//  ‾‾‾‾‾  ‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾
//   ↓      ↓    ↓                   ↓            ↓    ↓             ↓            ↓
//  _____  ___  __________________  ___________  ___  ____________  ___________  ___
// | new || ( || Identifier       || Arguments || . || Identifier || Arguments || ) |
//  ‾‾‾‾‾  ‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾‾‾‾‾‾‾‾‾  ‾‾‾
//   ↓      ↓    ↓                   ↓            ↓    ↓             ↓            ↓
     new    (    ClassName           ()           .    something     ()           )
```

Great! So the grammar says that's valid JavaScript.

In this case, the parent AST node is a `NewExpression` and has a
`CallExpression` as a child of it. This is reversed from previously where
`CallExpression` is the parent and `NewExpression` is the child.

## Conclusion

So in the end there is no ambiguity of how JavaScript parser see `new
ClassName().something()`. Our brain will probably still be a bit hesitant from
time to time, but just remember to see it as an overall `CallExpression` like
the parser does, or just put brackets around to help yourself.

How you should deal with the difference between human thinking and parser
thinking depends on your team's coding convention and decision.

But either way, hopefully once you understood how the parser see things in the
end, you'll never forget that. I sure won't.

  [1]: http://withkash.com
  [2]: https://github.com/dannysu/ecmascript1
  [4]: https://dannysu.com/es1-left-recursive-grammar/
