---
date: 2016-05-12 21:21:50 PDT
title: Human Thinking vs Parser Thinking
tags: Parser, Compiler, ECMAScript, JavaScript
description: How Humans Think About Code vs How Parsers Think About Code
---
Here's an interesting discussion that came up during a code review at my
company [Kash][1].

One reviewer said the following code looks weird and was surprised that it
even works:
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
JavaScript grammar, so I wondered how it worked exactly. I pulled up the
[ECMAScript 1 parser][2] I wrote and figured it out. Below I'll outline the
difference between how we humans think and how parsers think about this code.

## How Humans Think

```javascript
   new ClassName().something()
```
When us humans look at the code above, we think in terms of 2 parts and from
left to right.
```javascript
// [-- part 1    --][-- part2 --]
   (new ClassName()).something()
```

We think step-by-step, do one thing and then another thing. This is why if you
add in the brackets it seems more natural and closer to how humans decipher
code.

I guess in that sense it helps to have the brackets so you don't have to do
that mental exercise. Kind of like `1 + (2 * 3)` might be easier to read, than
`1 + 2 * 3`. We want to read it as `(1 + 2) * 3`, but instead we have to look
at the rest of the expression too. Looking at rest of the expression is exactly
how parser would look at things.

## How Parsers Think

Using my [parser][2] I figured out the resulting Abstract Syntax Tree (AST),
and that's how parsers view the code. The ASCII art of the parsing process is
shown below. I also double checked with [Acorn parser][3] just to be sure.

```javascript
// [-- ExpressionStatement                                                 --]
//     ↓
// [-- CallExpression                                                      --]
// (   This is the overall CallExpression node                               )
//     ↓                                                         ↓
// [-- MemberExpression                                  --] [-- Arguments --]
//     ↓                                  ↓   ↓                  ↓
// [-- MemberExpression                   .   Identifier --] [-- Arguments --]
//     ↓   ↓                ↓             ↓   ↓                  ↓
// [-- new MemberExpression Arguments --] .   Identifier --] [-- Arguments --]
// (  This is a NewExpression AST node  )
//     ↓   ↓                ↓             ↓   ↓                  ↓
// [-- new Identifier       Arguments --] .   Identifier --] [-- Arguments --]
//     ↓   ↓                ↓             ↓   ↓                  ↓
       new ClassName        ()            .   something          ()
```

In order for `new ClassName().something()` to be valid JavaScript, the parser
needs to see the whole expression as a `CallExpression`. Within the
`CallExpression` there is a `NewExpression`, which would need to be evaluated
in order to complete the evaluation of the `CallExpression`.

The difference compared to human reading code is quite subtle. Parser knows
what the whole expression must be to be valid JavaScript, and then within that
bigger expression it knows what the sub-expressions are.

## Is There Ambiguity?

But what about the case where you're just calling `new` on whatever returns on
the right? i.e. Can `new ClassName().something()` ever be mistaken to be
executing like `new (ClassName().something())`? Is there ambiguity in the
grammar? How does that work?

Well, let's check with the [grammar][4] and see. What we want to see is if we
could have `NewExpression` as the overall expression instead of
`CallExpression`.
```javascript
// We start out with Program and then ExpressionStatement:
// [-- ExpressionStatement --]
//     ↓
// [-- NewExpression       --]
```

`NewExpression` could either be `new NewExpression` or `MemberExpression`.

Let's first see if expanding to `new NewExpression` would work.
```
// [-- NewExpression               --]
//     ↓          ↓
// [-- new --][-- NewExpression    --]
//     ↓          ↓
// [-- new --][-- MemberExpression --]
```
From here the `MemberExpression` could be expanded to `MemberExpression .
Identifier`, but that doesn't work because we need `Identifier Arguments` at
the end.

The only other option is if `MemberExpression` becomes `PrimaryExpression`.
However, that also doesn't work in the end.

Now let's see if `NewExpression` expanding to `MemberExpression` immediately
would work.
```
// [-- NewExpression                                             --]
//     ↓
// [-- MemberExpression                                          --]
//     ↓          ↓                                    ↓
// [-- new --][-- MemberExpression              --][-- Arguments --]
//     ↓          ↓                ↓ ↓                 ↓
// [-- new --][-- MemberExpression . Identifier --][-- Arguments --]
```

That's closer. However the remaining `MemberExpression` can't handle
`ClassName()`. The grammar doesn't allow expansion to `Identifier Arguments`.

Therefore, when encountering `new ClassName().something()`, the only way the
parser is going to interpret the code is that it is an overall
`CallExpression`. There is no confusion here.

However, if you wrap `ClassName().something()` with brackets, then the grammar
allows `new (ClassName().something())`. We can see how parser will play out below:
```
// [-- NewExpression                                                                                --]
//     ↓          ↓
// [-- new --][-- NewExpression                                                                     --]
//     ↓          ↓
// [-- new --][-- MemberExpression                                                                  --]
//     ↓          ↓
// [-- new --][-- PrimaryExpression                                                                 --]
//     ↓          ↓        ↓                                                                      ↓
// [-- new --][-- ( --][-- Expression                                                      --][-- ) --]
//     ↓          ↓        ↓                                                                      ↓
// [-- new --][-- ( --][-- CallExpression                                                  --][-- ) --]
//     ↓          ↓        ↓                                                     ↓                ↓
// [-- new --][-- ( --][-- CallExpression                                 --][-- Arguments --][-- ) --]
//     ↓          ↓        ↓                                 ↓ ↓                 ↓                ↓
// [-- new --][-- ( --][-- CallExpression             --][-- . Identifier --][-- Arguments --][-- ) --]
//     ↓          ↓        ↓                ↓                ↓ ↓                 ↓                ↓
// [-- new --][-- ( --][-- MemberExpression Arguments --][-- . Identifier --][-- Arguments --][-- ) --]
//     ↓          ↓        ↓                ↓                ↓ ↓                 ↓                ↓
// [-- new --][-- ( --][-- Identifier       Arguments --][-- . Identifier --][-- Arguments --][-- ) --]
//     ↓          ↓        ↓                ↓                ↓ ↓                 ↓                ↓
       new        (        ClassName        ()               . something         ()               )
```

In this case, the parent AST node becomes `NewExpression` and has a
`CallExpression` as a child of it. This is reversed from previously where
`CallExpression` is the parent and `NewExpression` is the child.

## Conclusion

There's no confusion or ambiguity of how `new ClassName().something()`
should be read. Whenever you see code like this, remember to read it as an
overall `CallExpression`. The only exception is if there are brackets
surrounding the stuff after `new`. In the case `new (ClassName().something())`,
the overall AST node is a `NewExpression`.

How you should deal with the difference between human thinking and parser
thinking depends on your team's coding convention and decision.

  [1]: http://withkash.com
  [2]: https://github.com/dannysu/ecmascript1
  [3]: https://github.com/ternjs/acorn
  [4]: https://dannysu.com/es1-left-recursive-grammar/
