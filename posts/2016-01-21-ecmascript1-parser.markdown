---
date: 
title: ECMAScript 1 (ES1) Parser
tags: Parser, Compiler, ECMAScript, JavaScript, ReactJS, JSX, ESLint, UglifyJS
---
Although I took a compiler course while in university over 10 years ago, I
don't write parsers or compilers on a day-to-day basis. So even though I
understand that parsers are probably what's behind some of the JavaScript tools
I use, it sometimes feel a bit like magic.

Today we enjoy tools like [Babel][2], [ReactJS][3], [ESLint][4] or [JSHint][11],
and [UglifyJS][5], but my knowledge of them mainly revolves around using them
as a consumer. Therefore I set out to rectify this gap and wrote an ECMAScript
v1 parser. [Source code][1] on GitHub.

<br/><a name="toc"></a>

**Table of Contents**

 - <a href="#research">Parsers Everywhere</a>
 - <a href="#lexer">Step 1: Lexer</a>
     - <a href="#lexer_foundation">1.1: Lexer Foundation Functions</a>
     - <a href="#code_comments">1.2: Code Comments</a>
     - <a href="#other_tokens">1.3: Other Tokens</a>
 - <a href="#parser">Step 2: Parser</a>
     - <a href="#non_left_recursive">2.1: Grammar With Left-Recursion Eliminated</a>
     - <a href="#stuck_on_expression">2.2: Stuck on 1.1 + 2e2 + 3^2</a>
     - <a href="#precedence_climbing">2.3: Operator Precedence & Precedence Climbing</a>
     - <a href="#conditional_vs_lhs">2.4: ConditionalExpression vs LeftHandSideExpression</a>
     - <a href="#new_vs_call">2.5: NewExpression vs CallExpression vs
       MemberExpression</a>
 - <a href="#interesting_es1">Interesting Things About ES1</a>
 - <a href="#jsx">Implement JSX Extension</a>
 - <a href="#minifier">Implement a Minifier</a>
 - <a href="#linter">Implement a Linter</a>
 - <a href="#ka_live_editor">Khan Academy's Live Editor</a>
 - <a href="#next">What's Next</a>


<br/><a name="research"></a>

# Parsers Everywhere

I started my learning by researching how some of the tools work. From this
research I found out that Babel uses the [Babylon parser][6]. I found out that
ReactJS uses a [fork of the Esprima parser][7]. I also found out that ReactJS
appears to have plan to transition to use the [Acorn parser][8]. Similarly to
ReactJS, ESLint also uses a fork of Esprima named [Espree][9]. JSHint uses
Esprima as well. Lastly, UglifyJS has [its own parser][10].

Once I confirmed that parser underlies these tools, an idea came to me that I
should go write my own parser for v1 of JavaScript. I figured that the syntax
is simpler when JavaScript was just invented, so it'd make my job easier but
still allow me get a firm grasp of what the tools are doing. I liked that doing
so allows me to travel back to around 1997, a time when I was just starting to
learn English. I also liked that I could try to implement JSX support, making
it a 1997ish meets 2013ish combination.

<a name="lexer"></a>

# Step 1: Lexer

The first step to writing a parser is to write a lexer, which is also called
tokenizer or scanner. I started by reading the [original ECMAScript
standard][14] that was finalized in June 1997. You can find links to the
ECMAScript standards on the [Mozilla Developer Network site][12].

Section 7 is the part that you're interested in for this. It specifies the
*Lexical Conventions* and identifies all the individual tokens of the language.

Based on reading the spec, I coded up the first version of my lexer, but it
wasn't very good. I then happened to watch [Rob Pike's talk on lexer][13] for Go
and adopted ideas from there to form the foundation in my second iteration.

<a name="lexer_foundation"></a>

## 1.1: Lexer Foundation Functions

A lexer's job is to read the input string character by character and separate
them into tokens for processing later. In Rob Pike's lexer, there is a variable
`start` that marks the start of the token being processed. The end of a token
is being marked by a variable named `pos`. The `pos` variable gets incremented
each time a character is deemed to be belonging to the same token. When it is
decided that a token is finished, then one can take the substring from `start`
to `pos` and advance `start`.

[![][15]][15]

For my lexer, the reading of a character is done by a function named `peek`. It
reads the next character to be processed and returns it. However, the `peek`
function doesn't advance the `pos` position because sometime you need to see
what's next in order to decide how to process the input text.

```javascript
// Returns the next character in the source code
l.peek = function() {
    if (this.pos >= this.source.length) {
        // null represents EOF
        return null;
    }

    const c = this.source[this.pos];
    return c;
};
```

Other functions make use of the `peek` function to decide what to do. For
example, the `acceptRun` function uses `peek` to decide whether to keep
including characters into the current token. I've modified Rob Pike's
`acceptRun` to instead take a validator function since it's more flexible that
way and I use it to accumulate various types of tokens.

```javascript
l.acceptRun = function(validator) {
    let c;
    let startedAt = this.pos;
    do {
        c = this.peek();
        if (c === null) {
            break;
        }
    } while(validator(c) && ++this.pos);

    return (this.pos > startedAt);
};
```

The last function needed before being able to write the first iteration of the
lexer is the `ignore` function. It advances the `start` variable without doing
anything with the characters accumulated between `start` and `pos`. This is
used to discard things in the source code such as whitespaces or code comments.

```javascript
// Skips over the pending input before this point
l.ignore = function() {
    this.start = this.pos;
};
```

<a name="code_comments"></a>

## 1.2: Code Comments

One of the easiest things to process is a single-line code comment. In the
ECMAScript 1 standard it's specified as follows:

> <br/>
> SingleLineComment ::<br/>
> <span class="left-margin-40">`//` SingleLineCommentChars<sub>opt</sub></span><br/>
> SingleLineCommentChars ::<br/>
> <span class="left-margin-40">SingleLineCommentChar SingleLineCommentChars<sub>opt</sub></span><br/>
> SingleLineCommentChar ::<br/>
> <span class="left-margin-40">SourceCharacter **but not** LineTerminator</span>  

The standard says that a single-line comment starts with two forward slash
characters (`//`) followed by 0 or more `SingleLineCommentChars`.
`SingleLineCommentChars` is just source code character that is not a
`LineTerminator` (Carriage Return or Line Feed).

The lexer has a main function named `lexText` that keeps processing characters
until the end. To know that we've encountered a single-line comment, the
`lexText` function peeks at the next two characters to decide whether to call
`lexSingleLineComment` function to deal with it.

```javascript
l.lexText = function() {
    do {
        // Examine the next 2 characters to see if we're encountering code comments
        const nextTwo = this.source.substr(this.pos, 2);
        if (nextTwo === '//') {
            this.pos += 2;
            return this.lexSingleLineComment;
        }
    } while(true);
};
```

The `lexSingleLineComment` function utilizes the `acceptRun` and `ignore`
functions introduced earlier. There are couple helper functions as well. The
`not` function inverts the boolean result from the function passed to it. The
`isLineTerminator` function does what its name says and returns a boolean.

The `lexSingleLineComment` implementation below says "Keep accumulating
characters into the current token as long as it's not a line terminator". Then
once line terminator or end of input is reached, call `ignore()` to discard the
token.

```javascript
function not(fn) {
    return function(c) {
        const result = fn(c);
        return !result;
    };
}

function isLineTerminator(c) {
    if (c === '\n' || c === '\r') {
        return true;
    }
    return false;
}

l.lexSingleLineComment = function() {
    // Single line comment is only terminated by a line terminator
    // character and nothing else
    this.acceptRun(not(isLineTerminator));
    this.ignore();
    return this.lexText;
};
```

That's how my lexer processes single-line comment. It accumulates and then
discards code comment so that the later stages don't need to deal with them.

<a name="other_tokens"></a>

## 1.3: Other Tokens

Other types of tokens are processed similarly to single-line comment by
utilizing the foundation functions and additional helper functions. The code
for those are more verbose so I won't go through them here, but the concept is
the same. Have a look at `src/lexer.js` for the complete source code for the
lexer. You can build up the lexer incrementally by adding one type of token
processing each time and write tests.

<a name="parser"></a>

# Step 2: Parser

Once the lexer finishes processing we're left with a sequence of tokens to do
two things:

1. Determine if the token sequence conforms to the JavaScript grammar
1. Produce an Abstract Syntax Tree (AST) structure that can be used to build other tools

ECMAScript standard sections 11 to 14 specifies the language grammar. I've
compiled the grammar into a single page [here][16].

When one begins to look at parsing, there are many different approaches.
There are [LL][18] parsers, [LR][20] parsers and its variants ([GLR][22],
[LALR][19] and [SLR][21]), and more! There are also a variety of parser
generators, and lexer generators for that matter, that one can choose
from.

To get a sense of how others have done it, I skimmed over Esprima and Acorn
source code. From what I can tell from a quick scan, they're using recursive
descent, hand-coded variety. Therefore, that's what I'll do too: hand-code a
[recursive descent parser][23]. Studies on other types of parsers will have to
wait for another time since an exhaustive parsing technique study isn't my main
objective.

<a name="non_left_recursive"></a>

## 2.1: Grammar with Left-Recursion Eliminated

A recursive descent parser can parse grammar that doesn't contain
left-recursion. However, the original [ECMAScript grammar][16] contains a bunch
of left-recursions that'll be troublesome for a recursive descent parser. Step
one for beginning the parser is to convert the grammar by eliminating
left-recursion as I have done so [here][17].

It is interesting to note that the method for eliminating left-recursion also
messes with operator associativity, but I'll deal with it later.

Once we have the non-recursive grammar, writing the parser follows closely to
the grammar specification. For example, mine starts with `parseProgram` and
`parseSourceElement` functions similar to how the grammar is specified. Code
for the parser is in `src/parser.js`.

<a name="stuck_on_expression"></a>

## 2.2: Stuck on 1.1 + 2e2 + 3^2

One of the first problems I ran into was when I wrote a test case to parse the
statement `var a = 1.1 + 2e2 + 3^2;`. The parsing sequence that happens in my
initial implementation goes as follows:

```html
VariableStatement
  var VariableDeclarationList ;
    VariableDeclaration
      Identifier Initializer
        = AssignmentExpression
          ConditionalExpression
            LogicalORExpression
              LogicalANDExpression
                BitwiseORExpresion
                  BitwiseXORExpression
                    BitwiseANDExpression
                      EqualityExpression
                        RelationalExpression
                          ShiftExpression
                            AdditiveExpression + MultiplicativeExpression
```

The problem is that when `AdditiveExpression` expands to
`MultiplicativeExpression`, it is unable to then go back up the chain and parse
`^2` as `BitwiseXORExpression`.

I was stuck on this for a while and I really wanted to understand it and not
rush it. I knew from reading the header comment in Acorn parser's
[expression.js][24] that there are ways to deal with it. However, I could not
see how I could do it with the recursive descent parser I was writing. One of
my questions was whether that's even a valid expression since my parser was
having trouble. A quick check with node.js confirms that it is indeed valid.

So how is that possible?

Turns out for the parser to be able to parse the expression, it somehow needs
to know to expand `AssignmentExpression` to a `BitwiseXORExpression` first
separating the part containing `1.1 + 2e2 + 3` from `2`. Then parse those two
parts as `BitwiseANDExpression` and continue from there.

I don't think a simple recursive descent parser can deal with the JavaScript
grammar. A simplistic parser would have to somehow know that `^` occurs several
tokens later and know to expand `AssignmentExpression` to `BitwiseXORExpression`
instead. Once I understand how the expression conforms to the grammar I was
happy to move on and solve the problem using precedence climbing as discussed
next.

<a name="precedence_climbing"></a>

## 2.3: Operator Precedence & Precedence Climbing

From the way the JavaScript grammar was specified, one can deduce the operator
precedence from the hierarchy. For example, `AdditiveExpression` expands to
`MultiplicativeExpression`. That means multiplications have higher precedence than
additions. An expression like `1 + 2 * 3` would mean `1 + (2 * 3)`. A full list
of operator precedence levels in ES6 and beyond can be found [here][25].

For ES1 it's just a subset of what's listed:

```javascript
const operatorPrecedence = {
    '||': 0,
    '&&': 1,
    '|': 2,
    '^': 3,
    '&': 4,
    '==': 5,
    '!=': 5,
    '<': 6,
    '>': 6,
    '<=': 6,
    '=>': 6,
    '<<': 7,
    '>>': 7,
    '>>>': 7,
    '+': 8,
    '-': 8,
    '*': 9,
    '/': 9,
    '%': 9
};
```

Knowing the operator precedence levels, one can then use an algorithm called
[precedence climbing][26] to deal with the `1.1 + 2e2 + 3^2` problem in the
previous section. There are other algorithms such as the shunting-yard
algorithm, but I went with precedence climbing as it seems simpler.

Luckily in ES1 all binary expressions are left associative, so the part of
precedence climbing that deals with right associativity can be removed. In ES7
though, the exponentiation (\*\*) operator would introduce right associative
operator to the picture.

My implementation of precedence climbing can be found in the
`parseBinaryExpression` function. The parts dealing with `ast` and `lhs` can be
ignored for now. Those are discussed in the next section.

```javascript
// Uses precedence climbing to deal with binary expressions, all of which have
// left-to-right associtivity in this case.
p.parseBinaryExpression = function(minPrecedence) {
    const punctuators = [
        '||', '&&', '|', '^', '&', '==', '!=', '<', '>', '<=', '=>',
        '<<', '>>', '>>>', '+', '-', '*', '/', '%'
    ];

    const result = this.parseUnaryExpression();
    let ast = result.ast;
    let lhs = result.lhs

    while (this.matchPunctuators(punctuators) &&
           operatorPrecedence[this.next().value] >= minPrecedence) {

        // If any operator is encountered, then the result cannot be
        // LeftHandSideExpression anymore
        lhs = false;

        const precedenceLevel = operatorPrecedence[this.next().value];
        const operatorToken = this.expectPunctuators(punctuators);

        const right = this.parseBinaryExpression(precedenceLevel + 1);
        if (operatorToken.value === '||' || operatorToken.value === '&&') {
            ast = new estree.LogicalExpression(operatorToken.value, ast, right.ast);
        }
        else {
            ast = new estree.BinaryExpression(operatorToken.value, ast, right.ast);
        }
    }

    return {
        ast: ast,
        lhs: lhs
    };
};
```

<a name="conditional_vs_lhs"></a>

## 2.4: ConditionalExpression vs LeftHandSideExpression

Another challenge when parsing JavaScript is when the parser needs to parse an
`AssignmentExpression`. The parser needs to decide whether its parsing a
`ConditionalExpression` or a `LeftHandSideExpression` plus other stuff. The
grammar is shown below:

> <br/>
> AssignmentExpression:<br/>
> <span class="left-margin-40">ConditionalExpression</span><br/>
> <span class="left-margin-40">LeftHandSideExpression AssignmentOperator AssignmentExpression</span>

The problem is that `ConditionalExpression` can also expand to just
`LeftHandSideExpression`, so you can't just have an if-statement there and call
it done.

How I solved it is by delaying the decision until later. When parsing
`AssignmentExpression`, there is no if-statement initially. The code simply
attempts to parse `ConditionalExpression` immediately. Whether
`ConditionalExpression` is expanded into `LeftHandSideExpression` is decided
later and passed back up the chain. This way the parser doesn't need to
backtrack by trying to parse as one and discard result if that fails and try
another one.

The information of whether `ConditionalExpression` is actually a
`LeftHandSideExpression` is revealed when parsing `PostfixExpression`. If the parser
sees `LeftHandSideExpression` followed by `++` or `--` then it's not a lone
`LeftHandSideExpression`.

> <br/>
> PostfixExpression:<br/>
> <span class="left-margin-40">LeftHandSideExpression</span><br/>
> <span class="left-margin-40">LeftHandSideExpression [no LineTerminator here] ++</span><br/>
> <span class="left-margin-40">LeftHandSideExpression [no LineTerminator here] --</span>

<a name="new_vs_call"></a>

## 2.5: NewExpression vs CallExpression vs MemberExpression

A very similar challenge comes up when trying to parse
`LeftHandSideExpression`, which can be expanded to either `NewExpression` or
`CallExpression`. I applied the same solution of delaying decision as you can
see in the function declaration of `parseNewOrCallOrMemberExpression`. There is
no decision making in `parseLeftHandSideExpression` at all.

```javascript
p.parseNewOrCallOrMemberExpression = function(couldBeNewExpression, couldBeCallExpression) {
};

p.parseLeftHandSideExpression = function() {
    return this.parseNewOrCallOrMemberExpression(true, true).object;
};
```

In this case, the distinction whether we're dealing with `NewExpression` vs
`MemberExpression`, which comes later on, is by encountering `Arguments`. If
`Arguments` is found, then it is a `MemberExpression` and not `NewExpression`.
Similarly, if after parsing `MemberExpression` the parser encounters more
`Arguments`, then it's a `CallExpression`.

<a name="interesting_es1"></a>

# Interesting Things About ES1

I noticed some interesting things after I finished the parser because I tried
to parse real-world code and ran into issues.

Back in 1997, JavaScript didn't support regular expressions. It's so widely used
in source code nowadays and that prevented my parser from processing the source
code.

Another difference is that there is no such thing as `FunctionExpression`. ES1
has `FunctionDeclaration` but not the expression equivalent. All those `var a =
function(){};` statements are not valid syntax! No lambdas for you in 1997!

This omission is rectified by ES3 in 1999 though. Funny thing is in 1999 I think
I just advanced from ESL to regular English class. I wasn't even concerned
about programming back then let alone lambdas.

<a name="jsx"></a>

# Implement JSX Extension

At my company Kash, we implemented our online checkout interface in ReactJS, so
it's of interest to me. ReactJS makes use of JSX files which is this weird
JavaScript syntax mixed in with HTML tags.

An example JSX file looks like this:

```javascript
// Using JSX to express UI components.
var dropdown =
  <Dropdown>
    A dropdown list
    <Menu>
      <MenuItem>Do Something</MenuItem>
      <MenuItem>Do Something Fun!</MenuItem>
      <MenuItem>Do Something Else</MenuItem>
    </Menu>
  </Dropdown>;

render(dropdown);
```

Once I finished implementing my parser, I implemented an incomplete version of
the [JSX extension][27] to get a feel for it. Parsers are really driving a ton
of things we use today. My parser implementation is in the [jsx branch][28].
`var a = <div></div>;` now produces an Abstract Syntax Tree! We now time
travelled to 2013!

Just having a parser that understands JSX files aren't enough though. ReactJS
would have to take the AST and convert it to proper ECMAScript to be executed in
the browser. In the next section when I examine minifiers, I cover the task of
transforming the AST to output a different JavaScript source code.

<a name="minifier"></a>

# Implement a Minifier

I use UglifyJS to minify JavaScript code before deployment. A JavaScript
minifier removes whitespaces, removes new lines, and lots other things. But how
does one go about implementing a minifier?

Well, once the parser finishes parsing the source code, it produces an AST. My
parser spits out AST in the [ESTree][29] format, so does Esprima and Acorn
parsers. UglifyJS, on the other hand, has its own format from its parser but can
import ESTree format to be processed.

One of the things that a minifier will do is shorten the function names because
to a JavaScript engine, our `veryMaintainableFunctionName` isn't meaningful. In
order to achieve this renaming we need to first figure out what functions are
there. To do so we need to process the AST starting from the root `Program`
node.

The ESTree format specifies that the `Program` node is identified by `type =
'Program'` and has a member property `body` that contains an array of
statements. Our first task is to figure out what function declarations there are
and we can do this simply by looping over the `body` array like the following
code:

```javascript
if (node.type === 'Program') {
    // First, go through all statements in the source code to gather function
    // names
    const newScope = {}; // mapping of old function name to new function name
    for (let stmt of node.body) {
        if (stmt.type === 'FunctionDeclaration') {
            // FunctionDeclaration is specified in ESTree to have an 'id'
            // property, which is an Identifier with a 'name' property.
            newScope[stmt.id.name] = stmt.id.name;
        }
    }
}
```

Once we've obtained all function names, we can remap them to something else. For
example, turning `veryMaintainableFunctionName` into just `a`. To do this the
minifier needs to find all instances of `CallExpression` and instead of printing
out the original function `Identifier` being referenced, swap it with our
substitution. Below is a quick example but it doesn't deal with more than two
scopes:

```javascript
if (node.type === 'Identifier') {
    let scope = scopes[scopes.length - 1];
    // When we can't find the variable in the scope, look in the parent scope!
    if (!scope[node.name]) {
        scope = scopes[scopes.length - 2]
    }
    minified += scope[node.name]; // Substitute function name being referenced
}
```

There is of course a lot more to a minifier. A good one probably also makes use
of JavaScript's automatic semicolon insertion rules if the goal is to optimize
for file size.

<a name="linter"></a>

# Implement a Linter

Implementing a linter similarly needs to process the AST to intelligently figure
out what's in a program source code. For example, I use ESLint at work and
typically will turn on the `no-var` rule for ES6. Doing so gives me a warning
whenever I use `var` instead of new ES6 constructs like `const` and `let`.

To implement such a warning is fairly simple, the linter needs to disallow
`VariableDeclaration` and only allow the use of `LexicalDeclaration` in ES6.
Again, lots more to a linter than a simplistic example, but it all makes sense
once you know how the AST is being used.

<a name="ka_live_editor"></a>

# Khan Academy's Live Editor

[Khan Academy][30]'s Computer Science and Computer Programming subjects feature
an online coding environment that gives you helpful hints as you code. I was
quite impressed when I first tried it. Curious about how it works and what
parsers they use, I did a bit of researching.

The online text editing environment is called [Live Code Editor][31] and uses
JSHint as the linter for providing helpful messages in JavaScript code. It also
uses a [fork of Slowparse][32] for parsing HTML.

Very cool! Parsers are all around us.

<a name="next"></a>

# What's Next

There is so much one can learn in this area. There are the different parsing
techniques. There's the [TDOP algorithm used by JSLint][33]. There's the
automatic semicolon insertion rules that I didn't implement. There are things
in the spec that I have yet to handle like restricting line terminator from
appearing in `PostfixExpression`. Also, since these parsers are executing in
the browser, speed is another factor. Esprima website has a [speed
comparison][35] of different parsers. Aside from speed, there's something
called *Early Error* that I don't know much about in ES6. Also, I think it'd be
interesting to morph my ES1 parser by adding newer standards to get a sense of
how challenging each new language addition is to implement. I could also learn
to use the various parser generators like [Parsec that's mentioned in the Real
World Haskell book][34].

Reading through the standard for all the different versions would be a nice
thing to do too. For example, section 4.2.1 in the ES1 standard doc talks about
prototypical inheritance and its behaviour.

That said, I'm satisfied with my little learning project for now. There are lots
of other things to learn, and I've got my eyes set on my next target.

  [1]: https://github.com/dannysu/ecmascript1
  [2]: http://babeljs.io/
  [3]: http://facebook.github.io/react/
  [4]: http://eslint.org/
  [5]: http://lisperator.net/uglifyjs/
  [6]: https://www.npmjs.com/package/babylon
  [7]: https://github.com/facebook/esprima
  [8]: https://github.com/ternjs/acorn
  [9]: https://github.com/eslint/espree
  [10]: https://github.com/mishoo/UglifyJS
  [11]: http://jshint.com/
  [12]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Language_Resources
  [13]: https://www.youtube.com/watch?v=HxaD_trXwRE
  [14]: http://www.ecma-international.org/publications/files/ECMA-ST-ARCH/ECMA-262,%201st%20edition,%20June%201997.pdf
  [15]: //imagedatastore.appspot.com/ahBzfmltYWdlZGF0YXN0b3Jlcg4LEgVpbWFnZRj50vsBDA
  [16]: /es1-left-recursive-grammar/
  [17]: /es1-non-left-recursive-grammar/
  [18]: https://en.wikipedia.org/wiki/LL_parser
  [19]: https://en.wikipedia.org/wiki/LALR_parser
  [20]: https://en.wikipedia.org/wiki/LR_parser
  [21]: https://en.wikipedia.org/wiki/Simple_LR_parser
  [22]: https://en.wikipedia.org/wiki/GLR_parser
  [23]: https://en.wikipedia.org/wiki/Recursive_descent_parser
  [24]: https://github.com/ternjs/acorn/blob/master/src/expression.js
  [25]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence
  [26]: http://www.engr.mun.ca/~theo/Misc/exp_parsing.htm#climbing
  [27]: https://facebook.github.io/jsx/
  [28]: https://github.com/dannysu/ecmascript1/tree/jsx
  [29]: https://github.com/estree/estree
  [30]: https://www.khanacademy.org/
  [31]: https://github.com/Khan/live-editor
  [32]: https://github.com/Khan/live-editor/blob/master/external/slowparse/slowparse.js
  [33]: http://javascript.crockford.com/tdop/tdop.html
  [34]: http://book.realworldhaskell.org/read/using-parsec.html
  [35]: http://esprima.org/test/compare.html
