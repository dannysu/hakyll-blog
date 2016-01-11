---
title: ECMAScript 1 Grammar
---
ECMAScript grammar as specified in [ECMA-262 1st edition][1]. This grammar is
*left recursive* and is not friendly for LL parsers.

---

<a name="Program"></a>
Program:  
<span class="left-margin-40"><a href="#SourceElements">SourceElements</a></span>  

<a name="SourceElements"></a>
SourceElements:  
<span class="left-margin-40"><a href="#SourceElement">SourceElement</a></span>  
<span class="left-margin-40">SourceElements <a href="#SourceElement">SourceElement</a></span>  

<a name="SourceElement"></a>
SourceElement:  
<span class="left-margin-40"><a href="#Statement">Statement</a></span>  
<span class="left-margin-40"><a href="#FunctionDeclaration">FunctionDeclaration</a></span>  

<a name="Statement"></a>
Statement:  
<span class="left-margin-40"><a href="#Block">Block</a></span>  
<span class="left-margin-40"><a href="#VariableStatement">VariableStatement</a></span>  
<span class="left-margin-40"><a href="#EmptyStatement">EmptyStatement</a></span>  
<span class="left-margin-40"><a href="#ExpressionStatement">ExpressionStatement</a></span>  
<span class="left-margin-40"><a href="#IfStatement">IfStatement</a></span>  
<span class="left-margin-40"><a href="#IterationStatement">IterationStatement</a></span>  
<span class="left-margin-40"><a href="#ContinueStatement">ContinueStatement</a></span>  
<span class="left-margin-40"><a href="#BreakStatement">BreakStatement</a></span>  
<span class="left-margin-40"><a href="#ReturnStatement">ReturnStatement</a></span>  
<span class="left-margin-40"><a href="#WithStatement">WithStatement</a></span>  

<a name="Block"></a>
Block:  
<span class="left-margin-40">`{` <a href="#StatementList">StatementList</a><sub>opt</sub> `}`</span>  

<a name="StatementList"></a>
StatementList:  
<span class="left-margin-40"><a href="#Statement">Statement</a></span>  
<span class="left-margin-40">StatementList <a href="#Statement">Statement</a></span>  

<a name="VariableStatement"></a>
VariableStatement:  
<span class="left-margin-40">`var` <a href="#VariableDeclarationList">VariableDeclarationList</a> `;`</span>  

<a name="VariableDeclarationList"></a>
VariableDeclarationList:  
<span class="left-margin-40"><a href="#VariableDeclaration">VariableDeclaration</a></span>  
<span class="left-margin-40">VariableDeclarationList `,` <a href="#VariableDeclaration">VariableDeclaration</a></span>  

<a name="VariableDeclaration"></a>
VariableDeclaration:  
<span class="left-margin-40">Identifier <a href="#Initializer">Initializer</a><sub>opt</sub></span>  
<span class="left-margin-40"><span class="left-margin-40"><small>\*Identifier as defined in Chapter 7 of the spec</small></span></span>  

<a name="Initializer"></a>
Initializer:  
<span class="left-margin-40">`=` <a href="#AssignmentExpression">AssignmentExpression</a></span>  

<a name="AssignmentExpression"></a>
AssignmentExpression:  
<span class="left-margin-40"><a href="#ConditionalExpression">ConditionalExpression</a></span>  
<span class="left-margin-40"><a href="#LeftHandSideExpression">LeftHandSideExpression</a> AssignmentOperator AssignmentExpression</span>  
<span class="left-margin-40"><span class="left-margin-40"><small>\*AssignmentOperator as defined in Chapter 7 of the spec</small></span></span>  

<a name="ConditionalExpression"></a>
ConditionalExpression:  
<span class="left-margin-40"><a href="#LogicalORExpression">LogicalORExpression</a></span>  
<span class="left-margin-40"><a href="#LogicalORExpression">LogicalORExpression</a> `?` <a href="#AssignmentExpression">AssignmentExpression</a> `:` <a href="#AssignmentExpression">AssignmentExpression</a></span>  

<a name="LogicalORExpression"></a>
LogicalORExpression:  
<span class="left-margin-40"><a href="#LogicalANDExpression">LogicalANDExpression</a></span>  
<span class="left-margin-40">LogicalORExpression `||` <a href="#LogicalANDExpression">LogicalANDExpression</a></span>  

<a name="LogicalANDExpression"></a>
LogicalANDExpression:  
<span class="left-margin-40"><a href="#BitwiseORExpression">BitwiseORExpression</a></span>  
<span class="left-margin-40">LogicalANDExpression `&&` <a href="#BitwiseORExpression">BitwiseORExpression</a></span>  

<a name="BitwiseORExpression"></a>
BitwiseORExpression:  
<span class="left-margin-40"><a href="#BitwiseXORExpression">BitwiseXORExpression</a></span>  
<span class="left-margin-40">BitwiseORExpression `|` <a href="#BitwiseXORExpression">BitwiseXORExpression</a></span>  

<a name="BitwiseXORExpression"></a>
BitwiseXORExpression:  
<span class="left-margin-40"><a href="#BitwiseANDExpression">BitwiseANDExpression</a></span>  
<span class="left-margin-40">BitwiseXORExpression `^` <a href="#BitwiseANDExpression">BitwiseANDExpression</a></span>  

<a name="BitwiseANDExpression"></a>
BitwiseANDExpression:  
<span class="left-margin-40"><a href="#EqualityExpression">EqualityExpression</a></span>  
<span class="left-margin-40">BitwiseANDExpression `&` <a href="#EqualityExpression">EqualityExpression</a></span>  

<a name="EqualityExpression"></a>
EqualityExpression:  
<span class="left-margin-40"><a href="#RelationalExpression">RelationalExpression</a></span>  
<span class="left-margin-40">EqualityExpression `==` <a href="#RelationalExpression">RelationalExpression</a></span>  
<span class="left-margin-40">EqualityExpression `!=` <a href="#RelationalExpression">RelationalExpression</a></span>  

<a name="RelationalExpression"></a>
RelationalExpression:  
<span class="left-margin-40"><a href="#ShiftExpression">ShiftExpression</a></span>  
<span class="left-margin-40">RelationalExpression `<` <a href="#ShiftExpression">ShiftExpression</a></span>  
<span class="left-margin-40">RelationalExpression `>` <a href="#ShiftExpression">ShiftExpression</a></span>  
<span class="left-margin-40">RelationalExpression `<=` <a href="#ShiftExpression">ShiftExpression</a></span>  
<span class="left-margin-40">RelationalExpression `=>` <a href="#ShiftExpression">ShiftExpression</a></span>  

<a name="ShiftExpression"></a>
ShiftExpression:  
<span class="left-margin-40"><a href="#AdditiveExpression">AdditiveExpression</a></span>  
<span class="left-margin-40">ShiftExpression `<<` <a href="#AdditiveExpression">AdditiveExpression</a></span>  
<span class="left-margin-40">ShiftExpression `>>` <a href="#AdditiveExpression">AdditiveExpression</a></span>  
<span class="left-margin-40">ShiftExpression `>>>` <a href="#AdditiveExpression">AdditiveExpression</a></span>  

<a name="AdditiveExpression"></a>
AdditiveExpression:  
<span class="left-margin-40"><a href="#MultiplicativeExpression">MultiplicativeExpression</a></span>  
<span class="left-margin-40">AdditiveExpression `+` <a href="#MultiplicativeExpression">MultiplicativeExpression</a></span>  
<span class="left-margin-40">AdditiveExpression `-` <a href="#MultiplicativeExpression">MultiplicativeExpression</a></span>  

<a name="MultiplicativeExpression"></a>
MultiplicativeExpression:  
<span class="left-margin-40"><a href="#UnaryExpression">UnaryExpression</a></span>  
<span class="left-margin-40">MultiplicativeExpression `*` <a href="#UnaryExpression">UnaryExpression</a></span>  
<span class="left-margin-40">MultiplicativeExpression `/` <a href="#UnaryExpression">UnaryExpression</a></span>  
<span class="left-margin-40">MultiplicativeExpression `%` <a href="#UnaryExpression">UnaryExpression</a></span>  

<a name="UnaryExpression"></a>
UnaryExpression:  
<span class="left-margin-40"><a href="#PostfixExpression">PostfixExpression</a></span>  
<span class="left-margin-40">`delete` UnaryExpression</span>  
<span class="left-margin-40">`void` UnaryExpression</span>  
<span class="left-margin-40">`typeof` UnaryExpression</span>  
<span class="left-margin-40">`++` UnaryExpression</span>  
<span class="left-margin-40">`--` UnaryExpression</span>  
<span class="left-margin-40">`+` UnaryExpression</span>  
<span class="left-margin-40">`-` UnaryExpression</span>  
<span class="left-margin-40">`~` UnaryExpression</span>  
<span class="left-margin-40">`!` UnaryExpression</span>  

<a name="PostfixExpression"></a>
PostfixExpression:  
<span class="left-margin-40"><a href="#LeftHandSideExpression">LeftHandSideExpression</a></span>  
<span class="left-margin-40"><a href="#LeftHandSideExpression">LeftHandSideExpression</a> <small>*[no LineTerminator here]*</small> `++`</span>  
<span class="left-margin-40"><a href="#LeftHandSideExpression">LeftHandSideExpression</a> <small>*[no LineTerminator here]*</small> `--`</span>  

<a name="LeftHandSideExpression"></a>
LeftHandSideExpression:  
<span class="left-margin-40"><a href="#NewExpression">NewExpression</a></span>  
<span class="left-margin-40"><a href="#CallExpression">CallExpression</a></span>  

<a name="NewExpression"></a>
NewExpression:  
<span class="left-margin-40"><a href="#MemberExpression">MemberExpression</a></span>  
<span class="left-margin-40">`new` NewExpression</span>  

<a name="MemberExpression"></a>
MemberExpression:  
<span class="left-margin-40"><a href="#PrimaryExpression">PrimaryExpression</a></span>  
<span class="left-margin-40">MemberExpression `[` <a href="#Expression">Expression</a> `]`</span>  
<span class="left-margin-40">MemberExpression `.` Identifier</span>  
<span class="left-margin-40">`new` MemberExpression <a href="#Arguments">Arguments</a></span>  

<a name="PrimaryExpression"></a>
PrimaryExpression:  
<span class="left-margin-40">`this`</span>  
<span class="left-margin-40">Identifier</span>  
<span class="left-margin-40">Literal</span>  
<span class="left-margin-40">`(` <a href="#Expression">Expression</a> `)`</span>  
<span class="left-margin-40"><span class="left-margin-40"><small>\*Literal as defined in Chapter 7 of the spec</small></span></span>  

<a name="Expression"></a>
Expression:  
<span class="left-margin-40"><a href="#AssignmentExpression">AssignmentExpression</a></span>  
<span class="left-margin-40">Expression `,` <a href="#AssignmentExpression">AssignmentExpression</a></span>  

<a name="Arguments"></a>
Arguments:  
<span class="left-margin-40">`(` `)`</span>  
<span class="left-margin-40">`(` <a href="#ArgumentList">ArgumentList</a> `)`</span>  

<a name="ArgumentList"></a>
ArgumentList:  
<span class="left-margin-40"><a href="#AssignmentExpression">AssignmentExpression</a></span>  
<span class="left-margin-40">ArgumentList `,` <a href="#AssignmentExpression">AssignmentExpression</a></span>  

<a name="CallExpression"></a>
CallExpression:  
<span class="left-margin-40"><a href="#MemberExpression">MemberExpression</a> <a href="#Arguments">Arguments</a></span>  
<span class="left-margin-40">CallExpression <a href="#Arguments">Arguments</a></span>  
<span class="left-margin-40">CallExpression `[` <a href="#Expression">Expression</a> `]`</span>  
<span class="left-margin-40">CallExpression `.` Identifier</span>  

<a name="EmptyStatement"></a>
EmptyStatement:  
<span class="left-margin-40">`;`</span>  

<a name="ExpressionStatement"></a>
ExpressionStatement:  
<span class="left-margin-40"><a href="#Expression">Expression</a> `;`</span>  

<a name="IfStatement"></a>
IfStatement:  
<span class="left-margin-40">`if` `(` <a href="#Expression">Expression</a> `)` <a href="#Statement">Statement</a> `else` <a href="#Statement">Statement</a></span>  
<span class="left-margin-40">`if` `(` <a href="#Expression">Expression</a> `)` <a href="#Statement">Statement</a></span>  

<a name="IterationStatement"></a>
IterationStatement:  
<span class="left-margin-40">`while` `(` <a href="#Expression">Expression</a> `)` <a href="#Statement">Statement</a></span>  
<span class="left-margin-40">`for` `(` <a href="#Expression">Expression</a><sub>(opt)</sub> `;` <a href="#Expression">Expression</a><sub>(opt)</sub> `;` <a href="#Expression">Expression</a><sub>(opt)</sub> `)` <a href="#Statement">Statement</a></span>  
<span class="left-margin-40">`for` `(` `var` <a href="#VariableDeclarationList">VariableDeclarationList</a> `;` <a href="#Expression">Expression</a><sub>(opt)</sub> `;` <a href="#Expression">Expression</a><sub>(opt)</sub> `)` <a href="#Statement">Statement</a></span>  
<span class="left-margin-40">`for` `(` <a href="#LeftHandSideExpression">LeftHandSideExpression</a> `in` <a href="#Expression">Expression</a> `)` <a href="#Statement">Statement</a></span>  
<span class="left-margin-40">`for` `(` `var` Identifier <a href="#Initializer">Initializer</a><sub>(opt)</sub> `in` <a href="#Expression">Expression</a> `)` <a href="#Statement">Statement</a></span>  

<a name="ContinueStatement"></a>
ContinueStatement:  
<span class="left-margin-40">`continue` `;`</span>  

<a name="BreakStatement"></a>
BreakStatement:  
<span class="left-margin-40">`break` `;`</span>  

<a name="ReturnStatement"></a>
ReturnStatement:  
<span class="left-margin-40">`return` <small>*[no LineTerminator here]*</small> <a href="#Expression">Expression</a><sub>(opt)</sub> `;`</span>  

<a name="WithStatement"></a>
WithStatement:  
<span class="left-margin-40">`with` `(` <a href="#Expression">Expression</a> `)` <a href="#Statement">Statement</a></span>  

<a name="FunctionDeclaration"></a>
FunctionDeclaration:  
<span class="left-margin-40">`function` Identifier `(` <a href="#FormalParameterList">FormalParameterList</a><sub>(opt)</sub> `)` <a href="#Block">Block</a></span>  

<a name="FormalParameterList"></a>
FormalParameterList:  
<span class="left-margin-40">Identifier</span>  
<span class="left-margin-40">FormalParameterList `,` Identifier</span>  

  [1]: http://www.ecma-international.org/publications/files/ECMA-ST-ARCH/ECMA-262,%201st%20edition,%20June%201997.pdf
