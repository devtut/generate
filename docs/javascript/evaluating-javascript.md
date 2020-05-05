---
metaTitle: "JavsScript - Evaluating JavaScript"
description: "Introduction, Evaluation and Math, Evaluate a string of JavaScript statements"
---

# Evaluating JavaScript


In JavaScript, the `eval` function evaluates a string as if it were JavaScript code. The return value is the result of the evaluated string, e.g. `eval('2 + 2')` returns `4`.

`eval` is available in the global scope. The lexical scope of the evaluation is the local scope unless invoked indirectly (e.g. `var geval = eval; geval(s);`).

**The use of `eval` is strongly discouraged.** See the Remarks section for details.



## Introduction


You can always run JavaScript from inside itself, although this is **strongly discouraged** due to the security vulnerabilities it presents (see Remarks for details).

To run JavaScript from inside JavaScript, simply use the below function:

```js
eval("var a = 'Hello, World!'");

```



## Evaluation and Math


You can set a variable to something with the `eval()` function by using something similar to the below code:

```js
var x = 10;
var y = 20;
var a = eval("x * y") + "<br>";
var b = eval("2 + 2") + "<br>";
var c = eval("x + 17") + "<br>";

var res = a + b + c;

```

The result, stored in the variable `res`, will be:

> 
200<br>4<br>27


**The use of `eval` is strongly discouraged.** See the Remarks section for details.



## Evaluate a string of JavaScript statements


```js
var x = 5;
var str = "if (x == 5) {console.log('z is 42'); z = 42;} else z = 0; ";

console.log("z is ", eval(str));

```

**The use of `eval` is strongly discouraged.** See the Remarks section for details.



#### Syntax


- eval(string);



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|string|The JavaScript to be evaluated.



#### Remarks


**The use of `eval` is strongly discouraged; in many scenarios it presents a security vulnerability.**

> 
eval() is a dangerous function, which executes the code it's passed with the privileges of the caller. If you run eval() with a string that could be affected by a malicious party, you may end up running malicious code on the user's machine with the permissions of your webpage / extension. More importantly, third party code can see the scope in which eval() was invoked, which can lead to possible attacks in ways to which the similar Function is not susceptible.
[MDN JavaScript Reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/eval#Don%27t_use_eval_needlessly!)


Additionally:

- [Exploiting JavaScript's eval() method](http://stackoverflow.com/questions/18189496/exploiting-javascripts-eval-method)
- [What are the security issues with “eval()” in JavaScript?](http://security.stackexchange.com/questions/94017/what-are-the-security-issues-with-eval-in-javascript)

