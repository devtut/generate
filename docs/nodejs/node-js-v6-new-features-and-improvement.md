---
metaTitle: "Node.js v6 New Features and Improvement"
description: "Default Function Parameters, Rest Parameters, Spread Operator, Arrow Functions, this in Arrow Function"
---

# Node.js v6 New Features and Improvement


With node 6 becoming the new LTS version of node. We can see an number of improvements to the language through the new ES6 standards introduces. We'll be walking through some of the new features introduced and examples of how to implement them.



## Default Function Parameters


```js
function addTwo(a, b = 2) {
    return a + b;
}

addTwo(3) // Returns the result 5

```

With the addition of default function parameters you can now make arguments optional and have them default to a value of your choice.



## Rest Parameters


```js
function argumentLength(...args) {
    return args.length;
}

argumentLength(5) // returns 1
argumentLength(5, 3) //returns 2
argumentLength(5, 3, 6) //returns 3

```

By prefacing the last argument of your function with `...` all arguments passed to the function are read as an array. In this example we get pass in multiple arguments and get the length of the array created from those arguments.



## Spread Operator


```js
function myFunction(x, y, z) { }
var args = [0, 1, 2];
myFunction(...args);

```

The spread syntax allows an expression to be expanded in places where multiple arguments (for function calls) or multiple elements (for array literals) or multiple variables are expected. Just like the rest parameters simply preface your array with `...`



## Arrow Functions


Arrow function is the new way of defining a function in ECMAScript 6.

```js
// traditional way of declaring and defining function
var sum = function(a,b)
{
    return a+b;
}

// Arrow Function
let sum = (a, b)=> a+b;

//Function defination using multiple lines 
let checkIfEven = (a) => {
    if( a % 2 == 0 )
        return true;
    else
        return false;
}

```



## "this" in Arrow Function


****this**** in function refers to instance object used to call that function but ****this**** in arrow function is equal to **this** of function in which arrow function is defined.

Let's understand using diagram[<img src="https://i.stack.imgur.com/iRsl1.jpg" alt="lexical scope of this in arrow function" />](https://i.stack.imgur.com/iRsl1.jpg)

Understanding using examples.

```js
var normalFn = function(){
   console.log(this) // refers to global/window object.
}

var arrowFn = () => console.log(this); // refers to window or global object as function is defined in scope of global/window object
    
var service = {

    constructorFn : function(){

        console.log(this); //  refers to service as service object used to call method.

        var nestedFn = function(){
            console.log(this); // refers window or global object because no instance object was used to call this method.
        }
        nestedFn();
    },
    
    arrowFn : function(){
        console.log(this); // refers to service as service object was used to call method.
        let fn = () => console.log(this); // refers to service object as arrow function defined in function which is called using instance object.
        fn();
    } 
}

// calling defined functions
constructorFn();
arrowFn();
service.constructorFn();
service.arrowFn();

```

In arrow function, **this** is lexical scope which is the scope of function where arrow function is defined.<br />
The first example is the traditional way of defining functions and hence, **this** refers to **global/window** object.<br />
In the second example **this** is used inside arrow function hence **this** refers to the scope where it is defined(which is windows or global object).
In the third example **this** is service object as service object is used to call the function.<br />
In fourth example, arrow function in defined and called from the function whose scope is **service**, hence it prints **service** object.

**Note: - global object is printed in Node.Js and windows object in browser.**

