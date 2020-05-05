---
metaTitle: "Node.js - Node.js Error Management"
description: "try...catch block, Creating Error object, Throwing Error"
---

# Node.js Error Management


We will learn how to create Error objects and how to throw & handle errors in Node.js

Future edits related to best practices in error handling.



## try...catch block


try...catch block is for handling exceptions, remember exception means the thrown error not the error.

```js
try {
    var a = 1;
    b++; //this will cause an error because be is undefined
    console.log(b); //this line will not be executed
} catch (error) {
    console.log(error); //here we handle the error caused in the try block
}

```

In the `try` block `b++` cause an error and that error passed to `catch` block which can be handled there or even can be thrown the same error in catch block or make little bit modification then throw. Let's see next example.

```js
try {
    var a = 1;
    b++;
    console.log(b);
} catch (error) {
    error.message = "b variable is undefined, so the undefined can't be incremented"
    throw error;
}

```

In the above example we modified the `message` property of `error` object and then throw the modified `error`.

You can through any error in your try block and handle it in the catch block:

```js
try {
    var a = 1;
    throw new Error("Some error message");
    console.log(a); //this line will not be executed;
} catch (error) {
    console.log(error); //will be the above thrown error 
}

```



## Creating Error object


**new Error(message)**

Creates new error object, where the value `message` is being set to `message` property of the created object. Usually the `message` arguments are being passed to Error constructor as a string. However if the `message` argument is object not a string then Error constructor calls `.toString()` method of the passed object and sets that value to `message` property of the created error object.

```js
var err = new Error("The error message");
console.log(err.message); //prints: The error message
console.log(err);
//output
//Error: The error message
//    at ... 

```

Each error object has stack trace. Stack trace contains the information of error message and shows where the error happened (the above output shows the error stack). Once error object is created the system captures the stack trace of the error on current line. To get the stack trace use stack property of any created error object. Below two lines are identical:

```js
console.log(err);
console.log(err.stack);

```



## Throwing Error


Throwing error means exception if any exception is not handled then the node server will crash.

The following line throws error:

```js
throw new Error("Some error occurred"); 

```

or

```js
var err = new Error("Some error occurred");
throw err;

```

or

```js
throw "Some error occurred";

```

The last example (throwing strings) is not good practice and is not recommended (always throw errors which are instances of Error object).

Note that if you `throw` an error in your, then the system will crash on that line (if there is no exception handlers), no any code will be executed after that line.

```js
var a = 5;
var err = new Error("Some error message");
throw err; //this will print the error stack and node server will stop
a++; //this line will never be executed
console.log(a); //and this one also

```

But in this example:

```js
var a = 5;
var err = new Error("Some error message");
console.log(err); //this will print the error stack
a++; 
console.log(a); //this line will be executed and will print 6

```

