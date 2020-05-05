---
metaTitle: "JavsScript - Declarations and Assignments"
description: "Modifying constants, Declaring and initializing constants, Declaration, Data Types, Undefined, Assignment, Mathematic operations and assignment, Reassigning constants"
---

# Declarations and Assignments



## Modifying constants


Declaring a variable `const` only prevents its value from being **replaced** by a new value. `const` does not put any restrictions on the internal state of an object. The following example shows that a value of a property of a `const` object can be changed, and even new properties can be added, because the object that is assigned to `person` is modified, but not **replaced**.

```js
const person = { 
    name: "John" 
};
console.log('The name of the person is', person.name);

person.name = "Steve";
console.log('The name of the person is', person.name);

person.surname = "Fox";
console.log('The name of the person is', person.name, 'and the surname is', person.surname);

```

**Result:**

```js
The name of the person is John
The name of the person is Steve
The name of the person is Steve and the surname is Fox

```

In this example we've created constant object called `person` and we've reassigned `person.name` property and created new `person.surname` property.



## Declaring and initializing constants


You can initialize a constant by using the `const` keyword.

```js
const foo = 100;
const bar = false;
const person = { name: "John" };
const fun = function () = { /* ... */ };
const arrowFun = () => /* ... */ ;

```

**Important**<br />
You must declare and initialize a constant in the same statement.



## Declaration


There are four principle ways to declare a variable in JavaScript: using the `var`, `let` or `const` keywords, or without a keyword at all ("bare" declaration). The method used determines the resulting [scope](http://stackoverflow.com/documentation/javascript/480/scope#t=201608021853407675183) of the variable, or reassignability in the case of `const`.

- The `var` keyword creates a function-scope variable.
- The `let` keyword creates a block-scope variable.
- The `const` keyword creates a block-scope variable that cannot be reassigned.
- A bare declaration creates a global variable.

```js
var a = 'foo';    // Function-scope
let b = 'foo';    // Block-scope
const c = 'foo';  // Block-scope & immutable reference

```

Keep in mind that you can't declare constants without initializing them at the same time.

```js
const foo; // "Uncaught SyntaxError: Missing initializer in const declaration"

```

(An example of keyword-less variable declaration is not included above for technical reasons. Continue reading to see an example.)



## Data Types


JavaScript variables can hold many data types: numbers, strings, arrays, objects and more:

```js
// Number
var length = 16;

// String
var message = "Hello, World!"; 

// Array
var carNames = ['Chevrolet', 'Nissan', 'BMW']; 

// Object
var person = {
    firstName: "John",
    lastName: "Doe"
}; 

```

JavaScript has dynamic types. This means that the same variable can be used as different types:

```js
var a;              // a is undefined
var a = 5;          // a is a Number
var a = "John";     // a is a String

```



## Undefined


Declared variable without a value will have the value `undefined`

```js
var a;

console.log(a); // logs: undefined

```

Trying to retrieve the value of undeclared variables results in a ReferenceError. However, both the type of undeclared and unitialized variables is "undefined":

```js
var a;
console.log(typeof a === "undefined"); // logs: true
console.log(typeof variableDoesNotExist === "undefined"); // logs: true

```



## Assignment


To assign a value to a previously declared variable, use the assignment operator, `=`:

```js
a = 6;
b = "Foo";

```

As an alternative to independent declaration and assignment, it is possible to perform both steps in one statement:

```js
var a = 6;
let b = "Foo";

```

It is in this syntax that global variables may be declared without a keyword; if one were to declare a bare variable without an assignment immediately afterword, the interpreter would not be able to differentiate global declarations `a;` from references to variables `a;`.

```js
c = 5;
c = "Now the value is a String.";
myNewGlobal;    // ReferenceError

```

Note, however, that the above syntax is generally discouraged and is not strict-mode compliant. This is to avoid the scenario in which a programmer inadvertently drops a `let` or `var` keyword from their statement, accidentally creating a variable in the global namespace without realizing it. This can pollute the global namespace and conflict with libraries and the proper functioning of a script. Therefore global variables should be declared and initialized using the `var` keyword in the context of the window object, instead, so that the intent is explicitly stated.

Additionally, variables may be declared several at a time by separating each declaration (and optional value assignment) with a comma. Using this syntax, the var and let keywords need only be used once at the beginning of each statement.

```js
globalA = "1", globalB = "2";
let x, y = 5;
var person = 'John Doe',
    foo,
    age = 14,
    date = new Date(); 

```

Notice in the preceding code snippet that the order in which declaration and assignment expressions occur (`var a, b, c = 2, d;`) does not matter. You may freely intermix the two.

[Function declaration](http://stackoverflow.com/documentation/javascript/186/functions#t=201607220124257456041) effectively creates variables, as well.



## Mathematic operations and assignment


### Increment by

```js
var a = 9,  
b = 3;  
b += a;  

```

`b` will now be 12

This is functionally the same as

```js
b = b + a; 

```

### Decrement by

```js
var a = 9,  
b = 3;  
b -= a;  

```

`b` will now be 6

This is functionally the same as

```js
b = b - a;  

```

### Multiply by

```js
var a = 5,  
b = 3;  
b *= a;  

```

`b` will now be 15

This is functionally the same as

```js
b = b * a;  

```

### Divide by

```js
var a = 3,  
b = 15;  
b /= a;  

```

`b` will now be 5

This is functionally the same as

```js
b = b / a;  

```

### Raised to the power of

```js
var a = 3,  
b = 15;  
b **= a;  

```

`b` will now be 3375

This is functionally the same as

```js
b = b ** a;  

```



## Reassigning constants


You can't reassign constants.

```js
const foo = "bar";
foo = "hello";

```

**Prints:**

```js
Uncaught TypeError: Assignment to constant.

```



#### Syntax


- var foo [= value [, foo2 [, foo3 ... [, fooN]]]];
- let bar [= value [, bar2 [, foo3 ... [, barN]]]];
- const baz = value [, baz2 = value2 [, ... [, bazN = valueN]]];



#### Remarks


See also:

- [Reserved Keywords](http://stackoverflow.com/documentation/javascript/1853/reserved-keywords)
- [Scope](http://stackoverflow.com/documentation/javascript/480/scope#t=201608021853407675183)

