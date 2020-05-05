---
metaTitle: "JavsScript - JavaScript Variables"
description: "Defining a Variable, Using a Variable, Types of Variables, Arrays and Objects"
---

# JavaScript Variables


Variables are what make up most of JavaScript. These variables make up things from numbers to objects, which are all over JavaScript to make one's life much easier.



## Defining a Variable


```js
var myVariable = "This is a variable!";

```

This is an example of defining variables. This variable is called a "string" because it has ASCII characters (`A-Z`, `0-9`, `!@#$`, etc.)



## Using a Variable


```js
var number1 = 5;
number1 = 3;

```

Here, we defined a number called "number1" which was equal to 5. However, on the second line, we changed the value to 3. To show the value of a variable, we log it to the console or use `window.alert()`:

```js
console.log(number1); // 3
window.alert(number1); // 3

```

To add, subtract, multiply, divide, etc., we do like so:

```js
number1 = number1 + 5; // 3 + 5 = 8
number1 = number1 - 6; // 8 - 6 = 2
var number2 = number1 * 10; // 2 (times) 10 = 20
var number3 = number2 / number1; // 20 (divided by) 2 = 10;

```

We can also add strings which will concatenate them, or put them together. For example:

```js
var myString = "I am a " + "string!"; // "I am a string!"

```



## Types of Variables


```js
var myInteger = 12; // 32-bit number (from -2,147,483,648 to 2,147,483,647)
var myLong = 9310141419482; // 64-bit number (from -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807)
var myFloat = 5.5; // 32-bit floating-point number (decimal)
var myDouble = 9310141419482.22; // 64-bit floating-point number

var myBoolean = true; // 1-bit true/false (0 or 1)
var myBoolean2 = false;

var myNotANumber = NaN;
var NaN_Example = 0/0; // NaN: Division by Zero is not possible

var notDefined; // undefined: we didn't define it to anything yet
window.alert(aRandomVariable); // undefined

var myNull = null; // null
// to be continued...

```



## Arrays and Objects


```js
var myArray = []; // empty array

```

An array is a set of variables. For example:

```js
var favoriteFruits = ["apple", "orange", "strawberry"];
var carsInParkingLot = ["Toyota", "Ferrari", "Lexus"];
var employees = ["Billy", "Bob", "Joe"];
var primeNumbers = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31];
var randomVariables = [2, "any type works", undefined, null, true, 2.51];

myArray = ["zero", "one", "two"];
window.alert(myArray[0]); // 0 is the first element of an array
                          // in this case, the value would be "zero"
myArray = ["John Doe", "Billy"];
elementNumber = 1;

window.alert(myArray[elementNumber]); // Billy

```

An object is a group of values; unlike arrays, we can do something better than them:

```js
myObject = {};
john = {firstname: "John", lastname: "Doe", fullname: "John Doe"};
billy = {
    firstname: "Billy",
    lastname: undefined
    fullname: "Billy"
};
window.alert(john.fullname); // John Doe
window.alert(billy.firstname); // Billy

```

Rather than making an array `["John Doe", "Billy"]` and calling `myArray[0]`, we can just call `john.fullname` and `billy.fullname`.



#### Syntax


- var {variable_name} [= {value}];



#### Parameters


|variable_name|**{Required}** The name of the variable: used when calling it.
|---|---|---|---|---|---|---|---|---|---
|=|**[Optional]** Assignment (defining the variable)
|value|**{Required when using Assignment}** The value of a variable **[default: undefined]**



#### Remarks


```js
"use strict";

```

### 

```js
'use strict';

```

**Strict Mode** makes JavaScript stricter to assure you the best habits. For example, assigning a variable:

```js
"use strict"; // or 'use strict';
var syntax101 = "var is used when assigning a variable.";
uhOh = "This is an error!";

```

`uhOh` is supposed to be defined using `var`. Strict Mode, being on, shows an error (in the Console, it doesn't care). Use this to generate good habits on defining variables.

You may use **Nested Arrays and Objects** some time. They are sometimes useful, and they're also fun to work with. Here is how they work:

### Nested Arrays

```js
var myArray = [ "The following is an array", ["I'm an array"] ];

```

### 

```js
console.log(myArray[1]); // (1) ["I'm an array"]
console.log(myArray[1][0]); // "I'm an array"

```

### 

```js
var myGraph = [ [0, 0], [5, 10], [3, 12] ]; // useful nested array

```

### 

```js
console.log(myGraph[0]); // [0, 0]
console.log(myGraph[1][1]); // 10

```

### Nested Objects

```js
var myObject = {
    firstObject: {
        myVariable: "This is the first object"
    }
    secondObject: {
        myVariable: "This is the second object"
    }
}

```

### 

```js
console.log(myObject.firstObject.myVariable); // This is the first object.
console.log(myObject.secondObject); // myVariable: "This is the second object"

```

### 

```js
var people = {
    john: {
        name: {
            first: "John",
            last: "Doe",
            full: "John Doe"
        },
        knownFor: "placeholder names"
    },
    bill: {
        name: {
            first: "Bill",
            last: "Gates",
            full: "Bill Gates"
        },
        knownFor: "wealth"
    }
}

```

### 

```js
console.log(people.john.name.first); // John
console.log(people.john.name.full); // John Doe
console.log(people.bill.knownFor); // wealth
console.log(people.bill.name.last); // Gates
console.log(people.bill.name.full); // Bill Gates

```

