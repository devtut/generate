---
metaTitle: "ECMAScript 2015 (ES6) with Node.js"
description: "const/let declarations, Arrow functions, Arrow Function Example, destructuring, flow, ES6 Class"
---

# ECMAScript 2015 (ES6) with Node.js



## const/let declarations


Unlike `var`, `const`/`let` are bound to lexical scope rather than function scope.

```js
{
  var x = 1 // will escape the scope
  let y = 2 // bound to lexical scope
  const z = 3 // bound to lexical scope, constant
}

console.log(x) // 1
console.log(y) // ReferenceError: y is not defined
console.log(z) // ReferenceError: z is not defined

```

[Run in RunKit](https://runkit.com/594bb4eaaac7e6001294132c/595433650a7efc0011ffcf09)



## Arrow functions


Arrow functions automatically bind to the 'this' lexical scope of the surrounding code.

```js
performSomething(result => {
  this.someVariable = result
})

```

vs

```js
performSomething(function(result) {
  this.someVariable = result
}.bind(this))

```



## Arrow Function Example


Let's consider this example, that outputs the squares of the numbers 3, 5, and 7:

```js
let nums = [3, 5, 7]
let squares = nums.map(function (n) {
  return n * n
})
console.log(squares)

```

[Run in RunKit](https://runkit.com/594bb4eaaac7e6001294132c/595611661cba570012815901)

The function passed to `.map` can also be written as arrow function by removing the `function` keyword and instead adding the arrow `=>`:

```js
let nums = [3, 5, 7]
let squares = nums.map((n) => {
  return n * n
})
console.log(squares)

```

[Run in RunKit](https://runkit.com/594bb4eaaac7e6001294132c/595613101cba570012815999)

However, this can be written even more concise. If the function body consists of only one statement and that statement computes the return value, the curly braces of wrapping the function body can be removed, as well as the `return` keyword.

```js
let nums = [3, 5, 7]
let squares = nums.map(n => n * n)
console.log(squares)

```

[Run in RunKit](https://runkit.com/594bb4eaaac7e6001294132c/59561361f9fe430012c7ab34)



## destructuring


```

   let [x,y, ...nums] = [0, 1, 2, 3, 4, 5, 6];
console.log(x, y, nums);

let {a, b, ...props} = {a:1, b:2, c:3, d:{e:4}}
console.log(a, b, props);

let dog = {name: 'fido', age: 3};
let {name:n, age} = dog;
console.log(n, age);

```



## flow


```js
/* @flow */

function product(a: number, b: number){
  return a * b;
}

const b = 3;
let c = [1,2,3,,{}];
let d = 3;

import request from 'request';

request('http://dev.markitondemand.com/MODApis/Api/v2/Quote/json?symbol=AAPL', (err, res, payload)=>{
  payload = JSON.parse(payload);
  let {LastPrice} = payload;
  console.log(LastPrice);
});

```



## ES6 Class


```js
class Mammel {
  constructor(legs){
    this.legs = legs;
  }
  eat(){
    console.log('eating...');
  }
  static count(){
    console.log('static count...');
  }
}

class Dog extends Mammel{
  constructor(name, legs){
    super(legs);
    this.name = name;
  }
  sleep(){
    super.eat();
    console.log('sleeping');
  }
}

let d = new Dog('fido', 4);
d.sleep();
d.eat();
console.log('d', d);

```

