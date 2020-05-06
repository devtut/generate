---
metaTitle: "TypeScript - Functions"
description: "Optional and Default Parameters, Types of Functions, Function as a parameter, Functions with Union Types"
---

# Functions



## Optional and Default Parameters


**Optional Parameters**

In TypeScript, every parameter is assumed to be required by the function. You can add a **?** at the end of a parameter name to set it as optional.

For example, the `lastName` parameter of this function is optional:

```js
function buildName(firstName: string, lastName?: string) {
    // ...
}

```

Optional parameters must come after all non-optional parameters:

```js
function buildName(firstName?: string, lastName: string) // Invalid

```

**Default Parameters**

If the user passes `undefined` or doesn't specify an argument, the default value will be assigned. These are called **default-initialized** parameters.

For example, "Smith" is the default value for the `lastName` parameter.

```js
function buildName(firstName: string, lastName = "Smith") {
    // ...
}
buildName('foo', 'bar');      // firstName == 'foo', lastName == 'bar'
buildName('foo');             // firstName == 'foo', lastName == 'Smith'
buildName('foo', undefined);  // firstName == 'foo', lastName == 'Smith'

```



## Types of Functions


**Named functions**

```js
function multiply(a, b) {
    return a * b;
}

```

**Anonymous functions**

```js
let multiply = function(a, b) { return a * b; };

```

**Lambda / arrow functions**

```js
let multiply = (a, b) => { return a * b; };

```



## Function as a parameter


Suppose we want to receive a function as a parameter, we can do it like this:

```js
function foo(otherFunc: Function): void {
    ...
}

```

If we want to receive a constructor as a parameter:

```js
function foo(constructorFunc: { new() }) {
    new constructorFunc();
}

function foo(constructorWithParamsFunc: { new(num: number) }) {
    new constructorWithParamsFunc(1);
}

```

Or to make it easier to read we can define an interface describing the constructor:

```js
interface IConstructor {
    new();
}

function foo(contructorFunc: IConstructor) { 
    new constructorFunc();
}

```

Or with parameters:

```js
interface INumberConstructor {
    new(num: number);
}

function foo(contructorFunc: INumberConstructor) {
    new contructorFunc(1);
}

```

Even with generics:

```js
interface ITConstructor<T, U> {
    new(item: T): U;
}

function foo<T, U>(contructorFunc: ITConstructor<T, U>, item: T): U {
    return new contructorFunc(item);
}

```

If we want to receive a simple function and not a constructor it's almost the same:

```js
function foo(func: { (): void }) {
    func();
}

function foo(constructorWithParamsFunc: { (num: number): void }) {
    new constructorWithParamsFunc(1);
}

```

Or to make it easier to read we can define an interface describing the function:

```js
interface IFunction {
    (): void;
}

function foo(func: IFunction ) { 
    func();
}

```

Or with parameters:

```js
interface INumberFunction {
    (num: number): string;
}

function foo(func: INumberFunction ) {
    func(1);
}

```

Even with generics:

```js
interface ITFunc<T, U> {
    (item: T): U;
}

function foo<T, U>(contructorFunc: ITFunc<T, U>, item: T): U {
    return func(item);
}

```



## Functions with Union Types


A TypeScript function can take in parameters of multiple, predefined types using union types.

```js
function whatTime(hour:number|string, minute:number|string):string{
    return hour+':'+minute;
}

whatTime(1,30)         //'1:30'
whatTime('1',30)       //'1:30'
whatTime(1,'30')       //'1:30'
whatTime('1','30')     //'1:30'

```

Typescript treats these parameters as a single type that is a union of the other types, so your function must be able to handle parameters of any type that is in the union.

```js
function addTen(start:number|string):number{
    if(typeof number === 'string'){
        return parseInt(number)+10;
    }else{
        else return number+10;
    }
}

```



#### Remarks


Typescript documentation link for [Functions](https://github.com/Microsoft/TypeScript-Handbook/blob/master/pages/Functions.md)

