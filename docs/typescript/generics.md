---
metaTitle: "TypeScript - Generics"
description: "Generic Interfaces, Generic Class, Type parameters as constraints, Generics Constraints, Generic Functions, Using generic Classes and Functions:"
---

# Generics



## Generic Interfaces


### Declaring a generic interface

```js
interface IResult<T> {
    wasSuccessfull: boolean;
    error: T;
}

var result: IResult<string> = ....
var error: string = result.error;

```

### Generic interface with multiple type parameters

```js
interface IRunnable<T, U> {
    run(input: T): U;
}

var runnable: IRunnable<string, number> = ...
var input: string;
var result: number = runnable.run(input);

```

### Implementing a generic interface

```js
interface IResult<T>{
    wasSuccessfull: boolean;
    error: T;

    clone(): IResult<T>;
}

```

Implement it with generic class:

```js
class Result<T> implements IResult<T> {
    constructor(public result: boolean, public error: T) {
    }

    public clone(): IResult<T> {
        return new Result<T>(this.result, this.error);
    }
}

```

Implement it with non generic class:

```js
class StringResult implements IResult<string> {
    constructor(public result: boolean, public error: string) {
    }

    public clone(): IResult<string> {
        return new StringResult(this.result, this.error);
    }
}

```



## Generic Class


```js
class Result<T> {
    constructor(public wasSuccessful: boolean, public error: T) {
    }

    public clone(): Result<T> {
       ...
    }
}

let r1 = new Result(false, 'error: 42');  // Compiler infers T to string
let r2 = new Result(false, 42);           // Compiler infers T to number
let r3 = new Result<string>(true, null);  // Explicitly set T to string
let r4 = new Result<string>(true, 4);     // Compilation error because 4 is not a string

```



## Type parameters as constraints


With TypeScript 1.8 it becomes possible for a type parameter constraint to reference type parameters from the same type parameter list. Previously this was an error.

```

function assign<T extends U, U>(target: T, source: U): T {
    for (let id in source) {
        target[id] = source[id];
    }
    return target;
}

let x = { a: 1, b: 2, c: 3, d: 4 };
assign(x, { b: 10, d: 20 });
assign(x, { e: 0 });  // Error

```



## Generics Constraints


Simple constraint:

```js
interface IRunnable {
    run(): void;
}

interface IRunner<T extends IRunnable> {
    runSafe(runnable: T): void;
}

```

More complex constraint:

```js
interface IRunnble<U> {
    run(): U;
}

interface IRunner<T extends IRunnable<U>, U> {
    runSafe(runnable: T): U;
}

```

Even more complex:

```js
interface IRunnble<V> {
    run(parameter: U): V;
}

interface IRunner<T extends IRunnable<U, V>, U, V> {
    runSafe(runnable: T, parameter: U): V;
}

```

Inline type constraints:

```js
interface IRunnable<T extends { run(): void }> {
    runSafe(runnable: T): void;
}

```



## Generic Functions


In interfaces:

```js
interface IRunner {
    runSafe<T extends IRunnable>(runnable: T): void;
}

```

In classes:

```js
class Runner implements IRunner {

    public runSafe<T extends IRunnable>(runnable: T): void {
        try {
            runnable.run();
        } catch(e) {
        }
    }

}

```

Simple functions:

```js
function runSafe<T extends IRunnable>(runnable: T): void {
    try {
        runnable.run();
    } catch(e) {
    }
}

```



## Using generic Classes and Functions:


Create generic class instance:

```js
var stringRunnable = new Runnable<string>();

```

Run generic function:

```js
function runSafe<T extends Runnable<U>, U>(runnable: T);

// Specify the generic types:
runSafe<Runnable<string>, string>(stringRunnable);

// Let typescript figure the generic types by himself:
runSafe(stringRunnable);

```



#### Syntax


- The generic types declared within the triangle brackets: `<T>`
- Constrainting the generic types is done with the extends keyword: `<T extends Car>`



#### Remarks


The generic parameters are not available at runtime, they are just for the compile time.
This means you can't do something like this:

```js
class Executor<T, U> {
    public execute(executable: T): void {
        if (T instanceof Executable1) {    // Compilation error
            ...
        } else if (U instanceof Executable2){    // Compilation error
            ...
        }
    }
}

```

However, class information is still preserved, so you can still test for the type of a variable as you have always been able to:

```js
class Executor<T, U> {
    public execute(executable: T): void {
        if (executable instanceof Executable1) {
            ...
        } else if (executable instanceof Executable2){
            ...
        } // But in this method, since there is no parameter of type `U` it is non-sensical to ask about U's "type"
    }
}

```

