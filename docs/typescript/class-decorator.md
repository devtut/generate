---
metaTitle: "TypeScript - Class Decorator"
description: "Generating metadata using a class decorator, Passing arguments to a class decorator, Basic class decorator"
---

# Class Decorator



## Generating metadata using a class decorator


This time we are going to declare a class decorator that will add some metadata to a class when we applied to it:

```js
function addMetadata(target: any) {
    
    // Add some metadata
    target.__customMetadata = {
        someKey: "someValue"
    };
    
    // Return target
    return target;

}

```

We can then apply the class decorator:

```js
@addMetadata
class Person {
    private _name: string;
    public constructor(name: string) {
        this._name = name;
    }
    public greet() {
        return this._name;
    }
}

function getMetadataFromClass(target: any) {
    return target.__customMetadata;
}

console.log(getMetadataFromClass(Person));

```

The decorator is applied when the class is declared not when we create instances of the class. This means that the metadata is shared across all the instances of a class:

```js
function getMetadataFromInstance(target: any) {
    return target.constructor.__customMetadata;
}

let person1 = new Person("John");
let person2 = new Person("Lisa");

console.log(getMetadataFromInstance(person1));
console.log(getMetadataFromInstance(person2));

```



## Passing arguments to a class decorator


We can wrap a class decorator with another function to allow customization:

```js
function addMetadata(metadata: any) {
    return function log(target: any) {
    
        // Add metadata
        target.__customMetadata = metadata;
        
        // Return target
        return target;
    
    }
}

```

The `addMetadata` takes some arguments used as configuration and then returns an unnamed function which is the actual decorator. In the decorator we can access the arguments because there is a closure in place.

We can then invoke the decorator passing some configuration values:

```js
@addMetadata({ guid: "417c6ec7-ec05-4954-a3c6-73a0d7f9f5bf" })
class Person {
    private _name: string;
    public constructor(name: string) {
        this._name = name;
    }
    public greet() {
        return this._name;
    }
}

```

We can use the following function to access the generated metadata:

```js
function getMetadataFromClass(target: any) {
    return target.__customMetadata;
}

console.log(getMetadataFromInstance(Person));

```

If everything went right the console should display:

```js
{ guid: "417c6ec7-ec05-4954-a3c6-73a0d7f9f5bf" } 

```



## Basic class decorator


A class decorator is just a function that takes the class as its only argument and returns it after doing something with it:

```js
function log<T>(target: T) {
    
    // Do something with target
    console.log(target);
    
    // Return target
    return target;

}

```

We can then apply the class decorator to a class:

```js
@log
class Person {
    private _name: string;
    public constructor(name: string) {
        this._name = name;
    }
    public greet() {
        return this._name;
    }
}

```



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|target|The class being decorated

