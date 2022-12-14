---
metaTitle: "JavaScript - Proxy"
description: "Very simple proxy (using the set trap), Proxying property lookup"
---

# Proxy


A Proxy in JavaScript can be used to modify fundamental operations on objects. Proxies were introduced in ES6. A Proxy on an object is itself an object, that has **traps**. Traps may be triggered when operations are performed on the Proxy. This includes property lookup, function calling, modifying properties, adding properties, et cetera. When no applicable trap is defined, the operation is performed on the proxied object as if there was no Proxy.



## Very simple proxy (using the set trap)


This proxy simply appends the string `" went through proxy"` to every string property set on the target `object`.

```js
let object  = {};

let handler = {
    set(target, prop, value){ // Note that ES6 object syntax is used
        if('string' === typeof value){
            target[prop] = value + " went through proxy";
        }
    }
};

let proxied = new Proxy(object, handler);

proxied.example = "ExampleValue";

console.log(object); 
// logs: { example: "ExampleValue went trough proxy" }
// you could also access the object via proxied.target

```



## Proxying property lookup


To influence property lookup, the `get` handler must be used.

In this example, we modify property lookup so that not only the value, but also the type of that value is returned. We use [Reflect](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Reflect) to ease this.

```js
let handler = {
    get(target, property) {
        if (!Reflect.has(target, property)) {
            return {
                value: undefined,
                type: 'undefined'
            };
        }
        let value = Reflect.get(target, property);
        return {
            value: value,
            type: typeof value
        };
    }
};

let proxied = new Proxy({foo: 'bar'}, handler);
console.log(proxied.foo); // logs `Object {value: "bar", type: "string"}`

```



#### Syntax


- `let proxied = new Proxy(target, handler);`



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|target|The target object, actions on this object (getting, setting, etc...) will be routed trough the handler
|handler|An object that can define "traps" for intercepting actions on the target object (getting, setting, etc...)



#### Remarks


A full list of available "traps" can be found on [MDN - Proxy - "Methods of the handler object"](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Proxy#Methods_of_the_handler_object).

