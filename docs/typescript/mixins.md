---
metaTitle: "TypeScript - Mixins"
description: "Example of Mixins"
---

# Mixins



## Example of Mixins


To create mixins, simply declare lightweight classes that can be used as "behaviours".

```ts
class Flies {
    fly() {
        alert('Is it a bird? Is it a plane?');
    }
}

class Climbs {
    climb() {
        alert('My spider-sense is tingling.');
    }
}

class Bulletproof {
    deflect() {
        alert('My wings are a shield of steel.');
    }
}

```

You can then apply these behaviours to a composition class:

```ts
class BeetleGuy implements Climbs, Bulletproof {
        climb: () => void;
        deflect: () => void;
}
applyMixins (BeetleGuy, [Climbs, Bulletproof]);

```

The `applyMixins` function is needed to do the work of composition.

```ts
function applyMixins(derivedCtor: any, baseCtors: any[]) {
    baseCtors.forEach(baseCtor => {
        Object.getOwnPropertyNames(baseCtor.prototype).forEach(name => {
             if (name !== 'constructor') {
                derivedCtor.prototype[name] = baseCtor.prototype[name];
            }
        });
    });
}

```



#### Syntax


- class BeetleGuy implements Climbs, Bulletproof { }
- applyMixins (BeetleGuy, [Climbs, Bulletproof]);



#### Parameters


|Parameter|Description
|---|---|---|---|---|---|---|---|---|---
|derivedCtor|The class that you want to use as the composition class
|baseCtors|An array of classes to be added to the composition class



#### Remarks


There are three rules to bear in mind with mixins:

- You use the `implements` keyword, not the `extends` keyword when you write your composition class
- You need to have a matching signature to keep the compiler quiet (but it doesn’t need any real implementation – it will get that from the mixin).
- You need to call `applyMixins` with the correct arguments.

