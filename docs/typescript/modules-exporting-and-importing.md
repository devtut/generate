---
metaTitle: "TypeScript - Modules - exporting and importing"
description: "Re-export, Hello world module, Exporting/Importing declarations"
---

# Modules - exporting and importing




## Re-export


Typescript allow to re-export declarations.

```js
//Operator.ts
interface Operator {
    eval(a: number, b: number): number;
}
export default Operator;

```

```js
//Add.ts
import Operator from "./Operator";
export class Add implements Operator {
    eval(a: number, b: number): number {
        return a + b;
    }
}

```

```js
//Mul.ts
import Operator from "./Operator";
export class Mul implements Operator {
    eval(a: number, b: number): number {
        return a * b;
    }
}

```

You can bundle all operations in single library

```js
//Operators.ts
import {Add} from "./Add";
import {Mul} from "./Mul";

export {Add, Mul};

```

**Named declarations** can be re-exported using shorter syntax

```js
//NamedOperators.ts
export {Add} from "./Add";
export {Mul} from "./Mul";

```

**Default exports** can also be exported, but no short syntax is available. Remember, only one default export per module is possible.

```js
//Calculator.ts
export {Add} from "./Add";
export {Mul} from "./Mul";
import Operator from "./Operator";

export default Operator;

```

Possible is re-export of **bundled import**

```js
//RepackedCalculator.ts
export * from "./Operators";

```

When re-exporting bundle, declarations may be overridden when declared explicitly.

```js
//FixedCalculator.ts
export * from "./Calculator"
import Operator from "./Calculator";
export class Add implements Operator {
    eval(a: number, b: number): number {
        return 42;
    }
}

```

Usage example

```js
//run.ts
import {Add, Mul} from "./FixedCalculator";

const add = new Add();
const mul = new Mul();

console.log(add.eval(1, 1)); // 42
console.log(mul.eval(3, 4)); // 12

```



## Hello world module


**Load using directory index**

If directory contains file named `index.ts` it can be loaded using only directory name (for `index.ts` filename is optional).

**Example usage of defined modules**



## Exporting/Importing declarations


Any declaration (variable, const, function, class, etc.) can be exported from module to be imported in other module.

Typescript offer two export types: named and default.

**Named export**

When importing named exports, you can specify which elements you want to import.

**Default export**

Each module can have one default export

which can be imported using

**Bundled import**

Typescript offers method to import whole module into variable

