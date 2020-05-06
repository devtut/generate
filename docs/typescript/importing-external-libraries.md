---
metaTitle: "TypeScript - Importing external libraries"
description: "Finding definition files, Importing a module from npm, Using global external libraries without typings, Finding definition files with typescript 2.x"
---

# Importing external libraries



## Finding definition files


for typescript 2.x:

definitions from [DefinitelyTyped](https://github.com/DefinitelyTyped/DefinitelyTyped) are available via [@types npm](https://www.npmjs.com/%7Etypes) package

```js
npm i --save lodash
npm i --save-dev @types/lodash

```

but in case if you want use types from other repos then can be used old way:

for typescript 1.x:

[Typings](https://github.com/typings/typings) is an npm package that can automatically install type definition files into a local project. I recommend that you read the [quickstart](https://github.com/typings/typings#quick-start).

```js
npm install -global typings

```

Now we have access to the typings cli.

<li>
The first step is to search for the package used by the project

```js
typings search lodash
NAME              SOURCE HOMEPAGE                                        DESCRIPTION VERSIONS UPDATED
lodash            dt     http://lodash.com/                                          2        2016-07-20T00:13:09.000Z
lodash            global                                                             1        2016-07-01T20:51:07.000Z
lodash            npm    https://www.npmjs.com/package/lodash                        1        2016-07-01T20:51:07.000Z

```


</li>
<li>
Then decide which source you should install from. I use dt which stands for [DefinitelyTyped](https://github.com/DefinitelyTyped/DefinitelyTyped) a GitHub repo where the community can edit typings, it's also normally the most recently updated.
</li>
<li>
Install the typings files

```js
 typings install dt~lodash --global --save

```


</li>

Let's break down the last command. We are installing the DefinitelyTyped version of lodash as a global typings file in our project and saving it as a dependency in the `typings.json`. Now wherever we import lodash, typescript will load the lodash typings file.

<li>
If we want to install typings that will be used for development environment only, we can supply the `--save-dev` flag:

```js
 typings install chai --save-dev

```


</li>



## Importing a module from npm


If you have a type definition file (d.ts) for the module, you can use an `import` statement.

```js
import _ = require('lodash');

```

If you don't have a definition file for the module, TypeScript will throw an error on compilation because it cannot find the module you are trying to import.

In this case, you can import the module with the normal runtime `require` function. This returns it as the `any` type, however.

```js
// The _ variable is of type any, so TypeScript will not perform any type checking.
const _: any = require('lodash');

```

As of TypeScript 2.0, you can also use a **shorthand ambient module declaration** in order to tell TypeScript that a module exists when you don't have a type definition file for the module. TypeScript won't be able to provide any meaningful typechecking in this case though.

```js
declare module "lodash";

// you can now import from lodash in any way you wish:
import { flatten } from "lodash";
import * as _ from "lodash";

```

As of TypeScript 2.1, the rules have been relaxed even further. Now, as long as a module exists in your `node_modules` directory, TypeScript will allow you to import it, even with no module declaration anywhere. (Note that if using the `--noImplicitAny` compiler option, the below will still generate a warning.)

```js
// Will work if `node_modules/someModule/index.js` exists, or if `node_modules/someModule/package.json` has a valid "main" entry point
import { foo } from "someModule";

```



## Using global external libraries without typings


Although modules are ideal, if the library you are using is referenced by a global variable (like $ or _), because it was loaded by a `script` tag, you can create an ambient declaration in order to refer to it:

`declare const _: any;`



## Finding definition files with typescript 2.x


With the 2.x versions of typescript, typings are now available from the [npm @types repository](https://www.npmjs.com/%7Etypes). These are automatically resolved by the typescript compiler and are much simpler to use.

To install a type definition you simply install it as a dev dependency in your projects package.json

e.g.

```js
npm i -S lodash
npm i -D @types/lodash

```

after install you simply use the module as before

```js
import * as _ from 'lodash'

```



#### Syntax


- `import {component} from 'libName'; // Will import the class "component"`
- `import {component as c} from 'libName'; // Will import the class "component" into a "c" object`
- `import component from 'libname'; // Will import the default export from libName`
- `import * as lib from 'libName'; // Will import everything from libName into a "lib" object`
- `import lib = require('libName'); // Will import everything from libName into a "lib" object`
- `const lib: any = require('libName'); // Will import everything from libName into a "lib" object`
- `import 'libName'; // Will import libName module for its side effects only`



#### Remarks


It might seem that the syntax

```js
import * as lib from 'libName';

```

and

```js
import lib = require('libName');

```

are the same thing, but they are not!

Let us consider that we want to import a class **Person** exported with TypeScript-specific `export =`  syntax :

```js
class Person {
...
}
export = Person;

```

In this case it is not possible to import it with es6 syntax (we would get an error at compile time), TypeScript-specific `import =` syntax must be used.

```js
import * as Person from 'Person';  //compile error
import Person = require('Person');  //OK

```

The converse is true: classic modules can be imported with the second syntax, so, in a way, the last syntax is more powerful since it is able to import all exports.

For more information see the [official documentation](https://www.typescriptlang.org/docs/handbook/modules.html).

