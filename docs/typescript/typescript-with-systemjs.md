---
metaTitle: "TypeScript - TypeScript with SystemJS"
description: "Hello World in the browser with SystemJS"
---

# TypeScript with SystemJS



## Hello World in the browser with SystemJS


**Install systemjs and plugin-typescript**

```ts
npm install systemjs
npm install plugin-typescript

```

NOTE: this will install typescript 2.0.0 compiler which is not released yet.

For TypeScript 1.8 you have to use plugin-typescript 4.0.16

**Create `hello.ts` file**

```ts
export function greeter(person: String) {
    return 'Hello, ' + person;
}

```

**Create `hello.html` file**

```ts
<!doctype html>
<html>
<head>
    <title>Hello World in TypeScript</title>
    <script src="node_modules/systemjs/dist/system.src.js"></script>

    <script src="config.js"></script>

    <script>
        window.addEventListener('load', function() {
            System.import('./hello.ts').then(function(hello) {
                document.body.innerHTML = hello.greeter('World');
            });
        });
    </script>


</head>
<body>
</body>
</html>

```

**Create `config.js` - SystemJS configuration file**

```ts
System.config({
    packages: {
        "plugin-typescript": {
            "main": "plugin.js"
        },
        "typescript": {
            "main": "lib/typescript.js",
            "meta": {
                "lib/typescript.js": {
                    "exports": "ts"
                }
            }
        }
    },
    map: {
        "plugin-typescript": "node_modules/plugin-typescript/lib/",
        /* NOTE: this is for npm 3 (node 6) */
        /* for npm 2, typescript path will be */
        /* node_modules/plugin-typescript/node_modules/typescript */
        "typescript": "node_modules/typescript/"
    },
    transpiler: "plugin-typescript",
    meta: {
        "./hello.ts": {
            format: "esm",
            loader: "plugin-typescript"
        }
    },
    typescriptOptions: {
        typeCheck: 'strict'
    }
});

```

NOTE: if you don't want type checking, remove `loader: "plugin-typescript"` and `typescriptOptions` from `config.js`. Also note that it will never check javascript code, in particular code in the `<script>` tag in html example.

**Test it**

```ts
npm install live-server
./node_modules/.bin/live-server --open=hello.html

```

**Build it for production**

```ts
npm install systemjs-builder

```

Create `build.js` file:

```ts
var Builder = require('systemjs-builder');
var builder = new Builder();
builder.loadConfig('./config.js').then(function() {
    builder.bundle('./hello.ts', './hello.js', {minify: true});
});

```

build hello.js from hello.ts

```ts
node build.js

```

**Use it in production**

Just load hello.js with a script tag before first use

`hello-production.html` file:

```ts
<!doctype html>
<html>
<head>
    <title>Hello World in TypeScript</title>
    <script src="node_modules/systemjs/dist/system.src.js"></script>

    <script src="config.js"></script>
    <script src="hello.js"></script>
    <script>
        window.addEventListener('load', function() {
            System.import('./hello.ts').then(function(hello) {
                document.body.innerHTML = hello.greeter('World');
            });
        });
    </script>


</head>
<body>
</body>
</html>

```

