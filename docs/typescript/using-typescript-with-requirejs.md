---
metaTitle: "TypeScript - Using Typescript with RequireJS"
description: "HTML example using requireJS CDN to include an already compiled TypeScript file., tsconfig.json example to compile to view folder using requireJS import style."
---

# Using Typescript with RequireJS


RequireJS is a JavaScript file and module loader. It is optimized for in-browser use, but it can be used in other JavaScript environments, like Rhino and Node. Using a modular script loader like RequireJS will improve the speed and quality of your code.

Using TypeScript with RequireJS requires configuration of tsconfig.json, and including an snippet in any HTML file. Compiler will traduce imports from the syntax of TypeScript to RequireJS' format.



## HTML example using requireJS CDN to include an already compiled TypeScript file.


```js
<body onload="__init();">
    ...
    <script src="http://requirejs.org/docs/release/2.3.2/comments/require.js"></script>
    <script>
      function __init() {
        require(["view/index.js"]);
      }
    </script>
</body>

```



## tsconfig.json example to compile to view folder using requireJS import style.


```js
{
  "module": "amd",    // Using AMD module code generator which works with requireJS
  "rootDir": "./src", // Change this to your source folder
  "outDir": "./view",
    ...      
}

```

