---
metaTitle: "TypeScript - Publish TypeScript definition files"
description: "Include definition file with library on npm"
---

# Publish TypeScript definition files



## Include definition file with library on npm


Add typings to your package.json

```js
{
...
"typings": "path/file.d.ts"
...
}

```

Now when ever that library is imported typescript will load the typings file

