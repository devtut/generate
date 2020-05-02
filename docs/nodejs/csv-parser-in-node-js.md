---
metaTitle: "csv parser in node js"
description: "Using FS to read in a CSV"
---

# csv parser in node js


Reading data in from a csv can be handled in many ways. One solution is to read the `csv` file into an array. From there you can do work on the array.



## Using FS to read in a CSV


fs is the [File System API](https://nodejs.org/api/fs.h) in node. We can use the method readFile on our fs variable, pass it a `data.csv` file, format and function that reads and splits the `csv` for further processing.

**This assumes you have a file named `data.csv` in the same folder.**

```js
'use strict'

const fs = require('fs');

fs.readFile('data.csv', 'utf8', function (err, data) {
  var dataArray = data.split(/\r?\n/);
  console.log(dataArray);
});

```

You can now use the array like any other to do work on it.

