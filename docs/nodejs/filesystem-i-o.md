---
metaTitle: "Filesystem I/O"
description: "Asynchronously Read from Files, Listing Directory Contents with readdir or readdirSync, Reading from a file synchronously, Copying files by piping streams, Check Permissions of a File or Directory, Checking if a file or a directory exists, Determining the line count of a text file, Reading a file line by line, Writing to a file using writeFile or writeFileSync, Deleting a file using unlink or unlinkSync, Reading a file into a Buffer using streams, Avoiding race conditions when creating or using an existing directory, Cloning a file using streams, Changing contents of a text file"
---

# Filesystem I/O



## Asynchronously Read from Files


Use the filesystem module for all file operations:

```js
const fs = require('fs');

```

### With Encoding

In this example, read `hello.txt` from the directory `/tmp`. This operation will be completed in the background and the callback occurs on completion or failure:

```js
fs.readFile('/tmp/hello.txt', { encoding: 'utf8' }, (err, content) => {
  // If an error occurred, output it and return
  if(err) return console.error(err);

  // No error occurred, content is a string
  console.log(content);
});

```

### Without Encoding

Read the binary file `binary.txt` from the current directory, asynchronously in the background. Note that we do not set the 'encoding' option - this prevents Node.js from decoding the contents into a string:

```js
fs.readFile('binary', (err, binaryContent) => {
  // If an error occurred, output it and return
  if(err) return console.error(err);

  // No error occurred, content is a Buffer, output it in
  // hexadecimal representation.
  console.log(content.toString('hex'));
});

```

### Relative paths

Keep in mind that, in general case, your script could be run with an arbitrary current working directory. To address a file relative to the current script, use `__dirname` or `__filename`:

```js
fs.readFile(path.resolve(__dirname, 'someFile'), (err, binaryContent) => {
  //Rest of Function
}

```



## Listing Directory Contents with readdir or readdirSync


```js
const fs = require('fs');

// Read the contents of the directory /usr/local/bin asynchronously.
// The callback will be invoked once the operation has either completed
// or failed.
fs.readdir('/usr/local/bin', (err, files) => {
  // On error, show it and return
  if(err) return console.error(err);

  // files is an array containing the names of all entries
  // in the directory, excluding '.' (the directory itself)
  // and '..' (the parent directory).

  // Display directory entries
  console.log(files.join(' '));
});

```

A synchronous variant is available as `readdirSync` which blocks the main thread and therefore prevents execution of asynchronous code at the same time. Most developers avoid synchronous IO functions in order to improve performance.

```js
let files;

try {
  files = fs.readdirSync('/var/tmp');
} catch(err) {
  // An error occurred
  console.error(err);
}

```

### Using a generator

```js
const fs = require('fs');

// Iterate through all items obtained via
// 'yield' statements
// A callback is passed to the generator function because it is required by
// the 'readdir' method
function run(gen) {
  var iter = gen((err, data) => {
    if (err) { iter.throw(err); }

    return iter.next(data);
  });

  iter.next();
}

const dirPath = '/usr/local/bin';

// Execute the generator function
run(function* (resume) {
  // Emit the list of files in the directory from the generator
  var contents = yield fs.readdir(dirPath, resume);
  console.log(contents);
});

```



## Reading from a file synchronously


For any file operations, you will need the filesystem module:

```js
const fs = require('fs');

```

### Reading a String

`fs.readFileSync` behaves similarly to `fs.readFile`, but does not take a callback as it completes synchronously and therefore blocks the main thread. Most node.js developers prefer the asynchronous variants which will cause virtually no delay in the program execution.

If an `encoding` option is specified, a string will be returned, otherwise a `Buffer` will be returned.

```js
// Read a string from another file synchronously
let content;
try {
  content = fs.readFileSync('sync.txt', { encoding: 'utf8' });
} catch(err) {
  // An error occurred
  console.error(err);
}

```



## Copying files by piping streams


This program copies a file using readable and a writable stream with the `pipe()` function provided by the stream class

```js
// require the file system module
var fs = require('fs');

/*
    Create readable stream to file in current directory named 'node.txt'
    Use utf8 encoding 
    Read the data in 16-kilobyte chunks
*/
var readable = fs.createReadStream(__dirname + '/node.txt', { encoding: 'utf8', highWaterMark: 16 * 1024 });

// create writable stream
var writable = fs.createWriteStream(__dirname + '/nodePipe.txt');

// use pipe to copy readable to writable
readable.pipe(writable);

```



## Check Permissions of a File or Directory


`fs.access()` determines whether a path exists and what permissions a user has to the file or directory at that path. `fs.access` doesn't return a result rather, if it doesn't return an error, the path exists and the user has the desired permissions.

The permission modes are available as a property on the `fs` object, `fs.constants`

- `fs.constants.F_OK` - Has read/write/execute permissions (If no mode is provided, this is the default)
- `fs.constants.R_OK` - Has read permissions
- `fs.constants.W_OK` - Has write permissions
- `fs.constants.X_OK` - Has execute permissions (Works the same as `fs.constants.F_OK` on Windows)

### Asynchronously

```js
var fs = require('fs');
var path = '/path/to/check';

// checks execute permission
fs.access(path, fs.constants.X_OK, (err) => {
    if (err) {
        console.log("%s doesn't exist", path);
    } else {
        console.log('can execute %s', path);
    }
});
// Check if we have read/write permissions
// When specifying multiple permission modes
// each mode is separated by a pipe : `|`
fs.access(path, fs.constants.R_OK | fs.constants.W_OK, (err) => {
    if (err) {
        console.log("%s doesn't exist", path);
    } else {
        console.log('can read/write %s', path);
    }
});

```

### Synchronously

`fs.access` also has a synchronous version `fs.accessSync`. When using `fs.accessSync` you must enclose it within a try/catch block.

```js
// Check write permission
try {
    fs.accessSync(path, fs.constants.W_OK);
    console.log('can write %s', path);
}
catch (err) {
    console.log("%s doesn't exist", path);
}

```



## Checking if a file or a directory exists


### Asynchronously

```js
var fs = require('fs');

fs.stat('path/to/file', function(err) {
    if (!err) {
        console.log('file or directory exists');
    }
    else if (err.code === 'ENOENT') {
        console.log('file or directory does not exist');
    }
});

```

### Synchronously

here, we must wrap the function call in a `try/catch` block to handle error.

```js
var fs = require('fs');

try {
    fs.statSync('path/to/file');
    console.log('file or directory exists');
}
catch (err) {
  if (err.code === 'ENOENT') {
    console.log('file or directory does not exist');
  }
}

```



## Determining the line count of a text file


### app.js

```js
const readline = require('readline');
const fs = require('fs');

var file = 'path.to.file';
var linesCount = 0;
var rl = readline.createInterface({
    input: fs.createReadStream(file),
    output: process.stdout,
    terminal: false
});
rl.on('line', function (line) {
    linesCount++; // on each linebreak, add +1 to 'linesCount'
});
rl.on('close', function () {
    console.log(linesCount); // print the result when the 'close' event is called
});

```

Usage:

> 
node app




## Reading a file line by line


### app.js

```js
const readline = require('readline');
const fs = require('fs');

var file = 'path.to.file';
var rl = readline.createInterface({
    input: fs.createReadStream(file),
    output: process.stdout,
    terminal: false
});

rl.on('line', function (line) {
    console.log(line) // print the content of the line on each linebreak
});

```

Usage:

> 
node app




## Writing to a file using writeFile or writeFileSync


```js
var fs = require('fs');

// Save the string "Hello world!" in a file called "hello.txt" in
// the directory "/tmp" using the default encoding (utf8).
// This operation will be completed in background and the callback
// will be called when it is either done or failed.
fs.writeFile('/tmp/hello.txt', 'Hello world!', function(err) {
  // If an error occurred, show it and return
  if(err) return console.error(err);
  // Successfully wrote to the file!
});

// Save binary data to a file called "binary.txt" in the current
// directory. Again, the operation will be completed in background.
var buffer = new Buffer([ 0x48, 0x65, 0x6c, 0x6c, 0x6f ]);
fs.writeFile('binary.txt', buffer, function(err) {
  // If an error occurred, show it and return
  if(err) return console.error(err);
  // Successfully wrote binary contents to the file!
});

```

`fs.writeFileSync` behaves similarly to `fs.writeFile`, but does not take a callback as it completes synchronously and therefore blocks the main thread. Most node.js developers prefer the asynchronous variants which will cause virtually no delay in the program execution.

**Note: Blocking the main thread is bad practice in node.js. Synchronous function should only be used when debugging or when no other options are availables.**

```js
// Write a string to another file and set the file mode to 0755
try {
  fs.writeFileSync('sync.txt', 'anni', { mode: 0o755 });
} catch(err) {
  // An error occurred
  console.error(err);
}

```



## Deleting a file using unlink or unlinkSync


Delete a file asynchronously:

```js
var fs = require('fs');

fs.unlink('/path/to/file.txt', function(err) {
  if (err) throw err;

  console.log('file deleted');
});

```

You can also delete it synchronously*:

```js
var fs = require('fs');

fs.unlinkSync('/path/to/file.txt');
console.log('file deleted');

```

* avoid synchronous methods because they block the entire process until the execution finishes.



## Reading a file into a Buffer using streams


While reading content from a file is already asynchronous using the `fs.readFile()` method, sometimes we want to get the data in a Stream versus in a simple callback. This allows us to pipe this data to other locations or to process it as it comes in versus all at once at the end.

```js
const fs = require('fs');

// Store file data chunks in this array
let chunks = [];
// We can use this variable to store the final data
let fileBuffer;

// Read file into stream.Readable
let fileStream = fs.createReadStream('text.txt');

// An error occurred with the stream
fileStream.once('error', (err) => {
    // Be sure to handle this properly!
    console.error(err); 
});

// File is done being read
fileStream.once('end', () => {
    // create the final data Buffer from data chunks;
    fileBuffer = Buffer.concat(chunks);
    
    // Of course, you can do anything else you need to here, like emit an event!
});

// Data is flushed from fileStream in chunks,
// this callback will be executed for each chunk
fileStream.on('data', (chunk) => {
    chunks.push(chunk); // push data chunk to array

    // We can perform actions on the partial data we have so far!
});

```



## Avoiding race conditions when creating or using an existing directory


Due to Node's asynchronous nature, creating or using a directory by first:

1. checking for its existence with `fs.stat()`, then
1. creating or using it depending of the results of the existence check,

can lead to a [race condition](https://www.wikiwand.com/en/Race_condition#/File_systems) if the folder is created between the time of the check and the time of the creation. The method below wraps `fs.mkdir()` and `fs.mkdirSync()` in error-catching wrappers that let the exception pass if its code is `EEXIST` (already exists). If the error is something else, like `EPERM` (pemission denied), throw or pass an error like the native functions do.

**Asynchronous version with `fs.mkdir()`**

**Synchronous version with `fs.mkdirSync()`**



## Cloning a file using streams


This program illustrates how one can copy a file using readable and writable streams using the `createReadStream()`, and `createWriteStream()` functions provided by the file system module.

```js
//Require the file System module
var fs = require('fs');

/*
  Create readable stream to file in current directory (__dirname) named 'node.txt'
  Use utf8 encoding 
  Read the data in 16-kilobyte chunks
*/
var readable = fs.createReadStream(__dirname + '/node.txt', { encoding: 'utf8', highWaterMark: 16 * 1024 });

// create writable stream
var writable = fs.createWriteStream(__dirname + '/nodeCopy.txt');

// Write each chunk of data to the writable stream
readable.on('data', function(chunk) {
    writable.write(chunk);
});

```



## Changing contents of a text file


Example. It will be replacing the word `email` to a `name` in a text file `index.txt` with simple RegExp `replace(/email/gim, 'name')`

```js
var fs = require('fs');
 
fs.readFile('index.txt', 'utf-8', function(err, data) {
    if (err) throw err;
 
    var newValue = data.replace(/email/gim, 'name');
 
    fs.writeFile('index.txt', newValue, 'utf-8', function(err, data) {
        if (err) throw err;
        console.log('Done!');
    })
})

```



#### Remarks


In Node.js, resource intensive operations such as I/O are performed **asynchronously**, but have a **synchronous** counterpart (e.g. there exists a `fs.readFile` and its counterpart is `fs.readFileSync`). Since Node is single-threaded, you should be careful when using **synchronous** operations, because they will block the entire process.

If a process is blocked by a synchronous operation, the entire execution cycle (including the event loop) is halted. That means other asynchronous code, including events and event handlers, will not run and your program will continue to wait until the single blocking operation has completed.

There are appropriate uses for both synchronous and asynchronous operations, but care must be taken that they are utilized properly.

