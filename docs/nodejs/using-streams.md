---
metaTitle: "Using Streams"
description: "Read Data from TextFile with Streams, Piping streams, Creating your own readable/writable stream, Why Streams?"
---

# Using Streams



## Read Data from TextFile with Streams


I/O in node is asynchronous, so interacting with the disk and network involves passing callbacks to functions. You might be tempted to write code that serves up a file from disk like this:

```js
var http = require('http');
var fs = require('fs');

var server = http.createServer(function (req, res) {
    fs.readFile(__dirname + '/data.txt', function (err, data) {
        res.end(data);
    });
});
server.listen(8000);

```

This code works but it's bulky and buffers up the entire data.txt file into memory for every request before writing the result back to clients. If data.txt is very large, your program could start eating a lot of memory as it serves lots of users concurrently, particularly for users on slow connections.

The user experience is poor too because users will need to wait for the whole file to be buffered into memory on your server before they can start receiving any contents.

Luckily both of the (req, res) arguments are streams, which means we can write this in a much better way using fs.createReadStream() instead of fs.readFile():

```js
var http = require('http');
var fs = require('fs');

var server = http.createServer(function (req, res) {
    var stream = fs.createReadStream(__dirname + '/data.txt');
    stream.pipe(res);
});
server.listen(8000);

```

Here .pipe() takes care of listening for 'data' and 'end' events from the fs.createReadStream(). This code is not only cleaner, but now the data.txt file will be written to clients one chunk at a time immediately as they are received from the disk.



## Piping streams


Readable streams can be "piped," or connected, to writable streams. This makes data flow from the source stream to the destination stream without much effort.

```js
var fs = require('fs')

var readable = fs.createReadStream('file1.txt')
var writable = fs.createWriteStream('file2.txt')

readable.pipe(writable) // returns writable

```

When writable streams are also readable streams, i.e. when they're **duplex** streams, you can continue piping it to other writable streams.

```js
var zlib = require('zlib')

fs.createReadStream('style.css')
  .pipe(zlib.createGzip()) // The returned object, zlib.Gzip, is a duplex stream.
  .pipe(fs.createWriteStream('style.css.gz')

```

Readable streams can also be piped into multiple streams.

```js
var readable = fs.createReadStream('source.css')
readable.pipe(zlib.createGzip()).pipe(fs.createWriteStream('output.css.gz'))
readable.pipe(fs.createWriteStream('output.css')

```

Note that you must pipe to the output streams synchronously (at the same time) before any data 'flows'. Failure to do so might lead to incomplete data being streamed.

Also note that stream objects can emit `error` events; be sure to responsibly handle these events on **every** stream, as needed:

```js
var readable = fs.createReadStream('file3.txt')
var writable = fs.createWriteStream('file4.txt')
readable.pipe(writable)
readable.on('error', console.error)
writable.on('error', console.error)

```



## Creating your own readable/writable stream


We will see stream objects being returned by modules like fs etc but what if we want to create our own streamable object.

To create Stream object we need to use the stream module provided by NodeJs

```

   var fs = require("fs");
    var stream = require("stream").Writable;
    
    /* 
     *  Implementing the write function in writable stream class.
     *  This is the function which will be used when other stream is piped into this 
     *  writable stream.
     */
    stream.prototype._write = function(chunk, data){
        console.log(data);
    }
    
    var customStream = new stream();
    
    fs.createReadStream("am1.js").pipe(customStream);

```

This will give us our own custom writable stream. we can implement anything within         the **_write** function.
Above method works in NodeJs 4.x.x version but in NodeJs 6.x **ES6** introduced classes
therefore syntax have changed. Below is the code for 6.x version of NodeJs

```

   const Writable = require('stream').Writable;
    
    class MyWritable extends Writable {
      constructor(options) {
        super(options);
      }
    
      _write(chunk, encoding, callback) {
        console.log(chunk);
      }
    }

```



## Why Streams?


Lets examine the following two examples for reading a file's contents:

The first one, which uses an async method for reading a file, and providing a callback function which is called once the file is fully read into the memory:

```js
fs.readFile(`${__dirname}/utils.js`, (err, data) => {
  if (err) {
    handleError(err);
  } else {
    console.log(data.toString());
  }
})

```

And the second, which uses `streams` in order to read the file's content, piece by piece:

```js
var fileStream = fs.createReadStream(`${__dirname}/file`);
var fileContent = '';
fileStream.on('data', data => {
  fileContent += data.toString();
})

fileStream.on('end', () => {
  console.log(fileContent);
})

fileStream.on('error', err => {
  handleError(err)
})

```

It's worth mentioning that both examples do the **exact same thing**.
What's the difference  then?

- The first one is shorter and looks more elegant
- The second lets you do some processing on the file **while** it is being read (!)

When the files you deal with are small then there is no real effect when using `streams`, but what happens when the file is big? (so big that it takes 10 seconds to read it into memory)

Without `streams` you'll be waiting, doing absolutely nothing (unless your process does other stuff), until the 10 seconds pass and the file is **fully read**, and only then you can start processing the file.

With `streams`, you get the file's contents piece by piece, **right when they're available** - and that lets you process the file **while** it is being read.

The above example does not illustrate how `streams` can be utilized for work that cannot be done when going the callback fashion, so lets look at another example:

I would like to download a `gzip` file, unzip it and save its content to the disk.
Given the file's `url` this is what's need to be done:

- Download the file
- Unzip the file
- Save it to disk

Here's a [small file][1], which is stored in my `S3` storage. The following code does the above in the callback fashion.

```js
var startTime = Date.now()
s3.getObject({Bucket: 'some-bucket', Key: 'tweets.gz'}, (err, data) => {
  // here, the whole file was downloaded

  zlib.gunzip(data.Body, (err, data) => {
    // here, the whole file was unzipped

    fs.writeFile(`${__dirname}/tweets.json`, data, err => {
      if (err) console.error(err)

      // here, the whole file was written to disk
      var endTime = Date.now()
      console.log(`${endTime - startTime} milliseconds`) // 1339 milliseconds
    })
  })
})

// 1339 milliseconds

```

This is how it looks using `streams`:

```js
s3.getObject({Bucket: 'some-bucket', Key: 'tweets.gz'}).createReadStream()
  .pipe(zlib.createGunzip())
  .pipe(fs.createWriteStream(`${__dirname}/tweets.json`));

// 1204 milliseconds

```

Yep, it's not faster when dealing with small files - the tested file weights `80KB`.
Testing this on a bigger file, `71MB` gzipped (`382MB` unzipped), shows that the `streams` version is much faster

- It took 20925 milliseconds to download `71MB`, unzip it and then write `382MB` to disk - **using the callback fashion**.
- In comparison, it took 13434 milliseconds to do the same when using the `streams` version (35% faster, for a not-so-big file)



#### Parameters


|Parameter|Definition
|---|---|---
|Readable Stream|type of stream where data can be read from
|Writable Stream|type of stream where data can be written to
|Duplex Stream|type of stream that is both readable and writeable
|Transform Stream|type of duplex stream that can transform data as it is being read and then written

