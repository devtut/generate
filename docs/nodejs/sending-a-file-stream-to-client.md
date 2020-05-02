---
metaTitle: "Sending a file stream to client"
description: "Using fs And pipe To Stream Static Files From The Server, Streaming Using fluent-ffmpeg"
---

# Sending a file stream to client



## Using fs And pipe To Stream Static Files From The Server


A good VOD (Video On Demand) service should start with the basics. Lets say you have a directory on your server that is not publicly accessible, yet through some sort of portal or paywall you want to allow users to access your media.

```js
var movie = path.resolve('./public/' + req.params.filename);

        fs.stat(movie, function (err, stats) {

            var range = req.headers.range;

            if (!range) {

                return res.sendStatus(416);

            }

            //Chunk logic here
            var positions = range.replace(/bytes=/, "").split("-");
            var start = parseInt(positions[0], 10);
            var total = stats.size;
            var end = positions[1] ? parseInt(positions[1], 10) : total - 1;
            var chunksize = (end - start) + 1;

            res.writeHead(206, {

                'Transfer-Encoding': 'chunked',

                "Content-Range": "bytes " + start + "-" + end + "/" + total,

                "Accept-Ranges": "bytes",

                "Content-Length": chunksize,

                "Content-Type": mime.lookup(req.params.filename)

            });

            var stream = fs.createReadStream(movie, { start: start, end: end, autoClose: true })

                .on('end', function () {

                    console.log('Stream Done');

                })

                .on("error", function (err) {

                    res.end(err);

                })

                .pipe(res, { end: true });

        });

```

The above snippet is a basic outline for how you would like to stream your video to a client. The chunk logic depends on a variety of factors, including network traffic and latency. It is important to balance chuck size vs. quantity.

Finally, the .pipe call lets node.js know to keep a connection open with the server and to send additional chunks as needed.



## Streaming Using fluent-ffmpeg


You can also use flent-ffmpeg to convert .mp4 files to .flv files, or other types:

res.contentType('flv');

```

   var pathToMovie = './public/' + req.params.filename;

    var proc = ffmpeg(pathToMovie)

        .preset('flashvideo')

        .on('end', function () {

            console.log('Stream Done');

        })

        .on('error', function (err) {

            console.log('an error happened: ' + err.message);

            res.send(err.message);

        })

        .pipe(res, { end: true }); 

```

