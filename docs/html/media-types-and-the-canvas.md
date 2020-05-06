---
metaTitle: "HTML - Media types and the canvas"
description: "Basic loading and playing a video on the canvas., Loading and displaying an Image, Drawing an svg image, Capture canvas and Save as webM video"
---

# Media types and the canvas



## Basic loading and playing a video on the canvas.


The canvas can be used to display video from a variety of sources. This example shows how to load a video as a file resource, display it and add a simple click on screen play/pause toggle.

This stackoverflow self answered question [How do I display a video using HTML5 canvas tag](http://stackoverflow.com/a/38710126/3877726) shows the following example code in action.

### Just an image

A video is just an image as far as the canvas is concerned. You can draw it like any image. The difference being the video can play and has sound.

### Get canvas and basic setup

```html
// It is assumed you know how to add a canvas and correctly size it.
var canvas = document.getElementById("myCanvas"); // get the canvas from the page
var ctx = canvas.getContext("2d");
var videoContainer; // object to hold video and associated info

```

### Creating and loading the video

```html
var video = document.createElement("video"); // create a video element
video.src = "urlOffVideo.webm"; 
// the video will now begin to load.
// As some additional info is needed we will place the video in a
// containing object for convenience
video.autoPlay = false; // ensure that the video does not auto play
video.loop = true; // set the video to loop.
videoContainer = {  // we will add properties as needed
     video : video,
     ready : false,   
};

```

Unlike images elements videos don't have to be fully loaded to be displayed on the canvas. Videos also provide a host of extra events that can be used to monitor status of the video.

In this case we wish to know when the video is ready to play. `oncanplay` means that enough of the video has loaded to play some of it, but there may not be enough to play to the end.

```html
video.oncanplay = readyToPlayVideo; // set the event to the play function that 
                                  // can be found below

```

Alternatively you can use `oncanplaythrough` which will fire when enough of the video has loaded so that it can be played to the end.

```html
video.oncanplaythrough = readyToPlayVideo; // set the event to the play function that
                                         // can be found below

```

Only use one of the canPlay events not both.

### The can play event (equivalent to image onload)

```html
function readyToPlayVideo(event){ // this is a referance to the video
    // the video may not match the canvas size so find a scale to fit
    videoContainer.scale = Math.min(
                         canvas.width / this.videoWidth, 
                         canvas.height / this.videoHeight); 
    videoContainer.ready = true;
    // the video can be played so hand it off to the display function
    requestAnimationFrame(undateCanvas);
}

```

### Displaying

The video will not play itself on the canvas. You need to draw it for every new frame. As it is difficult to know the exact frame rate and when they occur the best approch is to display the video as if running at 60fps. If the frame rate is lower then w just render the same frame twice. If the frame rate is higher then there is nothing that can be don to see the extra frames so we just ignore them.

The video element is just a image element and can be draw like any image, you can scale, rotate, pan the video, mirror it, fade it, clip it and display only parts, draw it twice the second time with a global composite mode to add FX like lighten, screen, etc..

```html
function updateCanvas(){
    ctx.clearRect(0,0,canvas.width,canvas.height); // Though not always needed 
                                                     // you may get bad pixels from 
                                                     // previous videos so clear to be
                                                     // safe
    // only draw if loaded and ready
    if(videoContainer !== undefined && videoContainer.ready){ 
        // find the top left of the video on the canvas
        var scale = videoContainer.scale;
        var vidH = videoContainer.video.videoHeight;
        var vidW = videoContainer.video.videoWidth;
        var top = canvas.height / 2 - (vidH /2 ) * scale;
        var left = canvas.width / 2 - (vidW /2 ) * scale;
        // now just draw the video the correct size
        ctx.drawImage(videoContainer.video, left, top, vidW * scale, vidH * scale);
        if(videoContainer.video.paused){ // if not playing show the paused screen 
            drawPayIcon();
        }
    }
    // all done for display 
    // request the next frame in 1/60th of a second
    requestAnimationFrame(updateCanvas);
}

```

### Basic play pause control

Now we have the video loaded and displayed all we need is the play control. We will make it as a click toggle play on the screen. When the video is playing and the user clicks the video is paused. When paused the click resumes play. We will add a function to darken the video and draw an play icon (triangle)

```html
function drawPayIcon(){
     ctx.fillStyle = "black";  // darken display
     ctx.globalAlpha = 0.5;
     ctx.fillRect(0,0,canvas.width,canvas.height);
     ctx.fillStyle = "#DDD"; // colour of play icon
     ctx.globalAlpha = 0.75; // partly transparent
     ctx.beginPath(); // create the path for the icon
     var size = (canvas.height / 2) * 0.5;  // the size of the icon
     ctx.moveTo(canvas.width/2 + size/2, canvas.height / 2); // start at the pointy end
     ctx.lineTo(canvas.width/2 - size/2, canvas.height / 2 + size);
     ctx.lineTo(canvas.width/2 - size/2, canvas.height / 2 - size);
     ctx.closePath();
     ctx.fill();
     ctx.globalAlpha = 1; // restore alpha
}    

```

### Now the play pause event

```html
function playPauseClick(){
     if(videoContainer !== undefined && videoContainer.ready){
          if(videoContainer.video.paused){                                 
                videoContainer.video.play();
          }else{
                videoContainer.video.pause();
          }
     }
}
// register the event
canvas.addEventListener("click",playPauseClick);

```

### Summary

Playing a video is very easy using the canvas, adding effect in real time is also easy. There are however some limitations on formats, how you can play and seek. MDN HTMLMediaElement is the place to get the full referance to the video object.

Once the image has been drawn on the canvas you can use `ctx.getImageData` to access the pixels it contains. Or you can use `canvas.toDataURL` to snap a still and download it. (Only if the video is from a trusted source and does not taint the canvas).

Note if the video has sound then playing it will also play the sound.

Happy videoing.



## Loading and displaying an Image


To load an image and place it on the canvas

```html
var image = new Image();  // see note on creating an image
image.src = "imageURL";
image.onload = function(){
    ctx.drawImage(this,0,0);
}

```

**Creating an image**

There are several ways to create an image

- `new Image()`
- `document.createElement("img")`
- `<img src = 'imageUrl' id='myImage'>`  As part of the HTML body and retrieved with `document.getElementById('myImage')`

The image is a `HTMLImageElement`

**Image.src  property**

The image `src`can be any valid image URL or encoded dataURL. See this topic's Remarks for more information on image formats and support.

- `image.src = "http://my.domain.com/images/myImage.jpg"`
- `image.src = "data:image/gif;base64,R0lGODlhAQABAIAAAAUEBAAAACwAAAAAAQABAAACAkQBADs="` *

*<sub>The dataURL is a 1 by 1 pixel gif image containing black</sub>

**Remarks on loading and errors**

The image will begin loading when its src property is set. The loading is syncriouse but the `onload` event will not be called until the function or code has exited/returned.

If you get an image from the page (for example `document.getElementById("myImage")`) and its `src` is set it may or may not have loaded. You can check on the status of the image with `HTMLImageElement.complete` which will be `true` if complete. This does not mean the image has loaded, it means that it has either

- loaded
- there was an error
- src property has not been set and is equal to the empty String `""`

If the image is from an unreliable source and may not be accessible for a variety of reasons it will generate an error event. When this happens the image will be in a broken state. If you then attempt to draw it onto the canvas it will throw the following error

```html
Uncaught DOMException: Failed to execute 'drawImage' on 'CanvasRenderingContext2D': The HTMLImageElement provided is in the 'broken' state.

```

By supplying the `image.onerror = myImgErrorHandler` event you can take appropriate action to prevent errors.



## Drawing an svg image


To draw a vector SVG image, the operation is not different from a raster image :<br />
You first need to load your SVG image into an HTMLImage element, then use the `drawImage()` method.

```html
var image = new Image();
image.onload = function(){
    ctx.drawImage(this, 0,0);
}
image.src = "someFile.SVG";

```

SVG images have some advantages over raster ones, since you won't loose quality, whatever the scale you'll draw it on your canvas. But beware, it may also be a bit slower than drawing a raster image.

However, SVG images come with more restrictions than raster images.

<li>
<p>**For security purpose, no external content can be loaded from an SVG image referenced in an HTMLImageElement(`<img>`)**<br />
No external stylesheet, no external image referenced in SVGImage (`<image/>`) elements, no external filter or element linked by the `xlink:href` attribute (`<use xlink:href="anImage.SVG#anElement"/>`) or the funcIRI (`url()`) attribute method etc.<br />
Also, stylesheets appended in the main document won't have any effect on the SVG document once referenced in an HTMLImage element.<br />
Finally, no script will be executed inside the SVG Image.<br />
**Workaround :** You'll need to append all external elements inside the SVG itself before referrencing to the HTMLImage element. (for images or fonts, you need to append a dataURI version of your external resources).</p>
</li>
<li>
<p>**The root element (`<svg>`) must have its width and height attributes set to an absolute value.**<br />
If you were to use relative length (e.g `%`), then the browser won't be able to know to what it is relative.
Some browsers (Blink) will try to make a guess, but most will simply ignore your image and won't draw anything, without a warning.</p>
</li>
<li>
<p>**Some browsers will [taint the canvas](http://stackoverflow.com/documentation/html5-canvas/1892/introduction-to-html5-canvas/10512/the-tainted-canvas#t=201607270037419008111) when an SVG image has been drawn to it.**<br />
Specifically, Internet-Explorer < Edge in any case, and Safari 9 when a `<foreignObject>` is present in the SVG image.</p>
</li>



## Capture canvas and Save as webM video


Creating a WebM video from canvas frames and playing in canvas, or upload, or downloading.

### Example capture and play canvas

```html
name = "CanvasCapture"; // Placed into the Mux and Write Application Name fields of the WebM header
quality = 0.7; // good quality 1 Best < 0.7 ok to poor
fps = 30; // I have tried all sorts of frame rates and all seem to work
          // Do some test to workout what your machine can handle as there
          // is a lot of variation between machines.
var video = new Groover.Video(fps,quality,name)
function capture(){
    if(video.timecode < 5000){ // 5 seconds
         setTimeout(capture,video.frameDelay);             
    }else{
         var videoElement = document.createElement("video");
         videoElement.src = URL.createObjectURL(video.toBlob());
         document.body.appendChild(videoElement);
         video = undefined; // DeReference as it is memory hungry.
         return;
    }
    // first frame sets the video size
    video.addFrame(canvas); // Add current canvas frame
}
capture(); // start capture

```

Rather than put in a huge effort only to be rejected, this is a quick insert to see if acceptable. Will Give full details if accepted. Also include additional capture options for better HD capture rates (removed from this version, Can capture HD 1080 at 50fps on good machines.)

This was inspired by [Wammy](https://github.com/antimatter15/whammy) but is a complete rewrite with encode as you go methodology, greatly reducing the memory required during capture. Can capture more than 30 seconds better data, handling algorithms.

> 
**Note** frames are encoded into webP images. Only Chrome supports webP canvas encoding. For other browsers (Firefox and Edge) you will need to use a 3rd party webP encoder such as [Libwebp Javascript](http://libwebpjs.appspot.com/v0.1.3/) Encoding WebP images via Javascript is slow. (will include addition of raw webp images support if accepted).


The webM encoder inspired by [Whammy: A Real Time Javascript WebM](https://github.com/antimatter15/whammy)

```html
var Groover = (function(){
    // ensure webp is supported 
    function canEncode(){
        var canvas = document.createElement("canvas");
        canvas.width = 8;
        canvas.height = 8;
        return canvas.toDataURL("image/webp",0.1).indexOf("image/webp") > -1;
    }
    if(!canEncode()){
        return undefined;
    }    
    var webmData = null;
    var clusterTimecode = 0;
    var clusterCounter = 0;
    var CLUSTER_MAX_DURATION = 30000;
    var frameNumber = 0;
    var width;
    var height;
    var frameDelay;
    var quality;
    var name;
    const videoMimeType = "video/webm"; // the only one.
    const frameMimeType = 'image/webp'; // can be no other
    const S = String.fromCharCode;
    const dataTypes = {
        object : function(data){ return toBlob(data);},
        number : function(data){ return stream.num(data);},
        string : function(data){ return stream.str(data);},
        array  : function(data){ return data;}, 
        double2Str : function(num){
            var c = new Uint8Array((new Float64Array([num])).buffer);
            return S(c[7]) + S(c[6]) + S(c[5]) + S(c[4]) + S(c[3]) + S(c[2]) + S(c[1]) + S(c[0]);
        }
    };    
   
    const stream = {
        num : function(num){ // writes int
            var parts = [];
            while(num > 0){ parts.push(num & 0xff); num = num >> 8; }
            return new Uint8Array(parts.reverse());
        },
        str : function(str){ // writes string
            var i, len, arr;
            len = str.length;
            arr = new Uint8Array(len);
            for(i = 0; i < len; i++){arr[i] = str.charCodeAt(i);}
            return arr;
        },
        compInt : function(num){ // could not find full details so bit of a guess
            if(num < 128){       // number is prefixed with a bit (1000 is on byte 0100 two, 0010 three and so on)
                num += 0x80;
                return new Uint8Array([num]);
            }else
            if(num < 0x4000){
                num += 0x4000;
                return new Uint8Array([num>>8, num])
            }else
            if(num < 0x200000){
                num += 0x200000;
                return new Uint8Array([num>>16, num>>8, num])
            }else
            if(num < 0x10000000){
                num += 0x10000000;
                return new Uint8Array([num>>24, num>>16, num>>8, num])
            }            
        }
    }
    const ids = { // header names and values
        videoData          : 0x1a45dfa3, 
        Version            : 0x4286,
        ReadVersion        : 0x42f7,
        MaxIDLength        : 0x42f2,
        MaxSizeLength      : 0x42f3,
        DocType            : 0x4282,
        DocTypeVersion     : 0x4287,
        DocTypeReadVersion : 0x4285,
        Segment            : 0x18538067,
        Info               : 0x1549a966,
        TimecodeScale      : 0x2ad7b1,
        MuxingApp          : 0x4d80,
        WritingApp         : 0x5741,
        Duration           : 0x4489,
        Tracks             : 0x1654ae6b,
        TrackEntry         : 0xae,
        TrackNumber        : 0xd7,
        TrackUID           : 0x63c5,
        FlagLacing         : 0x9c,
        Language           : 0x22b59c,
        CodecID            : 0x86,
        CodecName          : 0x258688,
        TrackType          : 0x83,
        Video              : 0xe0,
        PixelWidth         : 0xb0,
        PixelHeight        : 0xba,
        Cluster            : 0x1f43b675,
        Timecode           : 0xe7,
        Frame              : 0xa3,
        Keyframe           : 0x9d012a,
        FrameBlock         : 0x81,
    };
    const keyframeD64Header = '\x9d\x01\x2a'; //VP8 keyframe header 0x9d012a
    const videoDataPos = 1; // data pos of frame data header
    const defaultDelay = dataTypes.double2Str(1000/25);
    const header = [  // structure of webM header/chunks what ever they are called.
        ids.videoData,[
            ids.Version, 1,
            ids.ReadVersion, 1,
            ids.MaxIDLength, 4,
            ids.MaxSizeLength, 8,
            ids.DocType, 'webm',
            ids.DocTypeVersion, 2,
            ids.DocTypeReadVersion, 2
        ],
        ids.Segment, [
            ids.Info, [
                ids.TimecodeScale, 1000000,
                ids.MuxingApp, 'Groover',
                ids.WritingApp, 'Groover',
                ids.Duration, 0
            ],
            ids.Tracks,[
                ids.TrackEntry,[
                    ids.TrackNumber, 1,
                    ids.TrackUID, 1,
                    ids.FlagLacing, 0,     // always o
                    ids.Language, 'und',   // undefined I think this means
                    ids.CodecID, 'V_VP8',  // These I think must not change
                    ids.CodecName, 'VP8',  // These I think must not change
                    ids.TrackType, 1,
                    ids.Video, [
                        ids.PixelWidth, 0,
                        ids.PixelHeight, 0
                    ]
                ]
            ]
        ]
    ];    
    function getHeader(){
        header[3][2][3] = name;
        header[3][2][5] = name;
        header[3][2][7] =  dataTypes.double2Str(frameDelay);
        header[3][3][1][15][1] =  width;
        header[3][3][1][15][3] =  height;
        function create(dat){
            var i,kv,data;
            data = [];
            for(i = 0; i < dat.length; i += 2){
                kv = {i : dat[i]};
                if(Array.isArray(dat[i + 1])){
                    kv.d = create(dat[i + 1]);
                }else{
                    kv.d = dat[i + 1];
                }
                data.push(kv);
            }
            return data;
        }
        return create(header);
    }
    function addCluster(){
        webmData[videoDataPos].d.push({ i: ids.Cluster,d: [{ i: ids.Timecode, d: Math.round(clusterTimecode)}]}); // Fixed bug with Round
        clusterCounter = 0;
    }
    function addFrame(frame){
        var VP8, kfS,riff;
        riff = getWebPChunks(atob(frame.toDataURL(frameMimeType, quality).slice(23)));
        VP8 = riff.RIFF[0].WEBP[0];
        kfS = VP8.indexOf(keyframeD64Header) + 3;
        frame = {
            width: ((VP8.charCodeAt(kfS + 1) << 8) | VP8.charCodeAt(kfS)) & 0x3FFF,
            height: ((VP8.charCodeAt(kfS + 3) << 8) | VP8.charCodeAt(kfS + 2)) & 0x3FFF,
            data: VP8,
            riff: riff
        };
        if(clusterCounter > CLUSTER_MAX_DURATION){
            addCluster();            
        }
        webmData[videoDataPos].d[webmData[videoDataPos].d.length-1].d.push({
            i: ids.Frame, 
            d: S(ids.FrameBlock) + S( Math.round(clusterCounter) >> 8) +  S( Math.round(clusterCounter) & 0xff) + S(128) + frame.data.slice(4),
        });
        clusterCounter += frameDelay;        
        clusterTimecode += frameDelay;
        webmData[videoDataPos].d[0].d[3].d = dataTypes.double2Str(clusterTimecode);
    }
    function startEncoding(){
        frameNumber = clusterCounter = clusterTimecode = 0;
        webmData  = getHeader();
        addCluster();
    }    
    function toBlob(vidData){
        var data,i,vData, len;
        vData = [];
        for(i = 0; i < vidData.length; i++){
            data = dataTypes[typeof vidData[i].d](vidData[i].d);
            len  = data.size || data.byteLength || data.length;
            vData.push(stream.num(vidData[i].i));
            vData.push(stream.compInt(len));
            vData.push(data)
        }
        return new Blob(vData, {type: videoMimeType});
    }
    function getWebPChunks(str){
        var offset, chunks, id, len, data;
        offset = 0;
        chunks = {};
        while (offset < str.length) {
            id = str.substr(offset, 4);
            // value will have top bit on (bit 32) so not simply a bitwise operation
            // Warning little endian (Will not work on big endian systems)
            len = new Uint32Array(
                new Uint8Array([
                    str.charCodeAt(offset + 7),
                    str.charCodeAt(offset + 6),
                    str.charCodeAt(offset + 5),
                    str.charCodeAt(offset + 4)
                ]).buffer)[0];
            id = str.substr(offset, 4);
            chunks[id] = chunks[id] === undefined ? [] : chunks[id];
            if (id === 'RIFF' || id === 'LIST') {
                chunks[id].push(getWebPChunks(str.substr(offset + 8, len)));
                offset += 8 + len;
            } else if (id === 'WEBP') {
                chunks[id].push(str.substr(offset + 8));
                break;
            } else {
                chunks[id].push(str.substr(offset + 4));
                break;
            }
        }
        return chunks;
    }
    function Encoder(fps, _quality = 0.8, _name = "Groover"){ 
        this.fps = fps;
        this.quality = quality = _quality;
        this.frameDelay = frameDelay = 1000 / fps;
        this.frame = 0;
        this.width = width = null;
        this.timecode = 0;
        this.name = name = _name;
    }
    Encoder.prototype = {
        addFrame : function(frame){
            if('canvas' in frame){
                frame = frame.canvas;    
            }
            if(width === null){
                this.width = width = frame.width,
                this.height = height = frame.height
                startEncoding();
            }else
            if(width !== frame.width || height !== frame.height){
                throw RangeError("Frame size error. Frames must be the same size.");
            }            
            addFrame(frame);   
            this.frame += 1;
            this.timecode = clusterTimecode;
        },        
        toBlob : function(){
            return toBlob(webmData);
        }
    }
    return {
        Video: Encoder,
    }
})()

```



#### Remarks


This topic is to cover the various media types and how they can be used with the canvas in 2D interface.

Media types have generic and format specific categories

**Media types**

- Animations
- Videos
- Images
- HD images
- Vector image
- Animated images

**Media formats**

- Jpg/Jpeg
- Png
- Gif
- SVG
- M-JPEG
- Webm
- Webp

**Images**

There are a wide variety of image formats supported by browsers, though no browser support them all. If you have particular image formats you wish to use [Wiki Browsers and supported image formats](https://en.wikipedia.org/wiki/Comparison_of_web_browsers#Image_format_support) provides a good overview.

The best support is for the 3 main formats, "jpeg", "png", and "gif" with all the major browsers providing support.

**JPEG**

JPEG images are best suited to photos and photo like images. They do not lend them selves well to charts, diagrams, and text. JPEG images do not support transparency.

Canvas can output JPEG images via `canvas.toDataURL` and `canvas.toBlob` and provides a quality setting. As JPEG does not support transparency any transparent pixels will be blended with black for the final output JPG. The resulting image will not be a perfect copy of the canvas.

[JPEG at wikipedia](https://en.wikipedia.org/wiki/JPEG)

**PNG**

PNG Image are the highest quality images and can also include an alpha channel for transparent pixels. The image data is compressed but does not produce artifacts like JPG images.

Because of the lossless compression and the alpha channel support PNGs are used for games, ui component images, charts, diagrams, text. When using them to for photo and photo like images their file size can be much larger than JPEG's. .

The PNG format also provides animation support though browser support is limited, and access to the animation for use on the canvas can only be done via Javascript APIs & libraries

THe canvas can be used to encode PNG images via `canvas.toDataURL` and `canvas.toBlob` though the output format is limited to compressed 32Bit RGBA. The PNG will provide a pixel perfect copy of the canvas.

[PNG at wikipedia](https://en.wikipedia.org/wiki/Portable_Network_Graphics)

**GIF**

GIFs are used for short animations, but can also be used to provide high quality charts, diagrams, and text like images. GIFs have very limited colour support with a maximum of 256 colours per frame. With cleaver image processing gif images can produce surprisingly good results, especially when animated. Gifs also provide transparency though this is limited to on or off

AS with PNG, GIF animations are not directly accessible for use on the canvas and you will need a Javascript API or library to get access. GIF can not be saved via the canvas and will require and API or library to do so.

[GIF at wikipedia](https://en.wikipedia.org/wiki/GIF)

