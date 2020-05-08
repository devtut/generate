---
metaTitle: "Android - JCodec"
description: "Getting Started, Getting frame from movie"
---

# JCodec




## Getting Started


You can get JCodec automatically with maven. For this just add below snippet to your pom.xml .

```java
<dependency>
    <groupId>org.jcodec</groupId>
    <artifactId>jcodec-javase</artifactId>
    <version>0.1.9</version>
</dependency>

```



## Getting frame from movie


Getting a single frame from a movie ( supports only AVC, H.264 in MP4, ISO BMF, Quicktime container ):

```java
int frameNumber = 150;
BufferedImage frame = FrameGrab.getFrame(new File("filename.mp4"), frameNumber);
ImageIO.write(frame, "png", new File("frame_150.png"));

```

Getting a sequence of frames from a movie ( supports only AVC, H.264 in MP4, ISO BMF, Quicktime container ):

```java
double startSec = 51.632;
FileChannelWrapper ch = null;
try {
    ch = NIOUtils.readableFileChannel(new File("filename.mp4"));
    FrameGrab fg = new FrameGrab(ch);
    grab.seek(startSec);
    for (int i = 0; i < 100; i++) {
        ImageIO.write(grab.getFrame(), "png",
            new File(System.getProperty("user.home"), String.format("Desktop/frame_%08d.png", i)));
    }
} finally {
    NIOUtils.closeQuietly(ch);
}

```

