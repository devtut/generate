---
metaTitle: "Android - Okio"
description: "Download / Implement, PNG decoder, ByteStrings and Buffers"
---

# Okio




## Download / Implement


Download the latest JAR or grab via Maven:

```java
<dependency>
    <groupId>com.squareup.okio</groupId>
    <artifactId>okio</artifactId>
    <version>1.12.0</version>
</dependency>

```

or Gradle:

```java
compile 'com.squareup.okio:okio:1.12.0'

```



## PNG decoder


Decoding the chunks of a PNG file demonstrates Okio in practice.

```java
private static final ByteString PNG_HEADER = ByteString.decodeHex("89504e470d0a1a0a");

public void decodePng(InputStream in) throws IOException {
  try (BufferedSource pngSource = Okio.buffer(Okio.source(in))) {
    ByteString header = pngSource.readByteString(PNG_HEADER.size());
    if (!header.equals(PNG_HEADER)) {
      throw new IOException("Not a PNG.");
    }

    while (true) {
      Buffer chunk = new Buffer();

      // Each chunk is a length, type, data, and CRC offset.
      int length = pngSource.readInt();
      String type = pngSource.readUtf8(4);
      pngSource.readFully(chunk, length);
      int crc = pngSource.readInt();

      decodeChunk(type, chunk);
      if (type.equals("IEND")) break;
    }
  }
}

private void decodeChunk(String type, Buffer chunk) {
  if (type.equals("IHDR")) {
    int width = chunk.readInt();
    int height = chunk.readInt();
    System.out.printf("%08x: %s %d x %d%n", chunk.size(), type, width, height);
  } else {
    System.out.printf("%08x: %s%n", chunk.size(), type);
  }
}

```



## ByteStrings and Buffers


ByteStrings and Buffers

Okio is built around two types that pack a lot of capability into a straightforward API:

**ByteString** is an immutable sequence of bytes. For character data, String is fundamental. ByteString is String's long-lost brother, making it easy to treat binary data as a value. This class is ergonomic: it knows how to encode and decode itself as hex, base64, and UTF-8.

**Buffer** is a mutable sequence of bytes. Like ArrayList, you don't need to size your buffer in advance. You read and write buffers as a queue: write data to the end and read it from the front. There's no obligation to manage positions, limits, or capacities.

Internally, `ByteString` and `Buffer` do some clever things to save CPU and memory. If you encode a UTF-8 string as a ByteString, it caches a reference to that string so that if you decode it later, there's no work to do.

`Buffer` is implemented as a linked list of segments. When you move data from one buffer to another, it reassigns ownership of the segments rather than copying the data across. This approach is particularly helpful for multithreaded programs: a thread that talks to the network can exchange data with a worker thread without any copying or ceremony.

