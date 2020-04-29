---
metaTitle: "ByteBuffer"
description: "Basic Usage - Creating a ByteBuffer, Basic Usage - Using DirectByteBuffer, Basic Usage - Write Data to the Buffer"
---

# ByteBuffer


The `ByteBuffer` class was introduced in java 1.4 to ease working on binary data. It's especially suited to use with primitive type data. It allows the creation, but also subsequent manipulation of a `byte[]`s on a higher abstraction level



## Basic Usage - Creating a ByteBuffer


There's two ways to create a `ByteBuffer`, where one can be subdivided again.

If you have an already existing `byte[]`, you can **"wrap"** it into a `ByteBuffer` to simplify processing:

```java
byte[] reqBuffer = new byte[BUFFER_SIZE];
int readBytes = socketInputStream.read(reqBuffer);
final ByteBuffer reqBufferWrapper = ByteBuffer.wrap(reqBuffer);

```

This would be a possibility for code that handles low-level networking interactions

If you do not have an already existing `byte[]`, you can create a `ByteBuffer` over an array that's specifically allocated for the buffer like this:

```java
final ByteBuffer respBuffer = ByteBuffer.allocate(RESPONSE_BUFFER_SIZE);
putResponseData(respBuffer);
socketOutputStream.write(respBuffer.array());

```

If the code-path is extremely performance critical and you need **direct system memory access**, the `ByteBuffer` can even allocate **direct** buffers using `#allocateDirect()`



## Basic Usage - Using DirectByteBuffer


`DirectByteBuffer` is special implementation of `ByteBuffer` that has no `byte[]` laying underneath.

We can allocate such ByteBuffer by calling:

```java
ByteBuffer directBuffer = ByteBuffer.allocateDirect(16);

```

This operation will allocate 16 bytes of memory. The contents of direct buffers **may** reside outside of the normal garbage-collected heap.

We can verify whether ByteBuffer is direct by calling:

```java
directBuffer.isDirect(); // true

```

The main characteristics of `DirectByteBuffer` is that JVM will try to natively work on allocated memory without any additional buffering so operations performed on it may be faster then those performed on ByteBuffers with arrays lying underneath.

It is recomended to use `DirectByteBuffer` with heavy IO operations that rely on speed of execution, like real time communication.

We have to be aware that if we try using `array()` method we will get `UnsupportedOperationException`. So it is a good practice to chech whether our ByteBuffer has it (byte array) before we try to access it:

```

byte[] arrayOfBytes;
 if(buffer.hasArray()) {
     arrayOfBytes = buffer.array();
 }

```

Another use of direct byte buffer is interop through JNI. Since a direct byte buffer does not use a `byte[]`, but an actual block of memory, it is possible to access that memory directly through a pointer in native code. This can save a bit of trouble and overhead on marshalling between the Java and native representation of data.

The JNI interface defines several functions to handle direct byte buffers: [NIO Support](http://docs.oracle.com/javase/8/docs/technotes/guides/jni/spec/functions.html#nio_support).



## Basic Usage - Write Data to the Buffer


Given a `ByteBuffer` instance one can write primitive-type data to it using **relative** and **absolute** `put`. The striking difference is that putting data using the **relative** method keeps track of the index the data is inserted at for you, while the absolute method always requires giving an index to `put` the data at.

Both methods allow **"chaining"** calls. Given a sufficiently sized buffer one can accordingly do the following:

```java
buffer.putInt(0xCAFEBABE).putChar('c').putFloat(0.25).putLong(0xDEADBEEFCAFEBABE);

```

which is equivalent to:

```java
buffer.putInt(0xCAFEBABE);
buffer.putChar('c');
buffer.putFloat(0.25);
buffer.putLong(0xDEADBEEFCAFEBABE);

```

Do note that the method operating on `byte`s is not named specially. Additionally note that it's also valid to pass both a `ByteBuffer` and a `byte[]` to `put`. Other than that, all primitive types have specialized `put`-methods.

An additional note: The index given when using absolute `put*` is always counted in `byte`s.



#### Syntax


- byte[] arr = new byte[1000];
- ByteBuffer buffer = ByteBuffer.wrap(arr);
- ByteBuffer buffer = ByteBuffer.allocate(1024);
- ByteBuffer buffer = ByteBuffer.allocateDirect(1024);
- byte b = buffer.get();
- byte b = buffer.get(10);
- short s = buffer.getShort(10);
- buffer.put((byte) 120);
- buffer.putChar('a');

