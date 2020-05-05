---
metaTitle: "Java - NIO - Networking"
description: "Using Selector to wait for events (example with OP_CONNECT)"
---

# NIO - Networking



## Using Selector to wait for events (example with OP_CONNECT)


NIO appeared in Java 1.4 and introduced the concept of "Channels", which are supposed to be faster than regular I/O. Network-wise, the [`SelectableChannel`](https://docs.oracle.com/javase/7/docs/api/java/nio/channels/SelectableChannel.html) is the most interesting as it allows to monitor different states of the Channel. It works in a similar manner as the C `select()` system call: we get woken-up when certain types of events occur:

- connection received (`OP_ACCEPT`)
- connection realized (`OP_CONNECT`)
- data available in read FIFO (`OP_READ`)
- data can be pushed to write FIFO (`OP_WRITE`)

It allows for separation between **detecting** socket I/O (something can be read/written/...) and **performing** the I/O (read/write/...). Especially, all I/O detection can be done in a single thread for multiple sockets (clients), while performing I/O can be handled in a thread pool or anywhere else. That allows for an application to scale easily to the number of connected clients.

The following example shows the basics:

1. Create a [`Selector`](https://docs.oracle.com/javase/7/docs/api/java/nio/channels/Selector.html)
1. Create a [`SocketChannel`](https://docs.oracle.com/javase/7/docs/api/java/nio/channels/SocketChannel.html)
1. Register the `SocketChannel`to the `Selector`
1. Loop with the `Selector` to detect events

```java
Selector sel = Selector.open(); // Create the Selector
SocketChannel sc = SocketChannel.open(); // Create a SocketChannel
sc.configureBlocking(false); // ... non blocking
sc.setOption(StandardSocketOptions.SO_KEEPALIVE, true); // ... set some options

// Register the Channel to the Selector for wake-up on CONNECT event and use some description as an attachement
sc.register(sel, SelectionKey.OP_CONNECT, "Connection to google.com"); // Returns a SelectionKey: the association between the SocketChannel and the Selector
System.out.println("Initiating connection");
if (sc.connect(new InetSocketAddress("www.google.com", 80)))
    System.out.println("Connected"); // Connected right-away: nothing else to do
else {
    boolean exit = false;
    while (!exit) {
        if (sel.select(100) == 0) // Did something happen on some registered Channels during the last 100ms?
            continue; // No, wait some more
        
        // Something happened...
        Set<SelectionKey> keys = sel.selectedKeys(); // List of SelectionKeys on which some registered operation was triggered
        for (SelectionKey k : keys) {
            System.out.println("Checking "+k.attachment());
            if (k.isConnectable()) { // CONNECT event
                System.out.print("Connected through select() on "+k.channel()+" -> ");
                if (sc.finishConnect()) { // Finish connection process
                    System.out.println("done!");
                    k.interestOps(k.interestOps() & ~SelectionKey.OP_CONNECT); // We are already connected: remove interest in CONNECT event
                    exit = true;
                } else
                    System.out.println("unfinished...");
            }
            // TODO: else if (k.isReadable()) { ...
        }
        keys.clear(); // Have to clear the selected keys set once processed!
    }
}
System.out.print("Disconnecting ... ");
sc.shutdownOutput(); // Initiate graceful disconnection
// TODO: emtpy receive buffer
sc.close();
System.out.println("done");

```

Would give the following output:

```java
Initiating connection
Checking Connection to google.com
Connected through 'select()' on java.nio.channels.SocketChannel[connection-pending remote=www.google.com/216.58.208.228:80] -> done!
Disconnecting ... done

```



#### Remarks


[`SelectionKey`](https://docs.oracle.com/javase/7/docs/api/java/nio/channels/SelectionKey.html) defines the different selectable operations and information between its [Selector](https://docs.oracle.com/javase/7/docs/api/java/nio/channels/SelectionKey.html#selector()) and [Channel](https://docs.oracle.com/javase/7/docs/api/java/nio/channels/SelectionKey.html#channel()). In particular, the [attachment](https://docs.oracle.com/javase/7/docs/api/java/nio/channels/SelectionKey.html#attachment()) can be used to store connection-related information.

Handling `OP_READ` is pretty straight-forward. However, care should be taken when dealing with `OP_WRITE`: most of the time, data can be written to sockets so the event will keep firing. Make sure to register `OP_WRITE` only before you want to write data (see [that answer](http://stackoverflow.com/a/6646131/1098603)).

Also, `OP_CONNECT` should be cancelled once the Channel has connected (because, well, it **is** connected. See [this](http://stackoverflow.com/a/9326318/1098603) and [that](http://stackoverflow.com/a/205354/1098603) answers on SO). Hence the `OP_CONNECT` removal after `finishConnect()` succeeded.

