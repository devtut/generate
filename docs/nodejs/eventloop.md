---
metaTitle: "Node.js - Eventloop"
description: "How the concept of event loop evolved."
---

# Eventloop


In this post we are going to discuss how the concept of Eventloop emerged and how it can be used for high performance servers and event driven applications like GUIs.



## How the concept of event loop evolved.


### Eventloop in pseudo code

An event loop is a loop that waits for events and then reacts to those events

```js
while true:
    wait for something to happen
    react to whatever happened

```

### Example of a single-threaded HTTP server with no event loop

```

   while true:
    socket = wait for the next TCP connection
    read the HTTP request headers from (socket)
    file_contents = fetch the requested file from disk
    write the HTTP response headers to (socket)
    write the (file_contents) to (socket)
    close(socket)

```

Here's a simple form of a HTTP server which is a single threaded but no event loop. The problem here is that it waits until each request is finished before starting to process the next one. If it takes a while to read the HTTP request headers or to fetch the file from disk, we should be able to start processing the next request while we wait for that to finish.

The most common solution is to make the program multi-threaded.

### Example of a multi-threaded HTTP server with no event loop

```js
function handle_connection(socket):
    read the HTTP request headers from (socket)
    file_contents = fetch the requested file from disk
    write the HTTP response headers to (socket)
    write the (file_contents) to (socket)
    close(socket)
while true:
    socket = wait for the next TCP connection
    spawn a new thread doing handle_connection(socket)

```

Now we have made our little HTTP server multi threaded. This way, we can immediately move on to the next request because the current request is running in a background thread. Many servers, including Apache, use this approach.

But it's not perfect. One limitation is that you can only spawn so many threads. For workloads where you have a huge number of connections, but each connection only requires attention every once in a while, the multi-threaded model won't perform very well. The solution for those cases is to use an event loop:

### Example of a HTTP server with event loop

```js
while true:
    event = wait for the next event to happen
    if (event.type == NEW_TCP_CONNECTION):
        conn = new Connection
        conn.socket = event.socket
        start reading HTTP request headers from (conn.socket) with userdata = (conn)
    else if (event.type == FINISHED_READING_FROM_SOCKET):
        conn = event.userdata
        start fetching the requested file from disk with userdata = (conn)
    else if (event.type == FINISHED_READING_FROM_DISK):
        conn = event.userdata
        conn.file_contents = the data we fetched from disk
        conn.current_state = "writing headers"
        start writing the HTTP response headers to (conn.socket) with userdata = (conn)
    else if (event.type == FINISHED_WRITING_TO_SOCKET):
        conn = event.userdata
        if (conn.current_state == "writing headers"):
            conn.current_state = "writing file contents"
            start writing (conn.file_contents) to (conn.socket) with userdata = (conn)
        else if (conn.current_state == "writing file contents"):
            close(conn.socket)

```

Hopefully this pseudocode is intelligible. Here's what's going on: We wait for things to happen. Whenever a new connection is created or an existing connection needs our attention, we go deal with it, then go back to waiting. That way, we perform well when there are many connections and each one only rarely requires attention.

In a real application (not pseudocode) running on Linux, the "wait for the next event to happen" part would be implemented by calling the poll() or epoll() system call. The "start reading/writing something to a socket" parts would be implemented by calling the recv() or send() system calls in non-blocking mode.

**Reference:**

[1]. "How does an event loop work?" [Online]. Available : [https://www.quora.com/How-does-an-event-loop-work](https://www.quora.com/How-does-an-event-loop-work)

