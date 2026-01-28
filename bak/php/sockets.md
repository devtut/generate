---
metaTitle: "PHP - Sockets"
description: "TCP client socket, TCP server socket, UDP server socket, Handling socket errors"
---

# Sockets



## TCP client socket


### Creating a socket that uses the TCP (Transmission Control Protocol)

```php
$socket = socket_create(AF_INET, SOCK_STREAM, SOL_TCP);

```

Make sure the socket is successfully created. The `onSocketFailure` function comes from [Handling socket errors](http://stackoverflow.com/documentation/php/6138/sockets/23034/handling-socket-errors) example in this topic.

```php
if(!is_resource($socket)) onSocketFailure("Failed to create socket");

```

### Connect the socket to a specified address

The second line fails gracefully if connection failed.

```php
socket_connect($socket, "chat.stackoverflow.com", 6667)
        or onSocketFailure("Failed to connect to chat.stackoverflow.com:6667", $socket);

```

### Sending data to the server

The `socket_write` function sends bytes through a socket. In PHP, a byte array is represented by a string, which is normally encoding-insensitive.

```php
socket_write($socket, "NICK Alice\r\nUSER alice 0 * :Alice\r\n");

```

### Receiving data from the server

The following snippet receives some data from the server using the `socket_read` function.

Passing `PHP_NORMAL_READ` as the third parameter reads until a `\r`/`\n` byte, and this byte is included in the return value.

Passing `PHP_BINARY_READ`, on the contrary, reads the required amount of data from the stream.

If `socket_set_nonblock` was called in prior, and `PHP_BINARY_READ` is used, `socket_read` will return `false` immediately. Otherwise, the method blocks until enough data (to reach the length in the second parameter, or to reach a line ending) are received, or the socket is closed.

This example reads data from a supposedly IRC server.

```php
while(true) {
    // read a line from the socket
    $line = socket_read($socket, 1024, PHP_NORMAL_READ);
    if(substr($line, -1) === "\r") {
        // read/skip one byte from the socket
        // we assume that the next byte in the stream must be a \n.
        // this is actually bad in practice; the script is vulnerable to unexpected values
        socket_read($socket, 1, PHP_BINARY_READ);
    }

    $message = parseLine($line);
    if($message->type === "QUIT") break;
}

```

### Closing the socket

Closing the socket frees the socket and its associated resources.

```php
socket_close($socket);

```



## TCP server socket


### Socket creation

Create a socket that uses the TCP. It is the same as creating a client socket.

```php
$socket = socket_create(AF_INET, SOCK_STREAM, SOL_TCP);

```

### Socket binding

Bind connections from a given network (parameter 2) for a specific port (parameter 3) to the socket.

The second parameter is usually `"0.0.0.0"`, which accepts connection from all networks. It can also

One common cause of errors from `socket_bind` is that [the address specified is already bound to another process](https://www.google.com.hk/search?q=site%3Astackexchange.com%20OR%20site%3Astackoverflow.com%20kill%20processes%20bound%20to%20address). Other processes are usually killed (usually manually to prevent accidentally killing critical processes) so that the sockets would be freed.

```php
socket_bind($socket, "0.0.0.0", 6667) or onSocketFailure("Failed to bind to 0.0.0.0:6667");

```

### Set a socket to listening

Make the socket listen to incoming connections using `socket_listen`. The second parameter is the maximum number of connections to allow queuing before they are accepted.

```php
socket_listen($socket, 5);

```

### Handling connection

A TCP server is actually a server that handles child connections. `socket_accept` creates a new child connection.

```php
$conn = socket_accept($socket);

```

Data transferring for a connection from `socket_accept` is the same as that for a [TCP client socket](http://stackoverflow.com/documentation/php/6138/sockets/23032/tcp-client-socket).

When this connection should be closed, call `socket_close($conn);` directly. This will not affect the original TCP server socket.

### Closing the server

On the other hand, `socket_close($socket);` should be called when the server is no longer used. This will free the TCP address as well, allowing other processes to bind to the address.



## UDP server socket


A UDP (user datagram protocol) server, unlike TCP, is not stream-based. It is packet-based, i.e. a client sends data in units called "packets" to the server, and the client identifies clients by their address. There is no builtin function that relates different packets sent from the same client (unlike TCP, where data from the same client are handled by a specific resource created by `socket_accept`). It can be thought as a new TCP connection is accepted and closed every time a UDP packet arrives.

### Creating a UDP server socket

```php
$socket = socket_create(AF_INET, SOCK_DGRAM, SOL_UDP);

```

### Binding a socket to an address

The parameters are same as that for a TCP server.

```php
socket_bind($socket, "0.0.0.0", 9000) or onSocketFailure("Failed to bind to 0.0.0.0:9000", $socket);

```

### Sending a packet

This line sends `$data` in a UDP packet to `$address`:`$port`.

```php
socket_sendto($socket, $data, strlen($data), 0, $address, $port);

```

### Receiving a packet

The following snippet attempts to manage UDP packets in a client-indexed manner.

```php
$clients = [];
while (true){
    socket_recvfrom($socket, $buffer, 32768, 0, $ip, $port) === true
            or onSocketFailure("Failed to receive packet", $socket);
    $address = "$ip:$port";
    if (!isset($clients[$address])) $clients[$address] = new Client();
    $clients[$address]->handlePacket($buffer);
}

```

### Closing the server

`socket_close` can be used on the UDP server socket resource. This will free the UDP address, allowing other processes to bind to this address.



## Handling socket errors


`socket_last_error` can be used to get the error ID of the last error from the sockets extension.

`socket_strerror` can be used to convert the ID to human-readable strings.

```php
function onSocketFailure(string $message, $socket = null) {
    if(is_resource($socket)) {
        $message .= ": " . socket_strerror(socket_last_error($socket));
    }
    die($message);
}

```

