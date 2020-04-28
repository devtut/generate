---
metaTitle: "Streams"
description: "Registering a stream wrapper"
---

# Streams



## Registering a stream wrapper


A stream wrapper provides a handler for one or more specific schemes.

The example below shows a simple stream wrapper that sends `PATCH` HTTP requests when the stream is closed.

```
// register the FooWrapper class as a wrapper for foo:// URLs.
stream_wrapper_register("foo", FooWrapper::class, STREAM_IS_URL) or die("Duplicate stream wrapper registered");

class FooWrapper {
    // this will be modified by PHP to show the context passed in the current call.
    public $context;

    // this is used in this example internally to store the URL
    private $url;

    // when fopen() with a protocol for this wrapper is called, this method can be implemented to store data like the host.
    public function stream_open(string $path, string $mode, int $options, string &$openedPath) : bool {
        $url = parse_url($path);
        if($url === false) return false;
        $this->url = $url["host"] . "/" . $url["path"];
        return true;
    }

    // handles calls to fwrite() on this stream
    public function stream_write(string $data) : int {
        $this->buffer .= $data;
        return strlen($data);
    }

    // handles calls to fclose() on this stream
    public function stream_close() {
        $curl = curl_init("http://" . $this->url);
        curl_setopt($curl, CURLOPT_POSTFIELDS, $this->buffer);
        curl_setopt($curl, CURLOPT_CUSTOMREQUEST, "PATCH");
        curl_exec($curl);
        curl_close($curl);
        $this->buffer = "";
    }

    // fallback exception handler if an unsupported operation is attempted.
    // this is not necessary.
    public function __call($name, $args) {
        throw new \RuntimeException("This wrapper does not support $name");
    }

    // this is called when unlink("foo://something-else") is called.
    public function unlink(string $path) {
        $url = parse_url($path);
        $curl = curl_init("http://" . $url["host"] . "/" . $url["path"]);
        curl_setopt($curl, CURLOPT_CUSTOMREQUEST, "DELETE");
        curl_exec($curl);
        curl_close($curl);
    }
}

```

This example only shows some examples of what a generic stream wrapper would contain. These are not all methods available. A full list of methods that can be implemented can be found at [http://php.net/streamWrapper](http://php.net/streamWrapper).



#### Syntax


- Every stream has a scheme and a target:
- <scheme>://<target>



#### Parameters


|Parameter Name|Description
|------
|Stream Resource|The data provider consisting of the `<scheme>://<target>` syntax



#### Remarks


Streams are essentially a transfer of data between an origin and a destination, to paraphrase Josh Lockhart in his book Modern PHP.

The origin and the destination can be

- a file
- a command-line process
- a network connection
- a ZIP or TAR archive
- temporary memory
- standard input/output

or any other resource available via [PHP's stream wrappers](http://php.net/manual/wrappers.php).

Examples of available stream wrappers (`schemes`):

- file:// — Accessing local filesystem
- http:// — Accessing HTTP(s) URLs
- ftp:// — Accessing FTP(s) URLs
- php:// — Accessing various I/O streams
- phar:// — PHP Archive
- ssh2:// — Secure Shell 2
- ogg:// — Audio streams

The scheme (origin) is the identifier of the stream's wrapper. For example, for the file system this is `file://`. The target is the stream's data source, for example the file name.

