---
metaTitle: "PHP - Asynchronous programming"
description: "Advantages of Generators, Using Icicle event loop, Spawning non-blocking processes with proc_open(), Using Amp event loop, Reading serial port with Event and DIO, HTTP Client Based on Event Extension, HTTP Client Based on Ev Extension"
---

# Asynchronous programming



## Advantages of Generators


PHP 5.5 introduces Generators and the yield keyword, which allows us to write asynchronous code that looks more like synchronous code.

The `yield` expression is responsible for giving control back to the calling code and providing a point of resumption at that place. One can send a value along the `yield` instruction. The return value of this expression is either `null` or the value which was passed to `Generator::send()`.

```php
function reverse_range($i) {
    // the mere presence of the yield keyword in this function makes this a Generator
    do {
        // $i is retained between resumptions
        print yield $i;
    } while (--$i > 0);
}

$gen = reverse_range(5);
print $gen->current();
$gen->send("injected!"); // send also resumes the Generator

foreach ($gen as $val) { // loops over the Generator, resuming it upon each iteration
    echo $val;
}

// Output: 5injected!4321

```

This mechanism can be used by a coroutine implementation to wait for Awaitables yielded by the Generator (by registering itself as a callback for resolution) and continue execution of the Generator as soon as the Awaitable is resolved.



## Using Icicle event loop


[Icicle](https://github.com/icicleio/icicle) uses Awaitables and Generators to create Coroutines.

```php
require __DIR__ . '/vendor/autoload.php';

use Icicle\Awaitable;
use Icicle\Coroutine\Coroutine;
use Icicle\Loop;

$generator = function (float $time) {
    try {
        // Sets $start to the value returned by microtime() after approx. $time seconds.
        $start = yield Awaitable\resolve(microtime(true))->delay($time);

        echo "Sleep time: ", microtime(true) - $start, "\n";

        // Throws the exception from the rejected awaitable into the coroutine.
        return yield Awaitable\reject(new Exception('Rejected awaitable'));
    } catch (Throwable $e) { // Catches awaitable rejection reason.
        echo "Caught exception: ", $e->getMessage(), "\n";
    }

    return yield Awaitable\resolve('Coroutine completed');
};

// Coroutine sleeps for 1.2 seconds, then will resolve with a string.
$coroutine = new Coroutine($generator(1.2));
$coroutine->done(function (string $data) {
    echo $data, "\n";
});

Loop\run();

```



## Spawning non-blocking processes with proc_open()


PHP has no support for running code concurrently unless you install extensions such as [`pthread`](http://stackoverflow.com/documentation/php/1583/multi-threading-extension#t=201609251928593249974). This can be sometimes bypassed by using [`proc_open()`](http://php.net/manual/en/function.proc-open.php) and [`stream_set_blocking()`](http://php.net/manual/en/function.stream-set-blocking.php) and reading their output asynchronously.

If we split code into smaller chunks we can run it as multiple suprocesses. Then using [`stream_set_blocking()`](http://php.net/manual/en/function.stream-set-blocking.php) function we can make each subprocess also non-blocking. This means we can spawn multiple subprocesses and then check for their output in a loop (similarly to an even loop) and wait until all of them finish.

As an example we can have a small subprocess that just runs a loop and in each iteration sleeps randomly for 100 - 1000ms (note, the delay is always the same for one subprocess).

```php
<?php
// subprocess.php
$name = $argv[1];
$delay = rand(1, 10) * 100;
printf("$name delay: ${delay}ms\n");

for ($i = 0; $i < 5; $i++) {
    usleep($delay * 1000);
    printf("$name: $i\n");
}

```

Then the main process will spawn subprocesses and read their output. We can split it into smaller blocks:

- Spawn subprocesses with [proc_open()](http://php.net/manual/en/function.proc-open.php) .
- Make each subprocess non-blocking with [`stream_set_blocking()`](http://php.net/manual/en/function.stream-set-blocking.php).
- Run a loop until all subprocesses finish using [`proc_get_status()`](http://php.net/manual/en/function.proc-get-status.php).
- Properly close file handles with the output pipe for each subprocess using [`fclose()`](http://php.net/manual/en/function.fclose.php) and close process handles with [`proc_close()`](http://php.net/manual/en/function.proc-close.php).

```php
<?php
// non-blocking-proc_open.php
// File descriptors for each subprocess.
$descriptors = [
    0 => ['pipe', 'r'], // stdin
    1 => ['pipe', 'w'], // stdout
];

$pipes = [];
$processes = [];
foreach (range(1, 3) as $i) {
    // Spawn a subprocess.
    $proc = proc_open('php subprocess.php proc' . $i, $descriptors, $procPipes);
    $processes[$i] = $proc;
    // Make the subprocess non-blocking (only output pipe).
    stream_set_blocking($procPipes[1], 0);
    $pipes[$i] = $procPipes;
}

// Run in a loop until all subprocesses finish.
while (array_filter($processes, function($proc) { return proc_get_status($proc)['running']; })) {
    foreach (range(1, 3) as $i) {
        usleep(10 * 1000); // 100ms
        // Read all available output (unread output is buffered).
        $str = fread($pipes[$i][1], 1024);
        if ($str) {
            printf($str);
        }
    }
}

// Close all pipes and processes.
foreach (range(1, 3) as $i) {
    fclose($pipes[$i][1]);
    proc_close($processes[$i]);
}

```

The output then contains mixture from all three subprocesses as they we're read by [fread()](http://php.net/manual/en/function.fread.php) (note, that in this case `proc1` ended much earlier than the other two):

```php
$ php non-blocking-proc_open.php 
proc1 delay: 200ms
proc2 delay: 1000ms
proc3 delay: 800ms
proc1: 0
proc1: 1
proc1: 2
proc1: 3
proc3: 0
proc1: 4
proc2: 0
proc3: 1
proc2: 1
proc3: 2
proc2: 2
proc3: 3
proc2: 3
proc3: 4
proc2: 4

```



## Using Amp event loop


[Amp](https://github.com/amphp/amp/tree/v1.x) harnesses Promises [another name for Awaitables] and Generators for coroutine creation.

```php
require __DIR__ . '/vendor/autoload.php';

use Amp\Dns;

// Try our system defined resolver or googles, whichever is fastest
function queryStackOverflow($recordtype) {
    $requests = [
        Dns\query("stackoverflow.com", $recordtype),
        Dns\query("stackoverflow.com", $recordtype, ["server" => "8.8.8.8"]),
    ];
    // returns a Promise resolving when the first one of the requests resolves
    return yield Amp\first($request);
}

\Amp\run(function() { // main loop, implicitly a coroutine
    try {
        // convert to coroutine with Amp\resolve()
        $promise = Amp\resolve(queryStackOverflow(Dns\Record::NS));
        list($ns, $type, $ttl) = // we need only one NS result, not all
            current(yield Amp\timeout($promise, 2000 /* milliseconds */));
        echo "The result of the fastest server to reply to our query was $ns";
    } catch (Amp\TimeoutException $e) {
        echo "We've heard no answer for 2 seconds! Bye!";
    } catch (Dns\NoRecordException $e) {
        echo "No NS records there? Stupid DNS nameserver!";
    }
});

```



## Reading serial port with Event and DIO


[**DIO**](http://php.net/manual/en/book.dio.php) streams are currently not recognized by the [**Event**](http://php.net/manual/en/book.event.php) extension. There is no clean way to obtain the file descriptor encapsulated into the DIO resource. But there is a workaround:

- open stream for the port with `fopen()`;
- make the stream non-blocking with [`stream_set_blocking()`](http://php.net/manual/en/function.stream-set-blocking.php);
- obtain numeric file descriptor from the stream with [`EventUtil::getSocketFd()`](http://php.net/manual/en/function.stream-set-blocking.php);
- pass the numeric file descriptor to `dio_fdopen()` (currently undocumented) and get the DIO resource;
- add an `Event` with a callback for listening to the read events on the file descriptor;
- in the callback drain the available data and process it according to the logic of your application.

**dio.php**

```php
<?php
class Scanner {
  protected $port; // port path, e.g. /dev/pts/5
  protected $fd; // numeric file descriptor
  protected $base; // EventBase
  protected $dio; // dio resource
  protected $e_open; // Event
  protected $e_read; // Event

  public function __construct ($port) {
    $this->port = $port;
    $this->base = new EventBase();
  }

  public function __destruct() {
    $this->base->exit();

    if ($this->e_open)
      $this->e_open->free();
    if ($this->e_read)
      $this->e_read->free();
    if ($this->dio)
      dio_close($this->dio);
  }

  public function run() {
    $stream = fopen($this->port, 'rb');
    stream_set_blocking($stream, false);

    $this->fd = EventUtil::getSocketFd($stream);
    if ($this->fd < 0) {
      fprintf(STDERR, "Failed attach to port, events: %d\n", $events);
      return;
    }

    $this->e_open = new Event($this->base, $this->fd, Event::WRITE, [$this, '_onOpen']);
    $this->e_open->add();
    $this->base->dispatch();

    fclose($stream);
  }

  public function _onOpen($fd, $events) {
    $this->e_open->del();

    $this->dio = dio_fdopen($this->fd);
    // Call other dio functions here, e.g.
    dio_tcsetattr($this->dio, [
      'baud' => 9600,
      'bits' => 8,
      'stop'  => 1,
      'parity' => 0
    ]);

    $this->e_read = new Event($this->base, $this->fd, Event::READ | Event::PERSIST,
      [$this, '_onRead']);
    $this->e_read->add();
  }

  public function _onRead($fd, $events) {
    while ($data = dio_read($this->dio, 1)) {
      var_dump($data);
    }
  }
}

// Change the port argument
$scanner = new Scanner('/dev/pts/5');
$scanner->run();

```

### Testing

Run the following command in terminal A:

```php
$ socat -d -d pty,raw,echo=0 pty,raw,echo=0
2016/12/01 18:04:06 socat[16750] N PTY is /dev/pts/5
2016/12/01 18:04:06 socat[16750] N PTY is /dev/pts/8
2016/12/01 18:04:06 socat[16750] N starting data transfer loop with FDs [5,5] and [7,7]

```

The output may be different. Use the PTYs from the first couple of rows (`/dev/pts/5` and `/dev/pts/8`, in particular).

In terminal B run the above-mentioned script. You may need root privileges:

```php
$ sudo php dio.php

```

In terminal C send a string to the first PTY:

```php
$ echo test > /dev/pts/8

```

**Output**

```php
string(1) "t"
string(1) "e"
string(1) "s"
string(1) "t"
string(1) "
"

```



## HTTP Client Based on Event Extension


This is a sample HTTP client class based on [Event](https://pecl.php.net/package/event) extension.

The class allows to schedule a number of HTTP requests, then run them asynchronously.

### http-client.php

```php
<?php
class MyHttpClient {
  /// @var EventBase
  protected $base;
  /// @var array Instances of EventHttpConnection
  protected $connections = [];

  public function __construct() {
    $this->base = new EventBase();
  }

  /**
   * Dispatches all pending requests (events)
   *
   * @return void
   */
  public function run() {
    $this->base->dispatch();
  }

  public function __destruct() {
    // Destroy connection objects explicitly, don't wait for GC.
    // Otherwise, EventBase may be free'd earlier.
    $this->connections = null;
  }

  /**
   * @brief Adds a pending HTTP request
   *
   * @param string $address Hostname, or IP
   * @param int $port Port number
   * @param array $headers Extra HTTP headers
   * @param int $cmd A EventHttpRequest::CMD_* constant
   * @param string $resource HTTP request resource, e.g. '/page?a=b&c=d'
   *
   * @return EventHttpRequest|false
   */
  public function addRequest($address, $port, array $headers,
    $cmd = EventHttpRequest::CMD_GET, $resource = '/')
  {
    $conn = new EventHttpConnection($this->base, null, $address, $port);
    $conn->setTimeout(5);

    $req = new EventHttpRequest([$this, '_requestHandler'], $this->base);

    foreach ($headers as $k => $v) {
      $req->addHeader($k, $v, EventHttpRequest::OUTPUT_HEADER);
    }
    $req->addHeader('Host', $address, EventHttpRequest::OUTPUT_HEADER);
    $req->addHeader('Connection', 'close', EventHttpRequest::OUTPUT_HEADER);
    if ($conn->makeRequest($req, $cmd, $resource)) {
      $this->connections []= $conn;
      return $req;
    }

    return false;
  }


  /**
   * @brief Handles an HTTP request
   *
   * @param EventHttpRequest $req
   * @param mixed $unused
   *
   * @return void
   */
  public function _requestHandler($req, $unused) {
    if (is_null($req)) {
      echo "Timed out\n";
    } else {
      $response_code = $req->getResponseCode();

      if ($response_code == 0) {
        echo "Connection refused\n";
      } elseif ($response_code != 200) {
        echo "Unexpected response: $response_code\n";
      } else {
        echo "Success: $response_code\n";
        $buf = $req->getInputBuffer();
        echo "Body:\n";
        while ($s = $buf->readLine(EventBuffer::EOL_ANY)) {
          echo $s, PHP_EOL;
        }
      }
    }
  }
}


$address = "my-host.local";
$port = 80;
$headers = [ 'User-Agent' => 'My-User-Agent/1.0', ];

$client = new MyHttpClient();

// Add pending requests
for ($i = 0; $i < 10; $i++) {
  $client->addRequest($address, $port, $headers,
    EventHttpRequest::CMD_GET, '/test.php?a=' . $i);
}

// Dispatch pending requests
$client->run();

```

### test.php

This is a sample script on the server side.

```php
<?php
echo 'GET: ', var_export($_GET, true), PHP_EOL;
echo 'User-Agent: ', $_SERVER['HTTP_USER_AGENT'] ?? '(none)', PHP_EOL;

```

### Usage

```php
php http-client.php

```

**Sample Output**

```php
Success: 200
Body:
GET: array (
  'a' => '1',
)
User-Agent: My-User-Agent/1.0
Success: 200
Body:
GET: array (
  'a' => '0',
)
User-Agent: My-User-Agent/1.0
Success: 200
Body:
GET: array (
  'a' => '3',
)
...

```

**(Trimmed.)**

Note, the code is designed for long-term processing in the [CLI SAPI](http://php.net/manual/en/features.commandline.introduction.php).



## HTTP Client Based on Ev Extension


This is a sample HTTP client based on [Ev](https://pecl.php.net/package/ev) extension.

Ev extension implements a simple yet powerful general purpose event loop. It doesn't provide network-specific watchers, but its [I/O watcher](http://docs.php.net/manual/en/class.evio.php) can be used for asynchronous processing of [sockets](http://docs.php.net/manual/en/book.sockets.php).

The following code shows how HTTP requests can be scheduled for parallel processing.

### http-client.php

```php
<?php
class MyHttpRequest {
  /// @var MyHttpClient
  private $http_client;
  /// @var string
  private $address;
  /// @var string HTTP resource such as /page?get=param
  private $resource;
  /// @var string HTTP method such as GET, POST etc.
  private $method;
  /// @var int
  private $service_port;
  /// @var resource Socket
  private $socket;
  /// @var double Connection timeout in seconds.
  private $timeout = 10.;
  /// @var int Chunk size in bytes for socket_recv()
  private $chunk_size = 20;
  /// @var EvTimer
  private $timeout_watcher;
  /// @var EvIo
  private $write_watcher;
  /// @var EvIo
  private $read_watcher;
  /// @var EvTimer
  private $conn_watcher;
  /// @var string buffer for incoming data
  private $buffer;
  /// @var array errors reported by sockets extension in non-blocking mode.
  private static $e_nonblocking = [
    11, // EAGAIN or EWOULDBLOCK
    115, // EINPROGRESS
  ];

  /**
   * @param MyHttpClient $client
   * @param string $host Hostname, e.g. google.co.uk
   * @param string $resource HTTP resource, e.g. /page?a=b&c=d
   * @param string $method HTTP method: GET, HEAD, POST, PUT etc.
   * @throws RuntimeException
   */
  public function __construct(MyHttpClient $client, $host, $resource, $method) {
    $this->http_client = $client;
    $this->host        = $host;
    $this->resource    = $resource;
    $this->method      = $method;

    // Get the port for the WWW service
    $this->service_port = getservbyname('www', 'tcp');

    // Get the IP address for the target host
    $this->address = gethostbyname($this->host);

    // Create a TCP/IP socket
    $this->socket = socket_create(AF_INET, SOCK_STREAM, SOL_TCP);
    if (!$this->socket) {
      throw new RuntimeException("socket_create() failed: reason: " .
        socket_strerror(socket_last_error()));
    }

    // Set O_NONBLOCK flag
    socket_set_nonblock($this->socket);

    $this->conn_watcher = $this->http_client->getLoop()
      ->timer(0, 0., [$this, 'connect']);
  }

  public function __destruct() {
    $this->close();
  }

  private function freeWatcher(&$w) {
    if ($w) {
      $w->stop();
      $w = null;
    }
  }

  /**
   * Deallocates all resources of the request
   */
  private function close() {
    if ($this->socket) {
      socket_close($this->socket);
      $this->socket = null;
    }

    $this->freeWatcher($this->timeout_watcher);
    $this->freeWatcher($this->read_watcher);
    $this->freeWatcher($this->write_watcher);
    $this->freeWatcher($this->conn_watcher);
  }

  /**
   * Initializes a connection on socket
   * @return bool
   */
  public function connect() {
    $loop = $this->http_client->getLoop();

    $this->timeout_watcher = $loop->timer($this->timeout, 0., [$this, '_onTimeout']);
    $this->write_watcher = $loop->io($this->socket, Ev::WRITE, [$this, '_onWritable']);

    return socket_connect($this->socket, $this->address, $this->service_port);
  }

  /**
   * Callback for timeout (EvTimer) watcher
   */
  public function _onTimeout(EvTimer $w) {
    $w->stop();
    $this->close();
  }

  /**
   * Callback which is called when the socket becomes wriable
   */
  public function _onWritable(EvIo $w) {
    $this->timeout_watcher->stop();
    $w->stop();

    $in = implode("\r\n", [
      "{$this->method} {$this->resource} HTTP/1.1",
      "Host: {$this->host}",
      'Connection: Close',
    ]) . "\r\n\r\n";

    if (!socket_write($this->socket, $in, strlen($in))) {
      trigger_error("Failed writing $in to socket", E_USER_ERROR);
      return;
    }

    $loop = $this->http_client->getLoop();
    $this->read_watcher = $loop->io($this->socket,
      Ev::READ, [$this, '_onReadable']);

    // Continue running the loop
    $loop->run();
  }

  /**
   * Callback which is called when the socket becomes readable
   */
  public function _onReadable(EvIo $w) {
    // recv() 20 bytes in non-blocking mode
    $ret = socket_recv($this->socket, $out, 20, MSG_DONTWAIT);

    if ($ret) {
      // Still have data to read. Append the read chunk to the buffer.
      $this->buffer .= $out;
    } elseif ($ret === 0) {
      // All is read
      printf("\n<<<<\n%s\n>>>>", rtrim($this->buffer));
      fflush(STDOUT);
      $w->stop();
      $this->close();
      return;
    }

    // Caught EINPROGRESS, EAGAIN, or EWOULDBLOCK
    if (in_array(socket_last_error(), static::$e_nonblocking)) {
      return;
    }

    $w->stop();
    $this->close();
  }
}

/////////////////////////////////////
class MyHttpClient {
  /// @var array Instances of MyHttpRequest
  private $requests = [];
  /// @var EvLoop
  private $loop;

  public function __construct() {
    // Each HTTP client runs its own event loop
    $this->loop = new EvLoop();
  }

  public function __destruct() {
    $this->loop->stop();
  }

  /**
   * @return EvLoop
   */
  public function getLoop() {
    return $this->loop;
  }

  /**
   * Adds a pending request
   */
  public function addRequest(MyHttpRequest $r) {
    $this->requests []= $r;
  }

  /**
   * Dispatches all pending requests
   */
  public function run() {
    $this->loop->run();
  }
}


/////////////////////////////////////
// Usage
$client = new MyHttpClient();
foreach (range(1, 10) as $i) {
  $client->addRequest(new MyHttpRequest($client, 'my-host.local', '/test.php?a=' . $i, 'GET'));
}
$client->run();

```

### Testing

Suppose `http://my-host.local/test.php` script is printing the dump of `$_GET`:

```php
<?php
echo 'GET: ', var_export($_GET, true), PHP_EOL;

```

Then the output of `php http-client.php` command will be similar to the following:

```php
<<<<
HTTP/1.1 200 OK
Server: nginx/1.10.1
Date: Fri, 02 Dec 2016 12:39:54 GMT
Content-Type: text/html; charset=UTF-8
Transfer-Encoding: chunked
Connection: close
X-Powered-By: PHP/7.0.13-pl0-gentoo

1d
GET: array (
  'a' => '3',
)

0
>>>>
<<<<
HTTP/1.1 200 OK
Server: nginx/1.10.1
Date: Fri, 02 Dec 2016 12:39:54 GMT
Content-Type: text/html; charset=UTF-8
Transfer-Encoding: chunked
Connection: close
X-Powered-By: PHP/7.0.13-pl0-gentoo

1d
GET: array (
  'a' => '2',
)

0
>>>>
...

```

**(trimmed)**

Note, in PHP 5 the **sockets** extension may log warnings for `EINPROGRESS`, `EAGAIN`, and `EWOULDBLOCK` `errno` values. It is possible to turn off the logs with

```php
error_reporting(E_ERROR);

```

