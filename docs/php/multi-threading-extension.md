---
metaTitle: "Multi Threading Extension"
description: "Getting Started, Using Pools and Workers"
---

# Multi Threading Extension



## Getting Started


To start with multi-threading, you would need the `pthreads-ext` for php, which can be installed by

```php
$ pecl install pthreads

```

and adding the entry to `php.ini`.

A simple example:

```php
<?php
// NOTE: Code uses PHP7 semantics.
class MyThread extends Thread {
    /**
     * @var string
     * Variable to contain the message to be displayed.
     */
    private $message;
    
    public function __construct(string $message) {
        // Set the message value for this particular instance.
        $this->message = $message;
    }

    // The operations performed in this function is executed in the other thread.
    public function run() {
        echo $this->message;
    }
}

// Instantiate MyThread
$myThread = new MyThread("Hello from an another thread!");
// Start the thread. Also it is always a good practice to join the thread explicitly.
// Thread::start() is used to initiate the thread,
$myThread->start();
// and Thread::join() causes the context to wait for the thread to finish executing
$myThread->join();

```



## Using Pools and Workers


> 
Pooling provides a higher level abstraction of the Worker functionality, including the management of references in the way required by pthreads. From: [http://php.net/manual/en/class.pool.php](http://php.net/manual/en/class.pool.php)


Pools and workers provide an higher level of control and ease of creating multi-threaded

```php
<?php
// This is the *Work* which would be ran by the worker.
// The work which you'd want to do in your worker.
// This class needs to extend the \Threaded or \Collectable or \Thread class.
class AwesomeWork extends Thread {
    private $workName;

    /**
     * @param string $workName
     * The work name wich would be given to every work.
     */
    public function __construct(string $workName) {
        // The block of code in the constructor of your work,
        // would be executed when a work is submitted to your pool.

        $this->workName = $workName;
        printf("A new work was submitted with the name: %s\n", $workName);
    }

    public function run() {
        // This block of code in, the method, run
        // would be called by your worker.
        // All the code in this method will be executed in another thread.
        $workName = $this->workName;
        printf("Work named %s starting...\n", $workName);
        printf("New random number: %d\n", mt_rand());
    }
}

// Create an empty worker for the sake of simplicity.
class AwesomeWorker extends Worker {
    public function run() {
        // You can put some code in here, which would be executed
        // before the Work's are started (the block of code in the `run` method of your Work)
        // by the Worker.
        /* ... */
    }
}

// Create a new Pool Instance.
// The ctor of \Pool accepts two parameters.
// First: The maximum number of workers your pool can create.
// Second: The name of worker class.
$pool = new \Pool(1, \AwesomeWorker::class);

// You need to submit your jobs, rather the instance of
// the objects (works) which extends the \Threaded class.
$pool->submit(new \AwesomeWork("DeadlyWork"));
$pool->submit(new \AwesomeWork("FatalWork"));

// We need to explicitly shutdown the pool, otherwise,
// unexpected things may happen.
// See: http://stackoverflow.com/a/23600861/23602185
$pool->shutdown();

```



#### Remarks


> 
With `pthreads` v3 `pthreads` can only be loaded when using the `cli` SAPI, thus it is a good practice to keep the `extension=pthreads.so` directive in `php-cli.ini` ONLY, if you are using PHP7 and Pthreads v3.


If you are using **Wamp** on **Windows**, you have to configure the extension in **php.ini** :

**Open php\php.ini and add:**

```php
extension=php_pthreads.dll

```

Concerning **Linux** users, you have to replace `.dll` by `.so`:

```php
extension=pthreads.so

```

You can directly execute this command to add it to `php.ini` (change `/etc/php.ini` with your custom path)

```php
echo "extension=pthreads.so" >> /etc/php.ini

```

