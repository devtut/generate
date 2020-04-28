---
metaTitle: "Using Redis with PHP"
description: "Connecting to a Redis instance, Installing PHP Redis on Ubuntu, Executing Redis commands in PHP"
---

# Using Redis with PHP



## Connecting to a Redis instance


Assuming a default server running on localhost with the default port, the command to connect to that Redis server would be:

```
$redis = new Redis();
$redis->connect('127.0.0.1', 6379);

```



## Installing PHP Redis on Ubuntu


To install PHP on Ubuntu, first install the Redis server:

```
sudo apt install redis-server

```

then install the PHP module:

```
sudo apt install php-redis

```

And restart the Apache server:

```
sudo service apache2 restart

```



## Executing Redis commands in PHP


The Redis PHP module gives access to the same commands as the Redis CLI client so it is quite straightforward to use.

The syntax is as follow:

```
// Creates two new keys:
$redis->set('mykey-1', 123);
$redis->set('mykey-2', 'abcd');

// Gets one key (prints '123')
var_dump($redis->get('mykey-1'));

// Gets all keys starting with 'my-key-'
// (prints '123', 'abcd')
var_dump($redis->keys('mykey-*'));

```

