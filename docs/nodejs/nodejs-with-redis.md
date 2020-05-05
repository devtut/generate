---
metaTitle: "Node.js - NodeJS with Redis"
description: "Getting Started, Storing Key-Value Pairs, Some more important operations supported by node_redis."
---

# NodeJS with Redis



## Getting Started


node_redis, as you may have guessed, is the [Redis client for Node.js](https://www.npmjs.com/package/redis). You can install it via npm using the following command.

```js
npm install redis

```

Once you have installed node_redis module you are good to go. Let’s create a simple file, app.js, and see how to connect with Redis from Node.js.

> 
app.js


```js
var redis = require('redis');
client = redis.createClient(); //creates a new client

```

By default, redis.createClient() will use 127.0.0.1 and 6379 as the hostname and port respectively. If you have a different host/port you can supply them as following:

```js
var client = redis.createClient(port, host);

```

Now, you can perform some action once a connection has been established. Basically, you just need to listen for connect events as shown below.

```js
client.on('connect', function() {
    console.log('connected');
});

```

So, the following snippet goes into app.js:

```js
var redis = require('redis');
var client = redis.createClient();

client.on('connect', function() {
    console.log('connected');
});

```

Now, type node app in the terminal to run the app. Make sure your Redis server is up and running before running this snippet.



## Storing Key-Value Pairs


Now that you know how to connect with Redis from Node.js, let’s see how to store key-value pairs in Redis storage.

> 
**Storing Strings**


All the Redis commands are exposed as different functions on the client object. To store a simple string use the following syntax:

```js
client.set('framework', 'AngularJS');

```

Or

```js
client.set(['framework', 'AngularJS']);

```

The above snippets store a simple string AngularJS against the key framework. You should note that both the snippets do the same thing. The only difference is that the first one passes a variable number of arguments while the later passes an args array to `client.set()` function. You can also pass an optional callback to get a notification when the operation is complete:

```js
client.set('framework', 'AngularJS', function(err, reply) {
  console.log(reply);
});

```

If the operation failed for some reason, the `err` argument to the callback represents the error. To retrieve the value of the key do the following:

```js
client.get('framework', function(err, reply) {
    console.log(reply);
});

```

`client.get()` lets you retrieve a key stored in Redis. The value of the key can be accessed via the callback argument reply. If the key doesn’t exist, the value of reply will be empty.

> 
**Storing Hash**


Many times storing simple values won’t solve your problem. You will need to store hashes (objects) in Redis. For that you can use `hmset()` function as following:

```js
client.hmset('frameworks', 'javascript', 'AngularJS', 'css', 'Bootstrap', 'node', 'Express');

client.hgetall('frameworks', function(err, object) {
    console.log(object);
});

```

The above snippet stores a hash in Redis that maps each technology to its framework. The first argument to `hmset()` is the name of the key. Subsequent arguments represent key-value pairs. Similarly, `hgetall()` is used to retrieve the value of the key. If the key is found, the second argument to the callback will contain the value which is an object.

Note that Redis doesn’t support nested objects. All the property values in the object will be coerced into strings before getting stored.
You can also use the following syntax to store objects in Redis:

```js
client.hmset('frameworks', {
    'javascript': 'AngularJS',
    'css': 'Bootstrap',
    'node': 'Express'
});

```

An optional callback can also be passed to know when the operation is completed.

All the functions (commands) can be called with uppercase/lowercase equivalents. For example, `client.hmset()` and `client.HMSET()` are the same.
Storing Lists

If you want to store a list of items, you can use Redis lists. To store a list use the following syntax:

```js
client.rpush(['frameworks', 'angularjs', 'backbone'], function(err, reply) {
    console.log(reply); //prints 2
});

```

The above snippet creates a list called frameworks and pushes two elements to it. So, the length of the list is now two. As you can see I have passed an `args` array to `rpush`. The first item of the array represents the name of the key while the rest represent the elements of the list. You can also use `lpush()` instead of `rpush()` to push the elements to the left.

To retrieve the elements of the list you can use the `lrange()` function as following:

```js
client.lrange('frameworks', 0, -1, function(err, reply) {
    console.log(reply); // ['angularjs', 'backbone']
});

```

Just note that you get all the elements of the list by passing -1 as the third argument to `lrange()`. If you want a subset of the list, you should pass the end index here.

> 
**Storing Sets**


Sets are similar to lists, but the difference is that they don’t allow duplicates. So, if you don’t want any duplicate elements in your list you can use a set. Here is how we can modify our previous snippet to use a set instead of list.

```js
client.sadd(['tags', 'angularjs', 'backbonejs', 'emberjs'], function(err, reply) {
    console.log(reply); // 3
});

```

As you can see, the `sadd()` function creates a new set with the specified elements. Here, the length of the set is three. To retrieve the members of the set, use the `smembers()` function as following:

```js
client.smembers('tags', function(err, reply) {
    console.log(reply);
});

```

This snippet will retrieve all the members of the set. Just note that the order is not preserved while retrieving the members.

This was a list of the most important data structures found in every Redis powered app. Apart from strings, lists, sets, and hashes, you can store sorted sets, hyperLogLogs, and more in Redis. If you want a complete list of commands and data structures, visit the official Redis documentation. Remember that almost every Redis command is exposed on the client object offered by the node_redis module.



## Some more important operations supported by node_redis.


> 
**Checking the Existence of Keys**


Sometimes you may need to check if a key already exists and proceed accordingly. To do so you can use `exists()` function as shown below:

```js
client.exists('key', function(err, reply) {
    if (reply === 1) {
        console.log('exists');
    } else {
        console.log('doesn\'t exist');
    }
});

```

> 
**Deleting and Expiring Keys**


At times you will need to clear some keys and reinitialize them. To clear the keys, you can use del command as shown below:

```js
client.del('frameworks', function(err, reply) {
    console.log(reply);
});

```

You can also give an expiration time to an existing key as following:

```js
client.set('key1', 'val1');
client.expire('key1', 30);

```

The above snippet assigns an expiration time of 30 seconds to the key key1.

> 
**Incrementing and Decrementing**


Redis also supports incrementing and decrementing keys. To increment a key use `incr()` function as shown below:

```js
client.set('key1', 10, function() {
    client.incr('key1', function(err, reply) {
        console.log(reply); // 11
    });
});

```

The `incr()` function increments a key value by 1. If you need to increment by a different amount, you can use `incrby()` function. Similarly, to decrement a key you can use the functions like `decr()` and `decrby()`.



#### Remarks


We have covered the basic and most commonly used operations in node_redis. You can use this module to leverage the full power of Redis and create really sophisticated Node.js apps. You can build many interesting things with this library such as a strong caching layer, a powerful Pub/Sub messaging system and more. To know more about the library check out their [documentation](https://www.npmjs.com/package/redis).

