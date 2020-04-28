---
metaTitle: "mongo-php"
description: "Everything in between MongoDB and Php"
---

# mongo-php



## Everything in between MongoDB and Php


**Requirements**

<li>
MongoDB server running on port usually 27017. (type `mongod` on command prompt to run mongodb server)
</li>
<li>
Php installed as either cgi or fpm with MongoDB extension installed(MongoDB extension is not bundled with default php)
</li>
<li>
Composer library(mongodb/mongodb).(In the project root run `php composer.phar require "mongodb/mongodb=^1.0.0"` to install the MongoDB library)
</li>

If everything is ok you are ready to move on.

> 
Check For Php installation


if not sure check Php installation by running `php -v` on command prompt will return
something like this

`PHP 7.0.6 (cli) (built: Apr 28 2016 14:12:14) ( ZTS ) Copyright (c) 1997-2016 The PHP Group Zend Engine v3.0.0, Copyright (c) 1998-2016 Zend Technologies`

> 
Check For MongoDB installation


Check MongoDB installation by running  `mongo --version` will return
`MongoDB shell version: 3.2.6`

> 
Check For Composer installation


Check for Composer installation by running  `php composer.phar --version` will return
`Composer version 1.2-dev (3d09c17b489cd29a0c0b3b11e731987e7097797d) 2016-08-30 16:12:39`
`

**Connecting to MongoDB from php**

```
<?php

  //This path should point to Composer's autoloader from where your MongoDB library will be loaded
  require 'vendor/autoload.php';


 // when using custom username password
  try {
        $mongo = new MongoDB\Client('mongodb://username:password@localhost:27017');
        print_r($mongo->listDatabases());
  } catch (Exception $e) {
        echo $e->getMessage();
  }


 // when using default settings
  try {
        $mongo = new MongoDB\Client('mongodb://localhost:27017');
        print_r($mongo->listDatabases());
  } catch (Exception $e) {
        echo $e->getMessage();
  }

```

**The above code will connect using MongoDB composer library(`mongodb/mongodb`) included as `vendor/autoload.php` to connect to the MongoDB server running on `port`: `27017`**. If everything is ok it will connect and list an array, if exception occurs connecting to MongoDB server the message will be printed.

**CREATE(Inserting) into MongoDB**

```
<?php

 //MongoDB uses collection rather than Tables as in case on SQL.
 //Use $mongo instance to select the database and collection
 //NOTE: if database(here demo) and collection(here beers) are not found in MongoDB both will be created automatically by MongoDB.
  $collection = $mongo->demo->beers;

 //Using $collection we can insert one document into MongoDB
 //document is similar to row in SQL.
  $result = $collection->insertOne( [ 'name' => 'Hinterland', 'brewery' => 'BrewDog' ] );

 //Every inserted document will have a unique id.
  echo "Inserted with Object ID '{$result->getInsertedId()}'";
?>

```

**In the example we are using the $mongo instance previously used in the `Connecting to MongoDB from php` part. MongoDB uses JSON type data format, so in php we will use array to insert data into MongoDB, this conversion from array to Json and vice versa will be done by mongo library. Every document in MongoDB has a unique id named as _id,during insertion we can get this by using `$result->getInsertedId()`;**

**READ(Find) in MongoDB**

```
<?php
 //use find() method to query for records, where parameter will be array containing key value pair we need to find.
 $result = $collection->find( [ 'name' => 'Hinterland', 'brewery' => 'BrewDog' ] );

 // all the data(result) returned as array
 // use for each  to filter the required keys
 foreach ($result as $entry) {
   echo $entry['_id'], ': ', $entry['name'], "\n";
 }

?>

```

**Drop in MongoDB**

```
<?php

 $result = $collection->drop( [ 'name' => 'Hinterland'] );

 //return 1 if the drop was sucessfull and 0 for failure
 print_r($result->ok);

?>

```

**There are many methods that can be performed on `$collection` see [Official documentation](http://mongodb.github.io/mongo-php-library/api/index.html) from MongoDB**



#### Syntax


1. find()

