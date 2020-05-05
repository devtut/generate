---
metaTitle: "PHP - Using MongoDB"
description: "Connect to MongoDB, Get multiple documents - find(), Get one document - findOne(), Insert document, Update a document, Delete a document"
---

# Using MongoDB



## Connect to MongoDB


Create a MongoDB connection, that later you can query:

```php
$manager = new \MongoDB\Driver\Manager('mongodb://localhost:27017');

```

In the next example, you will learn how to query the connection object.

This extension close the connection automatically, it's not necessary to close manually.



## Get multiple documents - find()


Example for searching multiple users with the name "Mike":

```php
$filter = ['name' => 'Mike'];
$query = new \MongoDB\Driver\Query($filter);

$cursor = $manager->executeQuery('database_name.collection_name', $query);
foreach ($cursor as $doc) {
    var_dump($doc);
}

```



## Get one document - findOne()


Example for searching just one user with a specific id, you should do:

```php
$options = ['limit' => 1];
$filter = ['_id' => new \MongoDB\BSON\ObjectID('578ff7c3648c940e008b457a')];
$query = new \MongoDB\Driver\Query($filter, $options);

$cursor = $manager->executeQuery('database_name.collection_name', $query);
$cursorArray = $cursor->toArray();
if(isset($cursorArray[0])) {
    var_dump($cursorArray[0]);
}

```



## Insert document


Example for adding a document:

```php
$document = [
    'name' => 'John',
    'active' => true,
    'info' => ['genre' => 'male', 'age' => 30]
];
$bulk = new \MongoDB\Driver\BulkWrite;
$_id1 = $bulk->insert($document);
$result = $manager->executeBulkWrite('database_name.collection_name', $bulk);

```



## Update a document


Example for updating all documents where name is equal to "John":

```php
$filter = ['name' => 'John'];
$document = ['name' => 'Mike'];

$bulk = new \MongoDB\Driver\BulkWrite;
$bulk->update(
    $filter,
    $document,
    ['multi' => true]
);
$result = $manager->executeBulkWrite('database_name.collection_name', $bulk);

```



## Delete a document


Example for deleting all documents where name is equal to "Peter":

```php
$bulk = new \MongoDB\Driver\BulkWrite;

$filter = ['name' => 'Peter'];
$bulk->delete($filter);

$result = $manager->executeBulkWrite('database_name.collection_name', $bulk);

```

