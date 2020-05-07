---
metaTitle: "MongoDB - Collections"
description: "Create a Collection, Drop Collection"
---

# Collections




## Create a Collection


First Select Or Create a database.

```js
> use mydb
switched to db mydb

```

Using `db.createCollection("yourCollectionName")` method you can explicitly create Collection.

```js
> db.createCollection("newCollection1")
{ "ok" : 1 }

```

Using `show collections` command see all collections in the database.

```js
> show collections
newCollection1
system.indexes
> 

```

The `db.createCollection()` method has the following parameters:

|Parameter|Type|Description
|---|---|---|---|---|---|---|---|---|---
|name|string|The name of the collection to create.
|options|document|**Optional.** Configuration options for creating a [capped collection](https://docs.mongodb.com/v3.2/reference/glossary/#term-capped-collection) or for preallocating space in a new collection.

The fllowing example shows the syntax of `createCollection()` method with few important options

```js
>db.createCollection("newCollection4", {capped :true, autoIndexId : true, size : 6142800, max : 10000})
{ "ok" : 1 }

```

Both the `db.collection.insert()` and the `db.collection.createIndex()` operations create their respective collection if they do not already exist.

```js
> db.newCollection2.insert({name : "XXX"})
> db.newCollection3.createIndex({accountNo : 1})

```

Now, Show All the collections using `show collections` command

```js
> show collections
newCollection1
newCollection2
newCollection3
newCollection4
system.indexes

```

If you want to see the inserted document, use the `find()` command.

```js
> db.newCollection2.find()
{ "_id" : ObjectId("58f26876cabafaeb509e9c1f"), "name" : "XXX" }

```



## Drop Collection


MongoDB's `db.collection.drop()` is used to drop a collection from the database.

First, check the available collections into your database `mydb`.

```js
> use mydb
switched to db mydb

> show collections
newCollection1
newCollection2
newCollection3
system.indexes

```

Now drop the collection with the name `newCollection1`.

```js
> db.newCollection1.drop()
true

```

**Note:** If the collection droped successfully then the method will return `true` otherwise it will return `false`.

Again check the list of collections into database.

```js
> show collections
newCollection2
newCollection3
system.indexes

```

**Reference:** MongoDB [drop()](https://docs.mongodb.com/manual/reference/method/db.collection.drop/) Method.



#### Remarks


Create Database

