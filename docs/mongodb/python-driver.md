---
metaTitle: "MongoDB - Python Driver"
description: "Connect to MongoDB using pymongo, PyMongo queries, Update all documents in a collection using PyMongo"
---

# Python Driver



## Connect to MongoDB using pymongo


```js
from pymongo import MongoClient

uri = "mongodb://localhost:27017/"

client = MongoClient(uri)

db = client['test_db']
# or
# db = client.test_db

# collection = db['test_collection']
# or
collection = db.test_collection


collection.save({"hello":"world"})

print collection.find_one()

```



## PyMongo queries


Once you got a `collection` object, queries use the same syntax as in the mongo shell. Some slight differences are:

<li>
every key must be enclosed in brackets. For example:

```js
db.find({frequencies: {$exists: true}})

```


becomes in `pymongo` (note the `True` in uppercase):

```js
db.find({"frequencies": { "$exists": True }})

```


</li>
<li>
<p>objects such as object ids or `ISODate` are manipulated using python classes. PyMongo uses its own
[`ObjectId`](http://api.mongodb.com/python/current/api/bson/objectid.html) class to deal with object ids, while dates use the standard `datetime` package. For example,
if you want to query all events between 2010 and 2011, you can do:</p>

```js
 from datetime import datetime

 date_from = datetime(2010, 1, 1)
 date_to = datetime(2011, 1, 1)
 db.find({ "date": { "$gte": date_from, "$lt": date_to } }):

```


</li>



## Update all documents in a collection using PyMongo


Let's say you need to add a field to every document in a collection.

```js
import pymongo

client = pymongo.MongoClient('localhost', 27017)
db = client.mydb.mycollection

for doc in db.find():
   db.update(
       {'_id': doc['_id']}, 
       {'$set': {'newField': 10} }, upsert=False, multi=False)

```

The `find` method returns a `Cursor`, on which you can easily iterate over using the `for in` syntax.
Then, we call the `update` method, specifying the `_id` and that we add a field (`$set`). The parameters
`upsert` and `multi` come from mongodb ([see here for more info](https://docs.mongodb.com/manual/reference/method/db.collection.update/)).



#### Syntax


- mongodb://[username:password@]host1[:port1][,host2[:port2],...[,hostN[:portN]]][/[database][?options]]



#### Parameters


|Parameter|Detail
|---|---|---|---|---|---|---|---|---|---
|hostX|Optional. You can specify as many hosts as necessary. You would specify multiple hosts, for example, for connections to replica sets.
|:portX|Optional. The default value is :27017 if not specified.
|database|Optional. The name of the database to authenticate if the connection string includes authentication credentialsIf /database is not specified and the connection string includes credentials, the driver will authenticate to the admin database.
|?options|Connection specific options

