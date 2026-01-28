---
metaTitle: "MongoDB - Java Driver"
description: "Fetch Collection data with condition, Create a tailable cursor, Create a database user"
---

# Java Driver



## Fetch Collection data with condition


To get data from `testcollection` collection in `testdb` database where `name=dev`

```js
import org.bson.Document;
import com.mongodb.BasicDBObject;
import com.mongodb.MongoClient;
import com.mongodb.ServerAddress;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoCursor;
import com.mongodb.client.MongoDatabase;

MongoClient mongoClient = new MongoClient(new ServerAddress("localhost", 27017));
MongoDatabase db = mongoClient.getDatabase("testdb");
MongoCollection<Document> collection = db.getCollection("testcollection");

BasicDBObject searchQuery = new BasicDBObject();
searchQuery.put("name","dev");

MongoCursor<Document> cursor = collection.find(searchQuery).iterator();  
try {
    while (cursor.hasNext()) {
        System.out.println(cursor.next().toJson());
    }
} finally {
    cursor.close();
}

```



## Create a tailable cursor


```js
find(query).projection(fields).cursorType(CursorType.TailableAwait).iterator();

```

That code applies to the `MongoCollection` class.

CursorType is an enum and it has the following values:

```js
Tailable
TailableAwait

```

Corresponding to the old (<3.0) DBCursor addOption Bytes types:

```js
Bytes.QUERYOPTION_TAILABLE
Bytes.QUERYOPTION_AWAITDATA

```



## Create a database user


To create a user **dev** with password **password123**

```js
MongoClient mongo = new MongoClient("localhost", 27017);
MongoDatabase db =  mongo.getDatabase("testDB");
Map<String, Object> commandArguments = new BasicDBObject();
commandArguments.put("createUser", "dev");
commandArguments.put("pwd", "password123");
String[] roles = { "readWrite" };
commandArguments.put("roles", roles);
BasicDBObject command = new BasicDBObject(commandArguments);
db.runCommand(command);

```

