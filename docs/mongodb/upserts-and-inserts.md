---
metaTitle: "MongoDB - Upserts and Inserts"
description: "Insert a document"
---

# Upserts and Inserts




## Insert a document


**_id** is a 12 bytes hexadecimal number which assures the uniqueness of every document. You can provide _id while inserting the document. **If you didn't provide then MongoDB provide a unique id for every document.** These 12 bytes first 4 bytes for the current timestamp, next 3 bytes for machine id, next 2 bytes for process id of mongodb server and remaining 3 bytes are simple incremental value.

```js
db.mycol.insert({
 _id: ObjectId(7df78ad8902c),
 title: 'MongoDB Overview',
 description: 'MongoDB is no sql database',
 by: 'tutorials point',
 url: 'http://www.tutorialspoint.com',
 tags: ['mongodb', 'database', 'NoSQL'],
 likes: 100
})

```

Here **mycol** is a collection name, if the collection doesn't exist in the database, then MongoDB will create this collection and then insert document into it. In the inserted document if we don't specify the **_id** parameter, then MongoDB assigns an unique ObjectId for this document.

