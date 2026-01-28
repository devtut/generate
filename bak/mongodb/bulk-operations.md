---
metaTitle: "MongoDB - Bulk Operations"
description: "Converting a field to another type and updating the entire collection in Bulk"
---

# Bulk Operations



## Converting a field to another type and updating the entire collection in Bulk


Usually the case when one wants to change a field type to another, for instance the original collection may have "numerical" or "date" fields saved as strings:

```js
{
    "name": "Alice",
    "salary": "57871",
    "dob": "1986-08-21"
},
{
    "name": "Bob",
    "salary": "48974",
    "dob": "1990-11-04"
}

```

The objective would be to update a humongous collection like the above to

```js
{
    "name": "Alice",
    "salary": 57871,
    "dob": ISODate("1986-08-21T00:00:00.000Z")
},
{
    "name": "Bob",
    "salary": 48974,
    "dob": ISODate("1990-11-04T00:00:00.000Z")
}

```

For relatively small data, one can achieve the above by iterating the collection using a **[`snapshot`](https://docs.mongodb.com/manual/reference/method/cursor.snapshot/)** with the cursor's **[`forEach()`](http://**%5B%60forEach()%60%5D%5B2%5D**)** method and updating each document as follows:

```js
db.test.find({
    "salary": { "$exists": true, "$type": 2 },
    "dob": { "$exists": true, "$type": 2 }
}).snapshot().forEach(function(doc){ 
    var newSalary = parseInt(doc.salary),
        newDob = new ISODate(doc.dob);        
    db.test.updateOne(
        { "_id": doc._id },
        { "$set": { "salary": newSalary, "dob": newDob } }
    );
});

```

Whilst this is optimal for small collections, performance with large collections is greatly reduced since looping through a large dataset and sending each update operation per request to the server incurs a computational penalty.

The **[`Bulk()`](https://docs.mongodb.com/manual/reference/method/Bulk/)** API comes to the rescue and greatly improves performance since write operations are sent to the server only once in bulk. Efficiency is achieved since the method does not send every write request to the server (as with the current update statement within the **[`forEach()`](https://docs.mongodb.com/manual/reference/method/cursor.forEach/)** loop) but just once in every 1000 requests, thus making updates more efficient and quicker than currently is.

Using the same concept above with the **[`forEach()`](https://docs.mongodb.com/manual/reference/method/cursor.forEach/)** loop to create the batches, we can update the collection in bulk as follows. In this demonstration the **[`Bulk()`](https://docs.mongodb.com/manual/reference/method/Bulk/)** API available in MongoDB versions `>= 2.6` and `< 3.2` uses the **[`initializeUnorderedBulkOp()`](https://docs.mongodb.com/manual/reference/method/db.collection.initializeUnorderedBulkOp/#db.collection.initializeUnorderedBulkOp)** method to execute in parallel, as well as in a nondeterministic order, the write operations in the batches.

It updates all the documents in the clients collection by changing the `salary` and `dob` fields to `numerical` and `datetime` values respectively:

```js
var bulk = db.test.initializeUnorderedBulkOp(),
    counter = 0; // counter to keep track of the batch update size

db.test.find({
    "salary": { "$exists": true, "$type": 2 },
    "dob": { "$exists": true, "$type": 2 }
}).snapshot().forEach(function(doc){ 
    var newSalary = parseInt(doc.salary),
        newDob = new ISODate(doc.dob);
    bulk.find({ "_id": doc._id }).updateOne({ 
        "$set": { "salary": newSalary, "dob": newDob }
    });

    counter++; // increment counter
    if (counter % 1000 == 0) {
        bulk.execute(); // Execute per 1000 operations and re-initialize every 1000 update statements
        bulk = db.test.initializeUnorderedBulkOp();
    }
});

```

The next example applies to the new MongoDB version `3.2` which has since deprecated the **[`Bulk()`](https://docs.mongodb.com/manual/reference/method/Bulk/)** API and provided a newer set of apis using **[`bulkWrite()`](https://docs.mongodb.org/v3.2/reference/method/db.collection.bulkWrite/#db.collection.bulkWrite)**.

It uses the same cursors as above but creates the arrays with the bulk operations using the same **[`forEach()`](https://docs.mongodb.com/manual/reference/method/cursor.forEach/)** cursor method to push each bulk write document to the array. Because write commands can accept no more than 1000 operations, there's need to group operations to have at most 1000 operations and re-intialise the array when the loop hits the 1000 iteration:

```js
var cursor = db.test.find({
        "salary": { "$exists": true, "$type": 2 },
        "dob": { "$exists": true, "$type": 2 }
    }),
    bulkUpdateOps = [];

cursor.snapshot().forEach(function(doc){ 
    var newSalary = parseInt(doc.salary),
        newDob = new ISODate(doc.dob);
    bulkUpdateOps.push({ 
        "updateOne": {
            "filter": { "_id": doc._id },
            "update": { "$set": { "salary": newSalary, "dob": newDob } }
         }
    });

    if (bulkUpdateOps.length === 1000) {
        db.test.bulkWrite(bulkUpdateOps);
        bulkUpdateOps = [];
    }
});         

if (bulkUpdateOps.length > 0) { db.test.bulkWrite(bulkUpdateOps); }

```



#### Remarks


Constructing a list of write operations to perform in bulk for a single collection.

