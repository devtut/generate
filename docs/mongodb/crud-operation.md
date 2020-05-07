---
metaTitle: "MongoDB - CRUD Operation"
description: "Create, Update, Delete, Read, More update operators, multi Parameter while updating multiple documents, Update of embedded documents."
---

# CRUD Operation



## Create


```js
db.people.insert({name: 'Tom', age: 28});

```

Or

```js
db.people.save({name: 'Tom', age: 28});

```

The difference with [`save`](https://docs.mongodb.com/manual/reference/method/db.collection.save/#db.collection.save) is that if the passed document contains an `_id` field, if a document already exists with that `_id` it will be updated instead of being added as new.

Two new methods to insert documents into a collection, in MongoDB 3.2.x:-

Use `insertOne` to insert only one record:-

```js
db.people.insertOne({name: 'Tom', age: 28});

```

Use `insertMany` to insert multiple records:-

```js
db.people.insertMany([{name: 'Tom', age: 28},{name: 'John', age: 25}, {name: 'Kathy', age: 23}])

```

Note that  `insert` is highlighted as deprecated in every official language driver since version 3.0. The full distinction being that the shell methods actually lagged behind the other drivers in implementing the method. The same thing applies for all other CRUD methods



## Update


Update the **entire** object:

```js
db.people.update({name: 'Tom'}, {age: 29, name: 'Tom'})

// New in MongoDB 3.2
db.people.updateOne({name: 'Tom'},{age: 29, name: 'Tom'}) //Will replace only first matching document.

db.people.updateMany({name: 'Tom'},{age: 29, name: 'Tom'}) //Will replace all matching documents.

```

Or just update a single field of a document. In this case `age`:

```js
db.people.update({name: 'Tom'}, {$set: {age: 29}})

```

You can also update multiple documents simultaneously by adding a third parameter. This query will update all documents where the name equals `Tom`:

```js
db.people.update({name: 'Tom'}, {$set: {age: 29}}, {multi: true})

// New in MongoDB 3.2
db.people.updateOne({name: 'Tom'},{$set:{age: 30}) //Will update only first matching document.

db.people.updateMany({name: 'Tom'},{$set:{age: 30}}) //Will update all matching documents.

```

If a new field is coming for update, that field will be added to the document.

```js
db.people.updateMany({name: 'Tom'},{$set:{age: 30, salary:50000}})// Document will have `salary` field as well.

```

If a document is needed to be replaced,

```js
db.collection.replaceOne({name:'Tom'}, {name:'Lakmal',age:25,address:'Sri Lanka'})

```

can be used.

**Note**: Fields you use to identify the object will be saved in the updated document. Field that are not defined in the update section will be removed from the document.



## Delete


Deletes all documents matching the query parameter:

```js
// New in MongoDB 3.2
db.people.deleteMany({name: 'Tom'})

// All versions
db.people.remove({name: 'Tom'})

```

Or just one

```js
// New in MongoDB 3.2
db.people.deleteOne({name: 'Tom'})

// All versions
db.people.remove({name: 'Tom'}, true)

```

MongoDB's `remove()` method. If you execute this command without any argument or without empty argument it will remove all documents from the collection.

```js
db.people.remove();

```

or

```js
db.people.remove({});

```



## Read


Query for all the docs in the `people` collection that have a `name` field with a value of `'Tom'`:

```js
db.people.find({name: 'Tom'})

```

Or just the first one:

```js
db.people.findOne({name: 'Tom'})

```

You can also specify which fields to return by passing a field selection parameter. The following will exclude the `_id` field and only include the `age` field:

```js
db.people.find({name: 'Tom'}, {_id: 0, age: 1})

```

Note: by default, the `_id` field will be returned, even if you don't ask for it. If you would like not to get the `_id` back, you can just follow the previous example and ask for the `_id` to be excluded by specifying `_id: 0` (or `_id: false`).If you want to find sub record like address object contains country, city, etc.

```js
db.people.find({'address.country': 'US'})

```

& specify field too if required

```js
db.people.find({'address.country': 'US'}, {'name': true, 'address.city': true})Remember that the result has a `.pretty()` method that pretty-prints resulting JSON:

db.people.find().pretty()

```



## More update operators


You can use other operators besides `$set` when updating a document.
The `$push` operator allows you to push a value into an array, in this case we will add a new nickname to the `nicknames` array.

```js
db.people.update({name: 'Tom'}, {$push: {nicknames: 'Tommy'}})
// This adds the string 'Tommy' into the nicknames array in Tom's document.

```

The `$pull` operator is the opposite of `$push`, you can pull specific items from arrays.

```js
db.people.update({name: 'Tom'}, {$pull: {nicknames: 'Tommy'}})
// This removes the string 'Tommy' from the nicknames array in Tom's document.

```

The `$pop` operator allows you to remove the first or the last value from an array. Let's say Tom's document has a property called siblings that has the value `['Marie', 'Bob', 'Kevin', 'Alex']`.

```js
db.people.update({name: 'Tom'}, {$pop: {siblings: -1}})
// This will remove the first value from the siblings array, which is 'Marie' in this case.


db.people.update({name: 'Tom'}, {$pop: {siblings: 1}})
// This will remove the last value from the siblings array, which is 'Alex' in this case.

```



## "multi" Parameter while updating multiple documents


To update multiple documents in a collection, set the multi option to true.

```js
db.collection.update(
   query,
   update,
   {
     upsert: boolean,
     multi: boolean,
     writeConcern: document
   }
)

```

multi is optional. If set to true, updates multiple documents that meet the query criteria. If set to false, updates one document. The default value is false.

> 
<p>db.mycol.find()
{ "_id" : ObjectId(598354878df45ec5), "title":"MongoDB Overview"}
{ "_id" : ObjectId(59835487adf45ec6), "title":"NoSQL Overview"}
{ "_id" : ObjectId(59835487adf45ec7), "title":"Tutorials Point Overview"}</p>


> 
<p>db.mycol.update({'title':'MongoDB Overview'},
{$set:{'title':'New MongoDB Tutorial'}},{multi:true})</p>




## Update of embedded documents.


For the following schema:

```js
{name: 'Tom', age: 28, marks: [50, 60, 70]}

```

Update Tom's marks to 55 where marks are 50 (Use the positional operator $):

```js
db.people.update({name: "Tom", marks: 50}, {"$set": {"marks.$": 55}})

```

For the following schema:

```js
{name: 'Tom', age: 28, marks: [{subject: "English", marks: 90},{subject: "Maths", marks: 100}, {subject: "Computes", marks: 20}]}

```

Update Tom's English marks to 85 :

```js
db.people.update({name: "Tom", "marks.subject": "English"},{"$set":{"marks.$.marks": 85}})

```

Explaining above example:

By using {name: "Tom", "marks.subject": "English"} you will get the position of the object in the marks array, where subject is English. In "marks.$.marks", $ is used to update in that position of the marks array

**Update Values in an Array**

The positional $ operator identifies an element in an array to update without explicitly specifying the position of the element in the array.

Consider a collection students with the following documents:

```js
{ "_id" : 1, "grades" : [ 80, 85, 90 ] }
{ "_id" : 2, "grades" : [ 88, 90, 92 ] }
{ "_id" : 3, "grades" : [ 85, 100, 90 ] }

```

To update 80 to 82 in the grades array in the first document, use the positional $ operator if you do not know the position of the element in the array:

```js
db.students.update(
   { _id: 1, grades: 80 },
   { $set: { "grades.$" : 82 } }
)

```



#### Syntax


- insert(**document or array of documents**)
<li>insertOne(
'document',
{
writeConcern: 'document'
}
)</li>
<li>insertMany(
{ [ document 1 , document 2, ... ] },
{
writeConcern: document,
ordered: boolean
}
)</li>
- find(**query**, **projection**)
- findOne(**query**, **projection**)
- update(**query**, **update**)
<li>updateOne(
**query**,
**update**,
{
upsert: boolean,
writeConcern: document
}
)</li>
<li>updateMany(
**query**,
**update**,
{
upsert: boolean,
writeConcern: document
}
)</li>
<li>replaceOne(
**query**,
**replacement**,
{
upsert: boolean,
writeConcern: document
}
)</li>
- remove(**query**, **justOne**)
- findAndModify(**query**, **sort**, **update**, **options[optional]**)



#### Remarks


Updating and Deleting a document should be done carefully. Since operation may affect for multiple documents.

