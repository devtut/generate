---
metaTitle: "MongoDB - Indexes"
description: "Dropping/Deleting an Index, Get Indices of a Collection, Sparse indexes and Partial indexes, Compound, Unique Index, Single field, Delete, List, Index Creation Basics"
---

# Indexes




## Dropping/Deleting an Index


If index name is known,

```js
db.collection.dropIndex('name_of_index');

```

If index name is not known,

```js
db.collection.dropIndex( { 'name_of_field' : -1 } );

```



## Get Indices of a Collection


```

db.collection.getIndexes();

```

**Output**

```js
[
    {
        "v" : 1,
        "key" : {
            "_id" : 1
        },
        "name" : "_id_",
        "ns" : "documentation_db.transactions"
    },
    {
        "v" : 1,
        "key" : {
            "cr_dr" : 1
        },
        "name" : "cr_dr_1",
        "ns" : "documentation_db.transactions"
    },
    {
        "v" : 1,
        "key" : {
            "cr_dr" : -1
        },
        "name" : "index on cr_dr desc",
        "ns" : "documentation_db.transactions"
    }
]

```



## Sparse indexes and Partial indexes


**Sparse indexes:**

These can be particularly useful for fields that are optional but which should also be unique.

```js
{ "_id" : "john@example.com", "nickname" : "Johnnie" }
{ "_id" : "jane@example.com" }
{ "_id" : "julia@example.com", "nickname" : "Jules"}
{ "_id" : "jack@example.com" }

```

Since two entries have no "nickname" specified and indexing will treat unspecified fields as null, the index creation would fail with 2 documents having 'null', so:

```js
db.scores.createIndex( { nickname: 1 } , { unique: true, sparse: true } )

```

will let you still have 'null' nicknames.

Sparse indexes are more compact since they skip/ignore documents that don't specify that field. So if you have a collection where only less than 10% of documents specify this field, you can create much smaller indexes - making better use of limited memory if you want to do queries like:

```js
db.scores.find({'nickname': 'Johnnie'})

```

**Partial indexes:**

> 
<p>Partial indexes represent a superset of the functionality offered by
sparse indexes and should be preferred over sparse indexes.
(**New in version 3.2**)</p>


Partial indexes determine the index entries based on the specified filter.

```js
db.restaurants.createIndex(
  { cuisine: 1 },
  { partialFilterExpression: { rating: { $gt: 5 } } }
)

```

If `rating` is greater than 5, then `cuisine` will be indexed. Yes, we can specify a property to be indexed based on the value of other properties also.

**Difference between Sparse and Partial indexes:**

Sparse indexes select documents to index solely based on the existence of the indexed field, or for compound indexes, the existence of the indexed fields.

Partial indexes determine the index entries based on the specified filter. The filter can include fields other than the index keys and can specify conditions other than just an existence check.

Still, a partial index can implement the same behavior as a sparse index

Eg:

```js
db.contacts.createIndex(
   { name: 1 },
   { partialFilterExpression: { name: { $exists: true } } }
)

```

> 
<p>**Note:** Both the **partialFilterExpression** option and the **sparse** option
cannot be specified at the same time.</p>




## Compound


```js
db.people.createIndex({name: 1, age: -1})

```

This creates an index on multiple fields, in this case on the `name` and `age` fields. It will be ascending in `name` and descending in `age`.

In this type of index, the sort order is relevant, because it will determine whether the index can support a sort operation or not. Reverse sorting is supported on any prefix of a compound index, as long as the sort is in the reverse sort direction for **all** of the keys in the sort. Otherwise, sorting for compound indexes need to match the order of the index.

Field order is also important, in this case the index will be sorted first by `name`, and within each name value, sorted by the values of the `age` field. This allows the index to be used by queries on the `name` field, or on `name` and `age`, but not on `age` alone.



## Unique Index


```js
db.collection.createIndex( { "user_id": 1 }, { unique: true } )

```

enforce uniqueness on the defined index (either single or compound). Building the index will fail if the collection already contains duplicate values; the indexing will fail also with multiple entries missing the field (since they will all be indexed with the value `null`) unless `sparse: true` is specified.



## Single field


```js
db.people.createIndex({name: 1})

```

This creates an ascending single field index on the field **name**.

In this type of indexes the sort order is irrelevant, because mongo can traverse the index in both directions.



## Delete


To drop an index you could use the index name

```js
db.people.dropIndex("nameIndex")

```

Or the index specification document

```js
db.people.dropIndex({name: 1})

```



## List


```js
db.people.getIndexes()

```

This will return an array of documents each describing an index on the **people** collection



## Index Creation Basics


See the below transactions collection.

```js
> db.transactions.insert({ cr_dr : "D", amount : 100, fee : 2});
> db.transactions.insert({ cr_dr : "C", amount : 100, fee : 2});
> db.transactions.insert({ cr_dr : "C", amount : 10,  fee : 2});
> db.transactions.insert({ cr_dr : "D", amount : 100, fee : 4});
> db.transactions.insert({ cr_dr : "D", amount : 10,  fee : 2});
> db.transactions.insert({ cr_dr : "C", amount : 10,  fee : 4});
> db.transactions.insert({ cr_dr : "D", amount : 100, fee : 2});

```

`getIndexes()` functions will show all the indices available for a collection.

```js
db.transactions.getIndexes();

```

Let see the output of above statement.

```js
[
    {
        "v" : 1,
        "key" : {
            "_id" : 1
        },
        "name" : "_id_",
        "ns" : "documentation_db.transactions"
    }
]

```

There is already one index for transaction collection. This is because MongoDB creates a **unique index** on the `_id` field during the creation of a collection. The `_id` index prevents clients from inserting two documents with the same value for the `_id` field. You cannot drop this index on the `_id` field.

Now let's add an index for cr_dr field;

```js
db.transactions.createIndex({ cr_dr : 1 });

```

The result of the index execution is as follows.

```js
{
    "createdCollectionAutomatically" : false,
    "numIndexesBefore" : 1,
    "numIndexesAfter" : 2,
    "ok" : 1
}

```

> 
<p>The createdCollectionAutomatically indicates if the operation created
a collection. If a collection does not exist, MongoDB creates the
collection as part of the indexing operation.</p>


Let run `db.transactions.getIndexes();` again.

```js
[
    {
        "v" : 1,
        "key" : {
            "_id" : 1
        },
        "name" : "_id_",
        "ns" : "documentation_db.transactions"
    },
    {
        "v" : 1,
        "key" : {
            "cr_dr" : 1
        },
        "name" : "cr_dr_1",
        "ns" : "documentation_db.transactions"
    }
]

```

Now you see transactions collection have two indices. Default `_id` index and `cr_dr_1` which we created. The name is assigned by MongoDB. You can set your own name like below.

```js
db.transactions.createIndex({ cr_dr : -1 },{name : "index on cr_dr desc"})

```

Now `db.transactions.getIndexes();` will give you three indices.

```js
[
    {
        "v" : 1,
        "key" : {
            "_id" : 1
        },
        "name" : "_id_",
        "ns" : "documentation_db.transactions"
    },
    {
        "v" : 1,
        "key" : {
            "cr_dr" : 1
        },
        "name" : "cr_dr_1",
        "ns" : "documentation_db.transactions"
    },
    {
        "v" : 1,
        "key" : {
            "cr_dr" : -1
        },
        "name" : "index on cr_dr desc",
        "ns" : "documentation_db.transactions"
    }
]

```

While creating index `{ cr_dr : -1 }` 1 means index will be in `ascending` order and -1 for `descending` order.

### Hashed indexes

Indexes can be defined also as **hashed**. This is more performant on **equality queries**, but is not efficient for **range queries**; however you can define both hashed and ascending/descending indexes on the same field.

```js
> db.transactions.createIndex({ cr_dr : "hashed" });    

> db.transactions.getIndexes(
[
    {
        "v" : 1,
        "key" : {
            "_id" : 1
        },
        "name" : "_id_",
        "ns" : "documentation_db.transactions"
    },
    {
        "v" : 1,
        "key" : {
            "cr_dr" : "hashed"
        },
        "name" : "cr_dr_hashed",
        "ns" : "documentation_db.transactions"
    }
]

```



#### Syntax


- `db.collection.createIndex({ <string field> : <1|-1 order> [, <string field> : <1|-1 order>] });`



#### Remarks


**Performance Impact**: Note that indexes improve read performances, but can have bad impact on write performance, as inserting a document requires updating all indexes.

