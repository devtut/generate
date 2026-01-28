---
metaTitle: "MongoDB - Pluggable Storage Engines"
description: "WiredTiger, MMAP, In-memory, mongo-rocks, Fusion-io, TokuMX"
---

# Pluggable Storage Engines




## WiredTiger


WiredTiger supports **LSM trees to store indexes**. LSM trees are faster for write operations when you need to write huge workloads of random inserts.

In WiredTiger, there is **no in-place updates**. If you need to update an element of a document, a new document will be inserted while the old document will be deleted.

WiredTiger also offers **document-level concurrency**. It assumes that two write operations will not affect the same document, but if it does, one operation will be rewind and executed later. That's a great performance boost if rewinds are rare.

WiredTiger supports **Snappy and zLib algorithms for compression** of data and indexes in the file system. Snappy is the default. It is less CPU-intensive but have a lower compression rate than zLib.

### How to use WiredTiger Engine

```js
mongod --storageEngine wiredTiger --dbpath <newWiredTigerDBPath>

```

**Note:**

1. After mongodb 3.2, the default engine is WiredTiger.
1. `newWiredTigerDBPath` should not contain data of another storage engine. To migrate your data, you have to dump them, and re-import them in the new storage engine.

```

   mongodump --out <exportDataDestination>
    mongod --storageEngine wiredTiger --dbpath <newWiredTigerDBPath>
    mongorestore <exportDataDestination>

```



## MMAP


MMAP is a pluggable storage engine that was named after the `mmap()` Linux command. It maps files to the virtual memory and optimizes read calls. If you have a large file but needs to read just a small part of it, `mmap()` is much faster then a `read()` call that would bring the entire file to the memory.

One disadvantage is that you can't have two write calls being processed in parallel for the same collection. So, MMAP has collection-level locking (and not document-level locking as WiredTiger offers). This collection-locking is necessary because one MMAP index can reference multiples documents and if those docs could be updated simultaneously, the index would be inconsistent.



## In-memory


All data is stored in-memory (RAM) for faster read/access.



## mongo-rocks


A key-value engine created to integrate with Facebook's RocksDB.



## Fusion-io


A storage engine created by SanDisk that makes it possible to bypass the OS file system layer and write directly to the storage device.



## TokuMX


A storage engine created by Percona that uses fractal tree indexes.



#### Remarks


In MongoDB 3.0, MMAP (default) and WiredTiger are the stable storage engines. Usually, if your app is read-heavy, use MMAP. If its write-heavy, use WiredTiger.

Your solution may also have a mixed replica set members where you can have one node configured with MMAP and another with WiredTiger. You can use one to insert massive data and the other to read with analytical tools.

After MongoDB 3.2, WiredTiger becomes the default engine.

