---
metaTitle: "MongoDB - Backing up and Restoring Data"
description: "Basic mongodump of local default mongod instance, Basic mongorestore of local default mongod dump"
---

# Backing up and Restoring Data



## Basic mongodump of local default mongod instance


```js
mongodump --db mydb --gzip --out "mydb.dump.$(date +%F_%R)"

```

This command will dump a bson gzipped archive of your local mongod 'mydb' database to the 'mydb.dump.{timestamp}' directory



## Basic mongorestore of local default mongod dump


```js
mongorestore --db mydb mydb.dump.2016-08-27_12:44/mydb --drop --gzip

```

This command will first drop your current 'mydb' database and then restore your gzipped bson dump from the 'mydb mydb.dump.2016-08-27_12:44/mydb' archive dump file.

