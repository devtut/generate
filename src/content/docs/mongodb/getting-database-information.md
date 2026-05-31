---
title: "Getting database information"
description: "List all databases, List all collections in database"
---

## List all databases


```js
show dbs

```

or

```js
db.adminCommand('listDatabases')

```

or

```js
db.getMongo().getDBNames()

```



## List all collections in database


```js
show collections

```

or

```js
show tables

```

or

```js
db.getCollectionNames()

```