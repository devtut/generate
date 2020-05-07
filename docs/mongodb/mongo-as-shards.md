---
metaTitle: "MongoDB - Mongo as Shards"
description: "Sharding Environment Setup"
---

# Mongo as Shards



## Sharding Environment Setup


Sharding Group Members :

For sharding there are three players.

<li>
Config Server
</li>
<li>
Replica Sets
</li>
<li>
Mongos
For a mongo shard we need to setup the above three servers.
</li>

Config Server Setup :
add the following to mongod conf file

```js
sharding:
  clusterRole: configsvr
replication:
  replSetName: <setname>  

```

**run :** mongod --config 

**we can choose config server as replica set or may be a standalone server. Based on our requirement we can choose the best. If config need to run in replica set we need to follow the replica set setup**

**Replica Setup :**
Create replica set
// Please refer the replica setup

**MongoS Setup :**
Mongos is main setup in shard. Its is query router to access all replica sets

Add the following in mongos conf file

```

   sharding:
      configDB: <configReplSetName>/cfg1.example.net:27017;

```

Configure Shared :

Connect the mongos via shell (mongo --host  --port )

1. sh.addShard( "/s1-mongo1.example.net:27017")
1. sh.enableSharding("")
1. sh.shardCollection("< database >.< collection >", { < key > : < direction > } )
1. sh.status() // To ensure the sharding

