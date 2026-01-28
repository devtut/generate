---
metaTitle: "MongoDB - Mongo as a Replica Set"
description: "Check MongoDB Replica Set states"
---

# Mongo as a Replica Set



## Check MongoDB Replica Set states


Use the below command to check the replica set status.

**Command** : rs.status()

Connect any one of replica member and fire this command it will give the full state of the replica set

Example :

```

  {
    "set" : "ReplicaName",
    "date" : ISODate("2016-09-26T07:36:04.935Z"),
    "myState" : 1,
    "term" : NumberLong(-1),
    "heartbeatIntervalMillis" : NumberLong(2000),
    "members" : [
            {
                    "_id" : 0,
                    "name" : "<IP>:<PORT>,
                    "health" : 1,
                    "state" : 1,
                    "stateStr" : "PRIMARY",
                    "uptime" : 5953744,
                    "optime" : Timestamp(1474875364, 36),
                    "optimeDate" : ISODate("2016-09-26T07:36:04Z"),
                    "electionTime" : Timestamp(1468921646, 1),
                    "electionDate" : ISODate("2016-07-19T09:47:26Z"),
                    "configVersion" : 6,
                    "self" : true
            },
            {
                    "_id" : 1,
                    "name" : "<IP>:<PORT>",
                    "health" : 1,
                    "state" : 2,
                    "stateStr" : "SECONDARY",
                    "uptime" : 5953720,
                    "optime" : Timestamp(1474875364, 13),
                    "optimeDate" : ISODate("2016-09-26T07:36:04Z"),
                    "lastHeartbeat" : ISODate("2016-09-26T07:36:04.244Z"),
                    "lastHeartbeatRecv" : ISODate("2016-09-26T07:36:03.871Z"),
                    "pingMs" : NumberLong(0),
                    "syncingTo" : "10.9.52.55:10050",
                    "configVersion" : 6
            },
            {
                    "_id" : 2,
                    "name" : "<IP>:<PORT>",
                    "health" : 1,
                    "state" : 7,
                    "stateStr" : "ARBITER",
                    "uptime" : 5953696,
                    "lastHeartbeat" : ISODate("2016-09-26T07:36:03.183Z"),
                    "lastHeartbeatRecv" : ISODate("2016-09-26T07:36:03.715Z"),
                    "pingMs" : NumberLong(0),
                    "configVersion" : 6
            },
            {
                    "_id" : 3,
                    "name" : "<IP>:<PORT>",
                    "health" : 1,
                    "state" : 2,
                    "stateStr" : "SECONDARY",
                    "uptime" : 1984305,
                    "optime" : Timestamp(1474875361, 16),
                    "optimeDate" : ISODate("2016-09-26T07:36:01Z"),
                    "lastHeartbeat" : ISODate("2016-09-26T07:36:02.921Z"),
                    "lastHeartbeatRecv" : ISODate("2016-09-26T07:36:03.793Z"),
                    "pingMs" : NumberLong(22),
                    "lastHeartbeatMessage" : "syncing from: 10.9.52.56:10050",
                    "syncingTo" : "10.9.52.56:10050",
                    "configVersion" : 6
            }
    ],
    "ok" : 1
    }

```

From the above we can know the entire replica set status

