---
metaTitle: "MongoDB - Managing MongoDB"
description: "Listing currently running queries"
---

# Managing MongoDB



## Listing currently running queries


The following command lists out the queries that are currently being run on the server

```js
db.currentOp()

```

The output looks something similar to this

```js
{
    "inprog" : [
        {
            "opid" : "302616759",
            "active" : true,
            "secs_running" : 1,
            "microsecs_running" : NumberLong(1167662),
            "op" : "getmore",
            "ns" : "local.oplog.rs",
            "query" : {
                
            },
            ...
        },
        {
            "desc" : "conn48",
            "threadId" : "0x114c00700",
            "connectionId" : 48,
            "opid" : "mdss_shard00:302616760",
            "active" : true,
            "secs_running" : 1,
            "microsecs_running" : NumberLong(1169659),
            "op" : "getmore",
            "ns" : "local.oplog.rs"
            ...
        }
    ]
}

```

The `inprog` attribute indicates the queries are currently in progress. The `opid` is Id of the query or operation. `secs_running` indicates the time for which it has been running. This is sometimes useful to identify long running queries.

