---
metaTitle: "MongoDB - Configuration"
description: "Starting mongo with a specific config file"
---

# Configuration



## Starting mongo with a specific config file


Using the `--config` flag.

```js
$ /bin/mongod --config /etc/mongod.conf
$ /bin/mongos --config /etc/mongos.conf

```

**Note that `-f` is the shorter synonym for `--config`.**



#### Parameters


|Parameter|Default
|---|---|---|---|---|---|---|---|---|---
|systemLog.verbosity|0
|systemLog.quiet|false
|systemLog.traceAllExceptions|false
|systemLog.syslogFacility|user
|systemLog.path|-
|systemLog.logAppend|false
|systemLog.logRotate|rename
|systemLog.destination|stdout
|systemLog.timeStampFormat|iso8601-local
|systemLog.component.accessControl.verbosity|0
|systemLog.component.command.verbosity|0
|systemLog.component.control.verbosity|0
|systemLog.component.ftdc.verbosity|0
|systemLog.component.geo.verbosity|0
|systemLog.component.index.verbosity|0
|systemLog.component.network.verbo|0
|systemLog.component.query.verbosity|0
|systemLog.component.replication.verbosity|0
|systemLog.component.sharding.verbosity|0
|systemLog.component.storage.verbosity|0
|systemLog.component.storage.journal.verbosity|0
|systemLog.component.write.verbosity|0
|processManagement.fork|false
|processManagement.pidFilePath|none
|net.port|27017
|net.bindIp|0.0.0.0
|net.maxIncomingConnections|65536
|net.wireObjectCheck|true
|net.ipv6|false
|net.unixDomainSocket.enabled|true
|net.unixDomainSocket.pathPrefix|/tmp
|net.unixDomainSocket.filePermissions|0700
|net.http.enabled|false
|net.http.JSONPEnabled|false
|net.http.RESTInterfaceEnabled|false
|net.ssl.sslOnNormalPorts|false
|net.ssl.mode|disabled
|net.ssl.PEMKeyFile|none
|net.ssl.PEMKeyPassword|none
|net.ssl.clusterFile|none
|net.ssl.clusterPassword|none
|net.ssl.CAFile|none
|net.ssl.CRLFile|none
|net.ssl.allowConnectionsWithoutCertificates|false
|net.ssl.allowInvalidCertificates|false
|net.ssl.allowInvalidHostnames|false
|net.ssl.disabledProtocols|none
|net.ssl.FIPSMode|false

