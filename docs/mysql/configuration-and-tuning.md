---
metaTitle: "Configuration and tuning"
description: "InnoDB performance, Parameter to allow huge data to insert, Increase the string limit for group_concat, Minimal InnoDB configuration, Secure MySQL encryption"
---

# Configuration and tuning



## InnoDB performance


There are hundreds of settings that can be placed in my.cnf.  For the 'lite' user of MySQL, they won't matter as much.

Once your database becomes non-trivial, it is advisable to set the following parameters:

```sql
innodb_buffer_pool_size

```

This should be set to about 70% of **available** RAM (if you have at least 4GB of RAM; a smaller percentage if you have a tiny VM or antique machine).  The setting controls the amount of cache used by the InnoDB ENGINE.  Hence, it is very important for performance of InnoDB.



## Parameter to allow huge data to insert


If you need to store images or videos in the column then we need to change the value as needed by your application

max_allowed_packet = 10M

M is Mb, G in Gb, K in Kb



## Increase the string limit for group_concat


`group_concat` is used to concatenate non-null values in a `group`. The maximum length of the resulting string can be set using the `group_concat_max_len` option:

```sql
SET [GLOBAL | SESSION] group_concat_max_len = val;

```

Setting the `GLOBAL` variable will ensure a permanent change, whereas setting the `SESSION` variable will set the value for the current session.



## Minimal InnoDB configuration


This is a bare minimum setup for MySQL servers using InnoDB tables. Using InnoDB, query cache is not required. Reclaim disk space when a table or database is `DROP`ed. If you're using SSDs, flushing is a redundant operation (SDDs are not sequential).

```sql
default_storage_engine = InnoDB
query_cache_type = 0
innodb_file_per_table = 1
innodb_flush_neighbors = 0

```

**Concurrency**

Make sure we can create more than than the default 4 threads by setting `innodb_thread_concurrency` to infinity (0); this lets InnoDB decide based on optimal execution.

```sql
innodb_thread_concurrency = 0
innodb_read_io_threads = 64
innodb_write_io_threads = 64

```

**Hard drive utilization**

Set the capacity (normal load) and capacity_max (absolute maximum) of IOPS for MySQL. The default of 200 is fine for HDDs, but these days, with SSDs capable of thousands of IOPS, you are likely to want to adjust this number. There are many tests you can run to determine IOPS. The values above should be nearly that limit **if you are running a dedicated MySQL server**. If you are running any other services on the same machine, you should apportion as appropriate.

```sql
innodb_io_capacity = 2500
innodb_io_capacity_max = 3000

```

**RAM utilization**

Set the RAM available to MySQL. Whilst the rule of thumb is 70-80%, this really depends on whether or not your instance is dedicated to MySQL, and how much RAM is available. Don't **waste** RAM (i.e. resources) if you have a lot available.

```sql
innodb_buffer_pool_size = 10G

```



## Secure MySQL encryption


The default encryption `aes-128-ecb` uses Electronic Codebook (ECB) mode, which is insecure and should never be used. Instead, add the following to your configuration file:

```sql
block_encryption_mode = aes-256-cbc

```



#### Remarks


Configuration happens in one of 3 ways:

- command line options
- the `my.cnf` configuration file
- setting variables from within the server

Command Line options takes the form `mysqld --long-parameter-name=value --another-parameter`. The same parameters can be placed in the `my.conf` configuration file. **Some** parameters are configurable using system variables from within MySQL. Check the official documentation for a complete list of parameters.

Variables can have dash `-` or underscore `_`.  Spaces may exist around the `=`.  Large numbers can be suffixed by `K`, `M`, `G` for kilo-, mega-, and giga-.  One setting per line.

Flags:  Usually `ON` and `1` are synonymous, ditto for `OFF` and `0`.  Some flags have nothing after them.

When placing the settings in `my.cnf`, all settings for the **server** must be in the `[mysqld]` section, so don't blindly add settings to the end of the file.  (Note: For tools that allow multiple mysql instances to share one my.cnf, the section names may be different.)

