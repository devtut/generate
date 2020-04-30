---
metaTitle: "PRAGMA Statements"
description: "PRAGMAs with permanent effects"
---

# PRAGMA Statements



## PRAGMAs with permanent effects


Most PRAGMA statements affect only the current database connection, which means that they have to be re-applied whenever the database has been opened.

However, the following PRAGMAs write to the database file, and can be executed at any time (but in some cases, not inside a transaction):

- [application_id](http://www.sqlite.org/pragma.html#pragma_application_id)
- [journal_mode](http://www.sqlite.org/pragma.html#pragma_journal_mode) when enabling or disabling [WAL mode](http://www.sqlite.org/wal.html)
- [schema_version](http://www.sqlite.org/pragma.html#pragma_schema_version)
- [user_version](http://www.sqlite.org/pragma.html#pragma_schema_version)
- [wal_checkpoint](http://www.sqlite.org/pragma.html#pragma_wal_checkpoint)

The following PRAGMA settings set properties of the database file which cannot be changed after creation, so they must be executed before the first actual write to the database:

- [auto_vacuum](http://www.sqlite.org/pragma.html#pragma_auto_vacuum) (can also be changed before [VACUUM](http://www.sqlite.org/lang_vacuum.html))
- [encoding](http://www.sqlite.org/pragma.html#pragma_encoding)
- [legacy_file_format](http://www.sqlite.org/pragma.html#pragma_legacy_file_format)
- [page_size](http://www.sqlite.org/pragma.html#pragma_page_size) (can also be changed before [VACUUM](http://www.sqlite.org/lang_vacuum.html))

For example:

```sql
-- open a connection to a not-yet-existing DB file
PRAGMA page_size = 4096;
PRAGMA auto_vacuum = INCREMENTAL;
CREATE TABLE t(x);              -- database is created here, with the above settings

```



#### Remarks


The SQLite documentation has a [reference of all PRAGMA statements](http://www.sqlite.org/pragma.html).

