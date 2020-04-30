---
metaTitle: "MyISAM Engine"
description: "ENGINE=MyISAM"
---

# MyISAM Engine



## ENGINE=MyISAM


```sql
CREATE TABLE foo (
     ...
) ENGINE=MyISAM;

```



#### Remarks


Over the years, InnoDB has improved to the point where it is almost always better than MyISAM, at least the currently supported versions.  Bottom line:  Don't use MyISAM, except maybe for tables that are truly temporary.

One advantage of MyISAM over InnoDB remains:  It is 2x-3x smaller in space required on disk.

When InnoDB first came out, MyISAM was still a viable Engine.  But with the advent of XtraDB and 5.6, InnoDB became "better" than MyISAM in most benchmarks.

Rumor has it that the next major version will eliminate the need for MyISAM by making truly temporary InnoDB tables and by moving the system tables into InnoDB.

