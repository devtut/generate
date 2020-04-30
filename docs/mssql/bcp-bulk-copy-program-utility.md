---
metaTitle: "bcp (bulk copy program) Utility"
description: "Example to Import Data without a Format File(using Native Format )"
---

# bcp (bulk copy program) Utility


The bulk copy program utility (bcp) bulk copies data between an instance of Microsoft SQL Server and a data file in a user-specified format. The bcp utility can be used to import large numbers of new rows into SQL Server tables or to export data out of tables into data files.



## Example to Import Data without a Format File(using Native Format )


```sql
REM Truncate table (for testing)
SQLCMD -Q "TRUNCATE TABLE TestDatabase.dbo.myNative;"

REM Import data
bcp TestDatabase.dbo.myNative IN D:\BCP\myNative.bcp -T -n

REM Review results
SQLCMD -Q "SELECT * FROM TestDatabase.dbo.myNative;"

```

