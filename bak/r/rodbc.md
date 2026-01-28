---
metaTitle: "R - RODBC"
description: "Connecting to Excel Files via RODBC, SQL Server Management Database connection to get individual table, Connecting to relational databases"
---

# RODBC



## Connecting to Excel Files via RODBC


While `RODBC` is restricted to Windows computers with compatible architecture between R and any target RDMS, one of its key flexibilities is to work with Excel files as if they were SQL databases.

```r
require(RODBC)
con = odbcConnectExcel("myfile.xlsx") # open a connection to the Excel file
sqlTables(con)$TABLE_NAME # show all sheets
df = sqlFetch(con, "Sheet1") # read a sheet
df = sqlQuery(con, "select * from [Sheet1 $]") # read a sheet (alternative SQL syntax)
close(con) # close the connection to the file

```



## SQL Server Management Database connection to get individual table


Another use of RODBC is in connecting with SQL Server Management Database.  We need to specify the 'Driver' i.e. SQL Server here, the database name "Atilla" and then use the `sqlQuery` to extract either the full table or a fraction of it.

```r
library(RODBC) 
cn  <- odbcDriverConnect(connection="Driver={SQL Server};server=localhost;database=Atilla;trusted_connection=yes;")
tbl <- sqlQuery(cn, 'select top 10 * from table_1')

```



## Connecting to relational databases


```r
library(RODBC)
con <- odbcDriverConnect("driver={Sql Server};server=servername;trusted connection=true")
dat <- sqlQuery(con, "select * from table");
close(con)

```

This will connect to a SQL Server instance.  For more information on what your connection string should look like, visit [connectionstrings.com](http://connectionstrings.com)

Also, since there's no database specified, you should make sure you fully qualify the object you're wanting to query like this databasename.schema.objectname

