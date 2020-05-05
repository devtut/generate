---
metaTitle: "Microsoft SQL Server - Export data in txt file by using SQLCMD"
description: "By using SQLCMD on Command Prompt"
---

# Export data in txt file by using SQLCMD



## By using SQLCMD on Command Prompt


**Command Structure is**

sqlcmd -S yourservername\instancename -d database_name -o outputfilename_withpath -Q "your select query"

Switches are as follows

-S     for servername and instance name

-d     for source database

-o     for target outputfile (it will create output file)

-Q     for query to fetch data



#### Syntax


<li>sqlcmd -S SHERAZM-E7450\SQL2008R2 -d Baseline_DB_Aug_2016 -o
c:\employee.txt -Q "select * from employee"</li>

