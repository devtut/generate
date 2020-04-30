---
metaTitle: "SQL Server Evolution through different versions (2000 - 2016)"
description: "SQL Server Version 2000 - 2016"
---

# SQL Server Evolution through different versions (2000 - 2016)


I am using SQL Server since 2004. I started with 2000 and now I am going to use SQL Server 2016. I created tables, views, functions, triggers, stored procedures and wrote many SQL queries but I did not use many new features from subsequent versions. I googled it but unfortunately, I did not find all the features in one place. So I gathered and validated these information from different sources and put here. I am just adding the high level information for all the versions starting from 2000 to 20



## SQL Server Version 2000 - 2016


**The following features added in SQL Server 2000 from its previous version:**

1. New data types were added (BIGINT, SQL_VARIANT, TABLE)
1. Instead of and for Triggers were introduced as advancement to the DDL.
1. Cascading referential integrity.
1. XML support
1. User defined functions and partition views.
1. Indexed Views (Allowing index on views with computed columns).

**The following features added in version 2005 from its previous version:**

1. Enhancement in TOP clause with “WITH TIES” option.
1. Data Manipulation Commands (DML) and OUTPUT clause to get INSERTED and DELETED values
1. The PIVOT and UNPIVOT operators.
1. Exception Handling with TRY/CATCH block
1. Ranking functions
1. Common Table Expressions (CTE)
1. Common Language Runtime (Integration of .NET languages to build objects like stored procedures, triggers, functions etc.)
1. Service Broker (Handling message between a sender and receiver in a loosely coupled manner)
1. Data Encryption (Native capabilities to support encryption of data stored in user defined databases)
1. SMTP mail
1. HTTP endpoints (Creation of endpoints using simple T-SQL statement exposing an object to be accessed over the internet)
1. Multiple Active Result Sets (MARS).This allows a persistent database connection from a single client to have more than one active request per connection.
1. SQL Server Integration Services (Will be used as a primary ETL (Extraction, Transformation and Loading) Tool
1. Enhancements in Analysis Services and Reporting Services.
1. Table and index partitioning. Allows partitioning of tables and indexes based on partition boundaries as specified by a PARTITION FUNCTION with individual partitions mapped to file groups via a PARTITION SCHEME.

**The following features added in version 2008 from its previous version:**

1. Enhancement in existing DATE and TIME Data Types
1. New functions like – SYSUTCDATETIME() and SYSDATETIMEOFFSET()
1. Spare Columns – To save a significant amount of disk space.
1. Large User Defined Types (up to 2 GB in size)
1. Introduced a new feature to pass a table datatype into stored procedures and functions
1. New MERGE command for INSERT, UPDATE and DELETE operations
1. New HierarchyID datatype
1. Spatial datatypes - To represent the physical location and shape of any geometric object.
1. Faster queries and reporting with GROUPING SETS - An extension to the GROUP BY clause.
1. Enhancement to FILESTREAM storage option

**The following features added in version 2008 R2 from its previous version:**

1. PowerPivot – For processing large data sets.
1. Report Builder 3.0
1. Cloud ready
1. StreamInsight
1. Master Data Services
1. SharePoint Integration
1. DACPAC (Data-tier Application Component Packages)
1. Enhancement in other features of SQL Server 2008

**The following features added in version 2012 from its previous version:**

1. Column store indexes - reduces I/O and memory utilization on large queries.
1. Pagination - pagination can be done by using “OFFSET” and “FETCH’ commands.
1. Contained database – Great feature for periodic data migrations.
1. AlwaysOn Availability Groups
1. Windows Server Core Support
1. User-Defined Server Roles
1. Big Data Support
1. PowerView
1. SQL Azure Enhancements
1. Tabular Model (SSAS)
1. DQS Data quality services
1. File Table - an enhancement to the FILESTREAM feature which was introduced in 2008.
1. Enhancement in Error Handling including THROW statement
<li>Improvement to SQL Server Management Studio Debugging
a.    SQL Server 2012 introduces more options to control breakpoints.
b.    Improvements to debug-mode windows<br />
c.    Enhancement in IntelliSense - like Inserting Code Snippets.</li>

**The following features added in version 2014 from its previous version:**

1. In-Memory OLTP Engine – Improves performance up to 20 times.
1. AlwaysOn Enhancements
1. Buffer Pool Extension
1. Hybrid Cloud Features
1. Enhancement in Column store Indexes (like Updatable Column store Indexes)
1. Query Handling Enhancements (like parallel SELECT INTO)
1. Power BI for Office 365 Integration
1. Delayed durability
1. Enhancements for Database Backups

**The following features added in version 2016 from its previous version:**

1. Always Encrypted - Always Encrypted is designed to protect data at rest or in motion.
1. Real-time Operational Analytics
1. PolyBase into SQL Server
1. Native JSON Support
1. Query Store
1. Enhancements to AlwaysOn
1. Enhanced In-Memory OLTP
1. Multiple TempDB Database Files
1. Stretch Database
1. Row Level Security
1. In-Memory Enhancements

> 
T-SQL Enhancements or new additions in SQL Server 2016


<li>
TRUNCATE TABLE with PARTITION
</li>
<li>
DROP IF EXISTS
</li>
<li>
STRING_SPLIT and STRING_ESCAPE Functions
</li>
<li>
ALTER TABLE can now alter many columns while the table remains online, using WITH (ONLINE = ON | OFF).
</li>
<li>
MAXDOP for DBCC CHECKDB, DBCC CHECKTABLE and DBCC CHECKFILEGROUP
</li>
<li>
ALTER DATABASE SET AUTOGROW_SINGLE_FILE
</li>
<li>
ALTER DATABASE SET AUTOGROW_ALL_FILES
</li>
<li>
COMPRESS and DECOMPRESS Functions
</li>
<li>
FORMATMESSAGE Statement
</li>
<li>
2016 introduces 8 more properties with SERVERPROPERTY
</li>

a.    InstanceDefaultDataPath

b.    InstanceDefaultLogPath

c.    ProductBuild

d.    ProductBuildType

e.    ProductMajorVersion

f.    ProductMinorVersion

g.    ProductUpdateLevel

h.    ProductUpdateReference

