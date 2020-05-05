---
metaTitle: "Microsoft SQL Server - Common Language Runtime Integration"
description: "Enable CLR on database, Adding .dll that contains Sql CLR modules, Create CLR Function in SQL Server, Create CLR User-defined type in SQL Server, Create CLR procedure in SQL Server"
---

# Common Language Runtime Integration



## Enable CLR on database


CLR procedures are not enabled by default. You need to run the following queries to enable CLR:

```sql
sp_configure 'show advanced options', 1;
GO
RECONFIGURE;
GO
sp_configure 'clr enabled', 1;
GO
RECONFIGURE;
GO

```

In addition, if some CLR module need external access, you should set TRUSTWORTHY property to ON in your database:

```sql
ALTER DATABASE MyDbWithClr SET TRUSTWORTHY ON

```



## Adding .dll that contains Sql CLR modules


Procedures, functions, triggers, and types written in .Net languages are stored in .dll files. Once you create .dll file containing CLR procedures you should import it into SQL Server:

```sql
CREATE ASSEMBLY MyLibrary
FROM 'C:\lib\MyStoredProcedures.dll'
    WITH PERMISSION_SET = EXTERNAL_ACCESS

```

PERMISSION_SET is Safe by default meaning that code in .dll don't need permission to access external resources (e.g. files, web sites, other servers), and that it will not use native code that can access memory.

PERMISSION_SET = EXTERNAL_ACCESS is used to mark assemblies that contain code that will access external resources.

you can find information about current CLR assembly files in sys.assemblies view:

```sql
SELECT *
FROM sys.assemblies asms
WHERE is_user_defined = 1

```



## Create CLR Function in SQL Server


If you have created .Net function, compiled it into .dll, and imported it into SQL server as an assembly, you can create user-defined function that references function in that assembly:

```sql
CREATE FUNCTION dbo.TextCompress(@input nvarchar(max)) 
RETURNS varbinary(max) 
AS EXTERNAL NAME MyLibrary.[Name.Space.ClassName].TextCompress 

```

You need to specify name of the function and signature with input parameters and return values that match .Net function. In AS EXTERNAL NAME clause you need to specify assembly name, namespace/class name where this function is placed and name of the method in the class that contains the code that will be exposed as function.

You can find information about the CLR functions using the following query:

```sql
SELECT * FROM dbo.sysobjects WHERE TYPE ='FS'

```



## Create CLR User-defined type in SQL Server


If you have create .Net class that represents some user-defined type, compiled it into .dll, and imported it into SQL server as an assembly, you can create user-defined function that references this class:

```sql
CREATE TYPE dbo.Point
EXTERNAL NAME MyLibrary.[Name.Space.Point]

```

You need to specify name of the type that will be used in T-SQL queries. In EXTERNAL NAME clause you need to specify assembly name, namespace, and class name.



## Create CLR procedure in SQL Server


If you have created .Net method in some class, compiled it into .dll, and imported it into SQL server as an assembly, you can create user-defined stored procedure that references method in that assembly:

```sql
CREATE PROCEDURE dbo.DoSomethng(@input nvarchar(max)) 
AS EXTERNAL NAME MyLibrary.[Name.Space.ClassName].DoSomething

```

You need to specify name of the procedure and signature with input parameters that match .Net method. In AS EXTERNAL NAME clause you need to specify assembly name, namespace/class name where this procedure is placed and name of the method in the class that contains the code that will be exposed as procedure.

