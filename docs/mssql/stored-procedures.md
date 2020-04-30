---
metaTitle: "Stored Procedures"
description: "Creating and executing a basic stored procedure, Stored Procedure with If...Else and Insert Into operation, Dynamic SQL in stored procedure, STORED PROCEDURE with OUT parameters, Simple Looping, Simple Looping"
---

# Stored Procedures


In SQL Server, a procedure is a stored program that you can pass parameters into. It does not return a value like a function does. However, it can return a success/failure status to the procedure that called it.



## Creating and executing a basic stored procedure


Using the `Authors` table in the [Library Database](http://stackoverflow.com/documentation/sql/280/example-databases/4978/library-database#t=20160722132728048284)

```sql
CREATE PROCEDURE GetName
(
    @input_id INT = NULL,      --Input parameter,  id of the person, NULL default
    @name VARCHAR(128) = NULL  --Input parameter, name of the person, NULL default
) 
AS
BEGIN 
    SELECT Name + ' is from ' + Country 
    FROM Authors 
    WHERE Id = @input_id OR Name = @name
END 
GO

```

You can execute a procedure with a few different syntaxes. First, you can use `EXECUTE` or `EXEC`

```sql
EXECUTE GetName @id = 1
EXEC Getname @name = 'Ernest Hemingway'

```

Additionally, you can omit the EXEC command. Also, you don't have to specify what parameter you are passing in, as you pass in all parameters.

```sql
GetName NULL, 'Ernest Hemingway'

```

When you want to specify the input parameters in a different order than how they are declared in the procedure you can specify the parameter name and assign values. For example

```

CREATE PROCEDURE dbo.sProcTemp 
 (
    @Param1 INT,
    @Param2 INT
)
AS
BEGIN

    SELECT
        Param1 = @Param1,
        Param2 = @Param2

END

```

the normal order to execute this procedure is to specify the value for @Param1 first and then @Param2 second. So it will look something like this

```

 EXEC dbo.sProcTemp @Param1 = 0,@Param2=1

```

But it's also possible that you can use the following

```

 EXEC dbo.sProcTemp @Param2 = 0,@Param1=1

```

in this, you are specifying the value for @param2 first and @Param1 second. Which means you do not have to keep the same order as it is declared in the procedure but you can have any order as you wish. but you will need to specify to which parameter you are setting the value

**Access stored procedure from any database**

And also you can create a procedure with a prefix `sp_` these procuedres, like all system stored procedures, can be executed without specifying the database because of the default behavior of SQL Server. When you execute a stored procedure that starts with "sp_", SQL Server looks for the procedure in the master database first. If the procedure is not found in master, it looks in the active database. If you have a stored procedure that you want to access from all your databases, create it in master and use a name that includes the "sp_" prefix.

```sql
Use Master

CREATE PROCEDURE sp_GetName
(
    @input_id INT = NULL,      --Input parameter,  id of the person, NULL default
    @name VARCHAR(128) = NULL  --Input parameter, name of the person, NULL default
) 
AS
BEGIN 
    SELECT Name + ' is from ' + Country 
    FROM Authors 
    WHERE Id = @input_id OR Name = @name
END 
GO

```



## Stored Procedure with If...Else and Insert Into operation


Create example table `Employee`:

```sql
CREATE TABLE Employee
(
    Id INT,
    EmpName VARCHAR(25),
    EmpGender VARCHAR(6),
    EmpDeptId INT
)

```

Creates stored procedure that checks whether the values passed in stored procedure are not null or non empty and perform insert operation in Employee table.

```sql
CREATE PROCEDURE spSetEmployeeDetails
(
    @ID int,
    @Name VARCHAR(25),
    @Gender VARCHAR(6),
    @DeptId INT
)
AS
BEGIN
    IF (
        (@ID IS NOT NULL AND LEN(@ID) !=0) 
        AND (@Name IS NOT NULL AND LEN(@Name) !=0)
        AND (@Gender IS NOT NULL AND LEN(@Gender) !=0)
        AND (@DeptId IS NOT NULL AND LEN(@DeptId) !=0)
    )
    BEGIN
        INSERT INTO Employee
        (
            Id,
            EmpName,
            EmpGender,
            EmpDeptId
        )
        VALUES
        (
            @ID,
            @Name,
            @Gender,
            @DeptId
        )
    END
ELSE
    PRINT 'Incorrect Parameters'
END
GO

```

Execute the stored procedure

```sql
DECLARE @ID INT,
    @Name VARCHAR(25),
    @Gender VARCHAR(6),
    @DeptId INT

EXECUTE spSetEmployeeDetails
    @ID = 1,
    @Name = 'Subin Nepal',
    @Gender = 'Male',
    @DeptId = 182666 

```



## Dynamic SQL in stored procedure


Dynamic SQL enables us to generate and run SQL statements at run time. Dynamic SQL is needed when our SQL statements contains identifier that may change at different compile times.

Simple Example of dynamic SQL:

```sql
CREATE PROC sp_dynamicSQL
@table_name      NVARCHAR(20),
@col_name        NVARCHAR(20), 
@col_value       NVARCHAR(20) 
AS
BEGIN
DECLARE  @Query  NVARCHAR(max)
SET      @Query = 'SELECT * FROM ' + @table_name
SET      @Query = @Query + ' WHERE ' + @col_name + ' = ' + ''''+@col_value+''''
EXEC     (@Query)
END

```

In the above sql query, we can see that we can use above query by defining values in `@table_name, @col_name, and @col_value` at run time. The query is generated at runtime and executed. This is technique in which we can create whole scripts as string in a variable and execute it. We can create more complex queries using dynamic SQL and concatenation concept. This concept is very powerful when you want to create a script that can be used under several conditions.

Executing stored procedure

```sql
DECLARE @table_name     NVARCHAR(20) = 'ITCompanyInNepal',
        @col_name       NVARCHAR(20) = 'Headquarter',
        @col_value      NVARCHAR(20) = 'USA'
    
EXEC    sp_dynamicSQL   @table_name,    
                        @col_name,
                        @col_value

```

Table I have used

[<img src="http://i.stack.imgur.com/dxCe3.png" alt="enter image description here" />](http://i.stack.imgur.com/dxCe3.png)

Output

[<img src="http://i.stack.imgur.com/kwVch.png" alt="enter image description here" />](http://i.stack.imgur.com/kwVch.png)



## STORED PROCEDURE with OUT parameters


Stored procedures can return values using the `OUTPUT` keyword in its parameter list.

### Creating a stored procedure with a single out parameter

```sql
CREATE PROCEDURE SprocWithOutParams
(
    @InParam VARCHAR(30),
    @OutParam VARCHAR(30) OUTPUT
)
AS
BEGIN
    SELECT @OutParam = @InParam + ' must come out'   
    RETURN
END   
GO

```

### Executing the stored procedure

```sql
DECLARE @OutParam VARCHAR(30)    
EXECUTE SprocWithOutParams 'what goes in', @OutParam OUTPUT   
PRINT @OutParam 

```

### Creating a stored procedure with multiple out parameters

```sql
CREATE PROCEDURE SprocWithOutParams2
(
    @InParam VARCHAR(30),
    @OutParam VARCHAR(30) OUTPUT,
    @OutParam2 VARCHAR(30) OUTPUT
)
AS
BEGIN
    SELECT @OutParam = @InParam +' must come out'   
    SELECT @OutParam2 = @InParam +' must come out'
    RETURN
END    
GO

```

### Executing the stored procedure

```sql
DECLARE @OutParam VARCHAR(30)    
DECLARE @OutParam2 VARCHAR(30)  
EXECUTE SprocWithOutParams2 'what goes in', @OutParam OUTPUT, @OutParam2 OUTPUT   
PRINT @OutParam 
PRINT @OutParam2

```



## Simple Looping


First lets get some data into a temp table named `#systables` and ad a incrementing row number so we can query one record at a time

```sql
select
    o.name,
    row_number() over (order by o.name) as rn
into
    #systables
from
    sys.objects as o
where
    o.type = 'S'

```

Next we declare some variables to control the looping and store the table name in this example

```sql
declare
    @rn int = 1,
    @maxRn int = (
                    select
                        max(rn)
                    from
                        #systables as s
                    )
declare    @tablename sys name

```

Now we can loop using a simple while. We increment `@rn` in the `select` statement but this could also have been a separate statement for ex `set @rn = @rn + 1` it will depend on your requirements. We also use the value of `@rn` before it's incremented to select a single record from `#systables`. Lastly we print the table name.

```sql
while @rn <= @maxRn
    begin

        select
            @tablename = name,
            @rn = @rn + 1
        from
            #systables as s
        where
            s.rn = @rn

        print @tablename
    end

```



## Simple Looping


```sql
CREATE PROCEDURE SprocWithSimpleLoop
(
    @SayThis VARCHAR(30),
    @ThisManyTimes INT
)
AS
BEGIN
    WHILE @ThisManyTimes > 0
    BEGIN
        PRINT @SayThis;
        SET @ThisManyTimes = @ThisManyTimes - 1;
    END
    
    RETURN;
END    
GO

```



#### Syntax


- CREATE { PROCEDURE | PROC } [schema_name.]procedure_name
- [ @parameter [type_schema_name.] datatype
- [ VARYING ] [ = default ] [ OUT | OUTPUT | READONLY ]
- , @parameter [type_schema_name.] datatype
- [ VARYING ] [ = default ] [ OUT | OUTPUT | READONLY ] ]
- [ WITH { ENCRYPTION | RECOMPILE | EXECUTE AS Clause } ]
- [ FOR REPLICATION ]
- AS
- BEGIN
- [declaration_section]
- executable_section
- END;

