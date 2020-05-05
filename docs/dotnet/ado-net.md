---
metaTitle: ".NET Framework - ADO.NET"
description: "Best Practices - Executing Sql Statements, Executing SQL statements as a command, Using common interfaces to abstract away vendor specific classes"
---

# ADO.NET


ADO(ActiveX Data Objects).Net is a tool provided by Microsoft which provides access to data sources such as SQL Server, Oracle, and XML through its components. .Net front-end applications can retrieve, create, and manipulate data, once they are connected to a data source through ADO.Net with appropriate privileges.

ADO.Net provides a connection-less architecture. It is a secure approach to interact with a database, since, the connection doesn't have to be maintained during the entire session.



## Best Practices - Executing Sql Statements


```dotnet
public void SaveNewEmployee(Employee newEmployee)
{
    // best practice - wrap all database connections in a using block so they are always closed & disposed even in the event of an Exception
    // best practice - retrieve the connection string by name from the app.config or web.config (depending on the application type) (note, this requires an assembly reference to System.configuration)
    using(SqlConnection con = new SqlConnection(System.Configuration.ConfigurationManager.ConnectionStrings["MyConnectionName"].ConnectionString))
    {
        // best practice - use column names in your INSERT statement so you are not dependent on the sql schema column order
        // best practice - always use parameters to avoid sql injection attacks and errors if malformed text is used like including a single quote which is the sql equivalent of escaping or starting a string (varchar/nvarchar)
        // best practice - give your parameters meaningful names just like you do variables in your code
        using(SqlCommand sc = new SqlCommand("INSERT INTO employee (FirstName, LastName, DateOfBirth /*etc*/) VALUES (@firstName, @lastName, @dateOfBirth /*etc*/)", con))
        {
            // best practice - always specify the database data type of the column you are using
            // best practice - check for valid values in your code and/or use a database constraint, if inserting NULL then use System.DbNull.Value
            sc.Parameters.Add(new SqlParameter("@firstName", SqlDbType.VarChar, 200){Value = newEmployee.FirstName ?? (object) System.DBNull.Value});
            sc.Parameters.Add(new SqlParameter("@lastName", SqlDbType.VarChar, 200){Value = newEmployee.LastName ?? (object) System.DBNull.Value});

            // best practice - always use the correct types when specifying your parameters, Value is assigned to a DateTime instance and not a string representation of a Date
            sc.Parameters.Add(new SqlParameter("@dateOfBirth", SqlDbType.Date){ Value = newEmployee.DateOfBirth });

            // best practice - open your connection as late as possible unless you need to verify that the database connection is valid and wont fail and the proceeding code execution takes a long time (not the case here)
            con.Open();
            sc.ExecuteNonQuery();
        }

        // the end of the using block will close and dispose the SqlConnection
        // best practice - end the using block as soon as possible to release the database connection
    }
}

// supporting class used as parameter for example
public class Employee
{
    public string FirstName { get; set; }
    public string LastName { get; set; }
    public DateTime DateOfBirth { get; set; }
}

```

### Best practice for working with [ADO.NET](https://msdn.microsoft.com/en-us/library/h43ks021(v=vs.110).aspx)

- Rule of thumb is to open connection for minimal time. Close the connection explicitly once your procedure execution is over this will return the connection object back to connection pool. Default connection pool max size = 100. As connection pooling enhances the performance of physical connection to SQL Server.[Connection Pooling in SQL Server](https://msdn.microsoft.com/en-us/library/8xx3tyca(v=vs.110).aspx)
- Wrap all database connections in a using block so they are always closed & disposed even in the event of an Exception. See [using Statement (C# Reference)](https://msdn.microsoft.com/en-us/library/yh598w02.aspx) for more information on using statements
<li>Retrieve the connection strings by name from the app.config or web.config (depending on the application type)
<ul>
- This requires an assembly reference to `System.configuration`
- See [Connection Strings and Configuration Files](https://msdn.microsoft.com/en-us/library/ms254494(v=vs.110).aspx) for additional information on how to structure your configuration file

- Avoid [sql injection](https://en.wikipedia.org/wiki/SQL_injection) attacks
- Avoid errors if malformed text is used like including a single quote which is the sql equivalent of escaping or starting a string (varchar/nvarchar)
- Letting the database provider reuse query plans (not supported by all database providers) which increases efficiency

- Sql parameters type and size mismatch is a common cause of insert/ updated/ select failure
- Give your Sql parameters meaningful names just like you do variables in your code
- Specify the database data type of the column you are using, this ensures the wrong parameter types is not used which could lead to unexpected results
- Validate your incoming parameters before you pass them into the command (as the saying goes, "[garbage in, garbage out](https://en.wikipedia.org/wiki/Garbage_in,_garbage_out)"). Validate incoming values as early as possible in the stack
- Use the correct types when assigning your parameter values, example: do not assign the string value of a DateTime, instead assign an actual DateTime instance to the value of the parameter
- Specify the [size](https://msdn.microsoft.com/en-us/library/system.data.sqlclient.sqlparameter.size(v=vs.110).aspx) of string-type parameters. This is because SQL Server can re-use execution plans if the parameters match in type **and** size. Use -1 for MAX
- Do not use the method [AddWithValue](https://msdn.microsoft.com/en-us/library/system.data.sqlclient.sqlparametercollection.addwithvalue(v=vs.110).aspx), the main reason is it is very easy to forget to specify the parameter type or the precision/scale when needed. For additional information see [Can we stop using AddWithValue already?](http://blogs.msmvps.com/jcoehoorn/blog/2014/05/12/can-we-stop-using-addwithvalue-already/)

- Open the connection as late as possible and close it as soon as possible. This is a general guideline when working with any external resource
<li>Never share database connection instances (example: having a singleton host a shared instance of type `SqlConnection`). Have your code always create a new database connection instance when needed and then have the calling code dispose of it and "throw it away" when it is done. The reason for this is
<ul>
- Most database providers have some sort of connection pooling so creating new managed connections is cheap
- It eliminates any future errors if the code starts working with multiple threads



## Executing SQL statements as a command


```dotnet
// Uses Windows authentication. Replace the Trusted_Connection parameter with
// User Id=...;Password=...; to use SQL Server authentication instead. You may
// want to find the appropriate connection string for your server.
string connectionString = @"Server=myServer\myInstance;Database=myDataBase;Trusted_Connection=True;"

string sql = "INSERT INTO myTable (myDateTimeField, myIntField) " +
    "VALUES (@someDateTime, @someInt);";

// Most ADO.NET objects are disposable and, thus, require the using keyword.
using (var connection = new SqlConnection(connectionString))
using (var command = new SqlCommand(sql, connection))
{
    // Use parameters instead of string concatenation to add user-supplied
    // values to avoid SQL injection and formatting issues. Explicitly supply datatype.

    // System.Data.SqlDbType is an enumeration. See Note1
    command.Parameters.Add("@someDateTime", SqlDbType.DateTime).Value = myDateTimeVariable;
    command.Parameters.Add("@someInt", SqlDbType.Int).Value = myInt32Variable;

    // Execute the SQL statement. Use ExecuteScalar and ExecuteReader instead
    // for query that return results (or see the more specific examples, once
    // those have been added).

    connection.Open();
    command.ExecuteNonQuery();
}

```

****Note 1:**** Please see [SqlDbType Enumeration](https://msdn.microsoft.com/en-us/library/system.data.sqldbtype(v=vs.110).aspx) for the MSFT SQL Server-specific variation.

****Note 2:**** Please see [MySqlDbType Enumeration](https://dev.mysql.com/doc/dev/connector-net/html/T_MySql_Data_MySqlClient_MySqlDbType.htm) for the MySQL-specific variation.



## Using common interfaces to abstract away vendor specific classes


```dotnet
var providerName = "System.Data.SqlClient";    //Oracle.ManagedDataAccess.Client, IBM.Data.DB2
var connectionString = "{your-connection-string}";
//you will probably get the above two values in the ConnectionStringSettings object from .config file

var factory = DbProviderFactories.GetFactory(providerName);
using(var connection = factory.CreateConnection()) {    //IDbConnection
    connection.ConnectionString = connectionString;
    connection.Open();
    
    using(var command = connection.CreateCommand()) {    //IDbCommand
        command.CommandText = "{query}";
        
        using(var reader = command.ExecuteReader()) {    //IDataReader
            while(reader.Read()) {
                ...
            }
        }
    }
}

```



#### Remarks


**A note on parameterizing SQLs with `Parameters.AddWithValue`:** `AddWithValue` is never a good starting point. That method relies on inferring the type of the data from what is passed in. With this, you might end up in a situation where the conversion prevents your query from [using an index](http://stackoverflow.com/q/799584/87698). Note that some SQL Server data types, such as `char`/`varchar` (without preceding "n") or `date` do not have a corresponding .NET data type. In those cases, [`Add` with the correct data type should be used instead](http://blogs.msmvps.com/jcoehoorn/blog/2014/05/12/can-we-stop-using-addwithvalue-already/).

