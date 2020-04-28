---
metaTitle: "Using SQLSRV"
description: "Fetching Query Results, Retrieving Error Messages, Creating a Connection, Making a Simple Query, Invoking a Stored Procedure, Making a Parameterised Query"
---

# Using SQLSRV



## Fetching Query Results


There are 3 main ways to fetch results from a query:

### sqlsrv_fetch_array()

`sqlsrv_fetch_array()` retrieves the next row as an array.

```php
$stmt = sqlsrv_query($conn, $query);

while($row = sqlsrv_fetch_array($stmt)) {
    echo $row[0];
    $var = $row["name"];
    //...
}

```

`sqlsrv_fetch_array()` has an optional second parameter to fetch back different types of array: `SQLSRV_FETCH_ASSOC`, `SQLSRV_FETCH_NUMERIC` and `SQLSRV_FETCH_BOTH`**(default)** can be used; each returns the associative, numeric, or associative and numeric arrays, respectively.

### sqlsrv_fetch_object()

`sqlsrv_fetch_object()` retrieves the next row as an object.

```php
$stmt = sqlsrv_query($conn, $query);

while($obj = sqlsrv_fetch_object($stmt)) {
    echo $obj->field; // Object property names are the names of the fields from the query
    //...
}

```

### sqlsrv_fetch()

`sqlsrv_fetch()` makes the next row available for reading.

```php
$stmt = sqlsrv_query($conn, $query);

while(sqlsrv_fetch($stmt) === true) {
    $foo = sqlsrv_get_field($stmt, 0); //gets the first field -
}

```



## Retrieving Error Messages


When a query goes wrong, it is important to fetch the error message(s) returned by the driver to identify the cause of the problem. The syntax is:

```php
sqlsrv_errors([int $errorsOrWarnings]);

```

This returns an array with:

|Key|Description
|------
|SQLSTATE|The state that the SQL Server / OBDC Driver is in
|code|The SQL Server error code
|message|The description of the error

It is common to use the above function like so:

```php
$brokenQuery = "SELECT BadColumnName FROM Table_1";
$stmt = sqlsrv_query($conn, $brokenQuery);

if ($stmt === false) {
    if (($errors = sqlsrv_errors()) != null) {
        foreach ($errors as $error) {
            echo "SQLSTATE: ".$error['SQLSTATE']."<br />";
            echo "code: ".$error['code']."<br />";
            echo "message: ".$error['message']."<br />";
        }
    }
}

```



## Creating a Connection


```php
$dbServer = "localhost,1234"; //Name of the server/instance, including optional port number (default is 1433)
$dbName = "db001"; //Name of the database
$dbUser = "user"; //Name of the user
$dbPassword = "password"; //DB Password of that user

$connectionInfo = array(
    "Database" => $dbName, 
    "UID" => $dbUser,
    "PWD" => $dbPassword
);

$conn = sqlsrv_connect($dbServer, $connectionInfo);

```

SQLSRV also has a PDO Driver. To connect using PDO:

```php
$conn = new PDO("sqlsrv:Server=localhost,1234;Database=db001", $dbUser, $dbPassword);

```



## Making a Simple Query


```php
//Create Connection
$conn = sqlsrv_connect($dbServer, $connectionInfo);

$query = "SELECT * FROM [table]"; 
$stmt = sqlsrv_query($conn, $query);

```

**Note: the use of square brackets** `[]` **is to escape the word** `table` **as it is a [reserved word](https://msdn.microsoft.com/en-us/library/ms189822.aspx). These work in the same way as backticks** ```php **do in MySQL**.



## Invoking a Stored Procedure


To call a stored procedure on the server:

```php
$query = "{call [dbo].[myStoredProcedure](?,?,?)}"; //Parameters '?' includes OUT parameters

$params = array(
    array($name, SQLSRV_PARAM_IN),
    array($age, SQLSRV_PARAM_IN),
    array($count, SQLSRV_PARAM_OUT, SQLSRV_PHPTYPE_INT) //$count must already be initialised
);

$result = sqlsrv_query($conn, $query, $params);

```



## Making a Parameterised Query


```php
$conn = sqlsrv_connect($dbServer, $connectionInfo);

$query = "SELECT * FROM [users] WHERE [name] = ? AND [password] = ?";
$params = array("joebloggs", "pa55w0rd");

$stmt = sqlsrv_query($conn, $query, $params);

```

If you plan on using the same query statement more than once, with different parameters, the same can be achieved with the `sqlsrv_prepare()` and `sqlsrv_execute()` functions, as shown below:

```php
$cart = array(
    "apple" => 3,
    "banana" => 1,
    "chocolate" => 2
);

$query = "INSERT INTO [order_items]([item], [quantity]) VALUES(?,?)";
$params = array(&$item, &$qty); //Variables as parameters must be passed by reference

$stmt = sqlsrv_prepare($conn, $query, $params);

foreach($cart as $item => $qty){
    if(sqlsrv_execute($stmt) === FALSE) {
        die(print_r(sqlsrv_errors(), true));
    }
}

```



#### Remarks


The SQLSRV driver is a Microsoft supported PHP extension that allows you to access Microsoft SQL Server and SQL Azure databases. It is an alternative for the MSSQL drivers that were deprecated as of PHP 5.3, and have been removed from PHP 7.

The SQLSRV extension can be used on the following operating systems:

- Windows Vista Service Pack 2 or later
- Windows Server 2008 Service Pack 2 or later
- Windows Server 2008 R2
- Windows 7

The SQLSRV extension requires that the Microsoft SQL Server 2012 Native Client be installed on the same computer that is running PHP. If the Microsoft SQL Server 2012 Native Client is not already installed, click the appropriate link at the ["Requirements" documentation page](http://php.net/manual/en/sqlsrv.requirements.php).

To download the latest SQLSRV drivers, go to the following: [Download](https://msdn.microsoft.com/en-us/library/mt683517.aspx)

A full list of system requirements for the SQLSRV Drivers can be found here: [System Requirements](https://msdn.microsoft.com/en-us/library/cc296170.aspx)

Those using SQLSRV 3.1+ must download the [Microsoft ODBC Driver 11 for SQL Server](https://www.microsoft.com/en-us/download/details.aspx?id=36434)

PHP7 users can download the latest drivers from [GitHub](https://github.com/Azure/msphpsql/tree/PHP-7.0)

[Microsoft® ODBC Driver 13 for SQL Server](https://www.microsoft.com/en-us/download/details.aspx?id=50420) supports Microsoft SQL Server 2008, SQL Server 2008 R2, SQL Server 2012, SQL Server 2014, SQL Server 2016 (Preview), Analytics Platform System, Azure SQL Database and Azure SQL Data Warehouse.

