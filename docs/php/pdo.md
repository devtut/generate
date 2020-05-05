---
metaTitle: "PHP - PDO"
description: "Preventing SQL injection with Parameterized Queries, Basic PDO Connection and Retrieval, Database Transactions with PDO, PDO: connecting to MySQL/MariaDB server, PDO: Get number of affected rows by a query, PDO::lastInsertId()"
---

# PDO


The [PDO](http://php.net/manual/en/book.pdo.php) (PHP Data Objects) extension allows developers to connect to numerous different types of databases and execute queries against them in a uniform, object oriented manner.



## Preventing SQL injection with Parameterized Queries


SQL injection is a kind of attack that allows a malicious user to modify the SQL query, adding unwanted commands to it. For example, the following code is **vulnerable**:

```php
// Do not use this vulnerable code!
$sql = 'SELECT name, email, user_level FROM users WHERE userID = ' . $_GET['user'];
$conn->query($sql);

```

This allows any user of this script to modify our database basically at will. For example consider the following query string:

```php
page.php?user=0;%20TRUNCATE%20TABLE%20users;

```

This makes our example query look like this

```php
SELECT name, email, user_level FROM users WHERE userID = 0; TRUNCATE TABLE users;

```

While this is an extreme example (most SQL injection attacks do not aim to delete data, nor do most PHP query execution functions support multi-query), this is an example of how a SQL injection attack can be made possible by the careless assembly of the query. Unfortunately, attacks like this are very common, and are highly effective due to coders who fail to take proper precautions to protect their data.

To prevent SQL injection from occurring, **prepared statements** are the recommended solution. Instead of concatenating user data directly to the query, a **placeholder** is used instead. The data is then sent separately, which means there is no chance of the SQL engine confusing user data for a set of instructions.

> 
While the topic here is PDO, please note that the PHP MySQLi extension also [supports prepared statements](http://stackoverflow.com/documentation/php/2784/php-mysqli/11958/prepared-statements-in-mysqli)


PDO supports two kinds of placeholders (placeholders cannot be used for column or table names, only values):

<li>
Named placeholders. A colon(`:`), followed by a distinct name (eg. `:user`)

```php
// using named placeholders
$sql = 'SELECT name, email, user_level FROM users WHERE userID = :user';
$prep = $conn->prepare($sql);
$prep->execute(['user' => $_GET['user']]); // associative array
$result = $prep->fetchAll();

```


</li>
<li>
Traditional SQL positional placeholders, represented as `?`:

```php
// using question-mark placeholders
$sql = 'SELECT name, user_level FROM users WHERE userID = ? AND user_level = ?';
$prep = $conn->prepare($sql);
$prep->execute([$_GET['user'], $_GET['user_level']]); // indexed array
$result = $prep->fetchAll();

```


</li>

If ever you need to dynamically change table or column names, know that this is at your own security risks and a bad practice. Though, it can be done by string concatenation. One way to improve security of such queries is to set a table of allowed values and compare the value you want to concatenate to this table.

Be aware that it is important to set connection charset through DSN only, otherwise your application could be prone to an [obscure vulnerability](https://stackoverflow.com/questions/134099/are-pdo-prepared-statements-sufficient-to-prevent-sql-injection/12202218#12202218) if some odd encoding is used. For PDO versions prior to 5.3.6 setting charset through DSN is not available and thus the only option is to set `PDO::ATTR_EMULATE_PREPARES` attribute  to `false` on the connection right after it’s created.

```php
$conn->setAttribute(PDO::ATTR_EMULATE_PREPARES, false);

```

This causes PDO to use the underlying DBMS’s native prepared statements instead of just emulating it.

However, be aware that PDO will [silently fallback](https://github.com/php/php-src/blob/master/ext/pdo_mysql/mysql_driver.c#L210) to emulating statements that MySQL cannot prepare natively: those that it can are [listed in the manual](http://dev.mysql.com/doc/en/sql-syntax-prepared-statements.html) ([source](https://stackoverflow.com/questions/134099/are-pdo-prepared-statements-sufficient-to-prevent-sql-injection/12202218#12202218)).



## Basic PDO Connection and Retrieval


Since PHP 5.0, [PDO](http://php.net/manual/en/intro.pdo.php) has been available as a database access layer. It is database agnostic, and so the following connection example code should work for any [of its supported databases](http://php.net/manual/en/pdo.drivers.php) simply by changing the DSN.

```php
// First, create the database handle

//Using MySQL (connection via local socket):
$dsn = "mysql:host=localhost;dbname=testdb;charset=utf8";

//Using MySQL (connection via network, optionally you can specify the port too):
//$dsn = "mysql:host=127.0.0.1;port=3306;dbname=testdb;charset=utf8";

//Or Postgres
//$dsn = "pgsql:host=localhost;port=5432;dbname=testdb;";

//Or even SQLite
//$dsn = "sqlite:/path/to/database"

$username = "user";
$password = "pass";
$db = new PDO($dsn, $username, $password);

// setup PDO to throw an exception if an invalid query is provided
$db->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);

// Next, let's prepare a statement for execution, with a single placeholder
$query = "SELECT * FROM users WHERE class = ?";
$statement = $db->prepare($query);

// Create some parameters to fill the placeholders, and execute the statement
$parameters = [ "221B" ];
$statement->execute($parameters);

// Now, loop through each record as an associative array
while ($row = $statement->fetch(PDO::FETCH_ASSOC)) {
    do_stuff($row);
}

```

The [`prepare`](http://php.net/manual/en/pdo.prepare.php) function creates a `PDOStatement` object from the query string. The execution of the query and retrieval of the results are performed on this returned object. In case of a failure, the function either returns `false` or throws an `exception` (depending upon how the PDO connection was configured).



## Database Transactions with PDO


Database transactions ensure that a set of data changes will only be made permanent if every statement is successful.  Any query or code failure during a transaction can be caught and you then have the option to roll back the attempted changes.

PDO provides simple methods for beginning, committing, and rollbacking back transactions.

```php
$pdo = new PDO(
    $dsn, 
    $username, 
    $password, 
    array(PDO::ATTR_ERRMODE => PDO::ERRMODE_EXCEPTION)
);

try {
    $statement = $pdo->prepare("UPDATE user SET name = :name");

    $pdo->beginTransaction();

    $statement->execute(["name"=>'Bob']);
    $statement->execute(["name"=>'Joe']);

    $pdo->commit();
} 
catch (\Exception $e) {
    if ($pdo->inTransaction()) {
        $pdo->rollback();
        // If we got here our two data updates are not in the database
    }
    throw $e;
}

```

During a transaction any data changes made are only visible to the active connection.  `SELECT` statements will return the altered changes even if they are not yet committed to the database.

**Note**: See database vendor documentation for details about transaction support.  Some systems do not support transactions at all.  Some support nested transactions while others do not.

**Practical Example Using Transactions with PDO**

In the following section is demonstrated a practical real world example where the use of transactions ensures the consistency of database.

Imagine the following scenario, let's say you are building a shopping cart for an e-commerce website and you decided to keep the orders in two database tables. One named `orders` with the fields `order_id`, `name`, `address`, `telephone` and `created_at`. And a second one named `orders_products` with the fields `order_id`, `product_id` and `quantity`. The first table contains the **metadata** of the order while the second one the actual **products** that have been ordered.

**Inserting a new order to the database**

To insert a new order into the database you need to do two things. First you need to `INSERT` a new record inside the `orders` table that will contain the **metadata** of the order (`name`, `address`, etc). And then you need to `INSERT` one record into the `orders_products` table, for each one of the products that are included in the order.

You could do this by doing something similar to the following:

```php
// Insert the metadata of the order into the database
$preparedStatement = $db->prepare(
    'INSERT INTO `orders` (`name`, `address`, `telephone`, `created_at`)
     VALUES (:name, :address, :telephone, :created_at)'
);

$preparedStatement->execute([
    'name' => $name,
    'address' => $address,
    'telephone' => $telephone,
    'created_at' => time(),
]);

// Get the generated `order_id`
$orderId = $db->lastInsertId();

// Construct the query for inserting the products of the order
$insertProductsQuery = 'INSERT INTO `orders_products` (`order_id`, `product_id`, `quantity`) VALUES';

$count = 0;
foreach ( $products as $productId => $quantity ) {
    $insertProductsQuery .= ' (:order_id' . $count . ', :product_id' . $count . ', :quantity' . $count . ')';
    
    $insertProductsParams['order_id' . $count] = $orderId;
    $insertProductsParams['product_id' . $count] = $productId;
    $insertProductsParams['quantity' . $count] = $quantity;
    
    ++$count;
}

// Insert the products included in the order into the database
$preparedStatement = $db->prepare($insertProductsQuery);
$preparedStatement->execute($insertProductsParams);

```

This will work great for inserting a new order into the database, until something unexpected happens and for some reason the second `INSERT` query fails. If that happens you will end up with a new order inside the `orders` table, which will have no products associated to it. Fortunately, the fix is very simple, all you have to do is to make the queries in the form of a single database transaction.

**Inserting a new order into the database with a transaction**

To start a transaction using `PDO` all you have to do is to call the `beginTransaction` method before you execute any queries to your database. Then you make any changes you want to your data by executing `INSERT` and / or `UPDATE` queries. And finally you call the `commit` method of the `PDO` object to make the changes permanent. Until you call the `commit` method every change you have done to your data up to this point is not yet permanent, and can be easily reverted by simply calling the `rollback` method of the `PDO` object.

On the following example is demonstrated the use of transactions for inserting a new order into the database, while ensuring the same time the consistency of the data. If one of the two queries fails all the changes will be reverted.

```php
// In this example we are using MySQL but this applies to any database that has support for transactions
$db = new PDO('mysql:host=' . $host . ';dbname=' . $dbname . ';charset=utf8', $username, $password);    

// Make sure that PDO will throw an exception in case of error to make error handling easier
$db->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);

try {
    // From this point and until the transaction is being committed every change to the database can be reverted
    $db->beginTransaction();    
    
    // Insert the metadata of the order into the database
    $preparedStatement = $db->prepare(
        'INSERT INTO `orders` (`order_id`, `name`, `address`, `created_at`)
         VALUES (:name, :address, :telephone, :created_at)'
    );
    
    $preparedStatement->execute([
        'name' => $name,
        'address' => $address,
        'telephone' => $telephone,
        'created_at' => time(),
    ]);
    
    // Get the generated `order_id`
    $orderId = $db->lastInsertId();

    // Construct the query for inserting the products of the order
    $insertProductsQuery = 'INSERT INTO `orders_products` (`order_id`, `product_id`, `quantity`) VALUES';
    
    $count = 0;
    foreach ( $products as $productId => $quantity ) {
        $insertProductsQuery .= ' (:order_id' . $count . ', :product_id' . $count . ', :quantity' . $count . ')';
        
        $insertProductsParams['order_id' . $count] = $orderId;
        $insertProductsParams['product_id' . $count] = $productId;
        $insertProductsParams['quantity' . $count] = $quantity;
        
        ++$count;
    }
    
    // Insert the products included in the order into the database
    $preparedStatement = $db->prepare($insertProductsQuery);
    $preparedStatement->execute($insertProductsParams);
    
    // Make the changes to the database permanent
    $db->commit();
}
catch ( PDOException $e ) { 
    // Failed to insert the order into the database so we rollback any changes
    $db->rollback();
    throw $e;
}

```



## PDO: connecting to MySQL/MariaDB server


There are two ways to connect to a MySQL/MariaDB server, depending on your infrastructure.

### Standard (TCP/IP) connection

```php
$dsn = 'mysql:dbname=demo;host=server;port=3306;charset=utf8';
$connection = new \PDO($dsn, $username, $password);

// throw exceptions, when SQL error is caused
$connection->setAttribute(\PDO::ATTR_ERRMODE, \PDO::ERRMODE_EXCEPTION);
// prevent emulation of prepared statements
$connection->setAttribute(\PDO::ATTR_EMULATE_PREPARES, false);

```

Since PDO was designed to be compatible with older MySQL server versions (which did not have support for prepared statements), you have to explicitly disable the emulation. Otherwise, you will lose the added **injection prevention** benefits, that are usually granted by using prepared statements.

Another design compromise, that you have to keep in mind, is the default error handling behavior. If not otherwise configured, PDO will not show any indications of SQL errors.

It is strongly recommended setting it to "exception mode", because that gains you additional functionality, when writing persistence abstractions (for example: having an exception, when violating `UNIQUE` constraint).

### Socket connection

```php
$dsn = 'mysql:unix_socket=/tmp/mysql.sock;dbname=demo;charset=utf8';
$connection = new \PDO($dsn, $username, $password);

// throw exceptions, when SQL error is caused
$connection->setAttribute(\PDO::ATTR_ERRMODE, \PDO::ERRMODE_EXCEPTION);
// prevent emulation of prepared statements
$connection->setAttribute(\PDO::ATTR_EMULATE_PREPARES, false);

```

On unix-like systems, if host name is `'localhost'`, then the connection to the server is made through a domain socket.



## PDO: Get number of affected rows by a query


We start off with `$db`, an instance of the PDO class. After executing a query we often want to determine the number of rows that have been affected by it. The `rowCount()` method of the `PDOStatement` will work nicely:

NOTE: This method should only be used to determine the number of rows affected by INSERT, DELETE, and UPDATE statements. Although this method may work for SELECT statements as well, it is not consistent across all databases.



## PDO::lastInsertId()


You may often find the need to get the auto incremented ID value for a row that you have just inserted into your database table.  You can achieve this with the lastInsertId() method.

```php
// 1. Basic connection opening (for MySQL)
$host = 'localhost';
$database = 'foo';
$user = 'root'
$password = '';
$dsn = "mysql:host=$host;dbname=$database;charset=utf8";
$pdo = new PDO($dsn, $user, $password);

// 2. Inserting an entry in the hypothetical table 'foo_user'
$query = "INSERT INTO foo_user(pseudo, email) VALUES ('anonymous', 'anonymous@example.com')";
$query_success = $pdo->query($query);

// 3. Retrieving the last inserted id
$id = $pdo->lastInsertId(); // return value is an integer

```

In postgresql and oracle, there is the RETURNING Keyword, which returns the specified columns of the currently inserted / modified rows.
Here example for inserting one entry:

```php
// 1. Basic connection opening (for PGSQL)
$host = 'localhost';
$database = 'foo';
$user = 'root'
$password = '';
$dsn = "pgsql:host=$host;dbname=$database;charset=utf8";
$pdo = new PDO($dsn, $user, $password);

// 2. Inserting an entry in the hypothetical table 'foo_user'
$query = "INSERT INTO foo_user(pseudo, email) VALUES ('anonymous', 'anonymous@example.com') RETURNING id";
$statement = $pdo->query($query);

// 3. Retrieving the last inserted id
$id = $statement->fetchColumn();  // return the value of the id column of the new row in foo_user

```



#### Syntax


- [`PDO::LastInsertId()`](http://php.net/manual/fr/pdo.lastinsertid.php)
- [`PDO::LastInsertId($columnName)`](http://php.net/manual/fr/pdo.lastinsertid.php) // some drivers need the column name



#### Remarks


**Warning**
Do not miss to check for exceptions while using [`lastInsertId()`](http://php.net/manual/fr/pdo.lastinsertid.php). It can throw the following error:

> 
SQLSTATE IM001 : Driver does not support this function


Here is how you should properly check for exceptions using this method :

```php
// Retrieving the last inserted id
$id = null;

try {
    $id = $pdo->lastInsertId(); // return value is an integer    
}
catch( PDOException $e ) {
    echo $e->getMessage();
}

```

