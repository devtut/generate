---
metaTitle: "Perl - Simple interaction with database via DBI module"
description: "DBI module"
---

# Simple interaction with database via DBI module



## DBI module


You should make sure that module DBI has been installed on your pc, then follow the bellow steps:

1. use DBI module in your perl script

`use DBI;`

1. Declare some primary parameters

`my $driver = "MyDriver";`

`my $database = "DB_name";`

`my $dsn = "DBI:$driver:dbname=$database";`

`my $userid = "your_user_ID";`

`my $password = "your_password";`

`my $tablename = "your_table";`

1. Connect to your database

`my $dbh = DBI->connect($dsn, $userid, $password);`

1. Prepare your query

`my $query = $dbh->prepare("Your DB query");`

Ex:

`$my_query = qq/SELECT * FROM table WHERE column1 = 2/;`

`my $query = $dbh->prepare($my_query);`

We can also use variable in the query, like below:

`my $table_name = "table";`

`my $filter_value = 2;`

`$my_query = qq/SELECT * FROM $table_name WHERE column1 = $filter_value/;`

1. Execute your query

`$query->execute();`

*Note: To avoid injection attack, you should use placeholders `?` instead of put your variable in the query.

Ex: you want to show the all data from 'table' where column1=$value1 and column2=$value2:

`my $query = $dbh->prepare("SELECT * FROM table WHERE column1 = ? AND column2 = ?;");`

`$query->execute($value1, $value2);`

1. Fletch your data

`my @row = $query->fetchrow_array();` store data as array

or

`my $ref = $sth->fetchrow_hashref();` store data as hash reference

1. Finish and disconnect DB

`$sth->finish;`

`$dbh->disconnect();`



#### Parameters


|Column|Column
|---|---|---|---|---|---|---|---|---|---
|`$driver`|Driver for DB, "Pg" for Postgresql and "mysql" for MySQL
|`$database`|your database name
|`$userid`|your database id
|`$password`|your database password
|`$query`|put your query here, ex: "select * from $your_table"

