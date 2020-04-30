---
metaTitle: "Table Creation"
description: "Table creation with Primary Key, Basic table creation, Table creation with Foreign Key, Show Table Structure, Cloning an existing table, CREATE TABLE FROM SELECT, Table Create With TimeStamp Column To Show Last Update"
---

# Table Creation




## Table creation with Primary Key


```sql
CREATE TABLE Person (
    PersonID     INT UNSIGNED NOT NULL,
    LastName     VARCHAR(66) NOT NULL,
    FirstName    VARCHAR(66),
    Address      VARCHAR(255),
    City         VARCHAR(66),
    PRIMARY KEY (PersonID)
);

```

A **primary key** is a `NOT NULL` single or a multi-column identifier which uniquely identifies a row of a table. An [index](http://stackoverflow.com/documentation/mysql/1748/indexes-and-keys) is created, and if not explicitly declared as `NOT NULL`, MySQL will declare them so silently and implicitly.

A table can have only one `PRIMARY KEY`, and each table is recommended to have one. InnoDB will automatically create one in its absence, (as seen in [MySQL documentation](https://dev.mysql.com/doc/refman/5.7/en/create-table.html)) though this is less desirable.

Often, an `AUTO_INCREMENT` `INT` also known as "surrogate key", is used for thin index optimization and relations with other tables. This value will (normally) increase by 1 whenever a new record is added, starting from a default value of 1.

However, despite its name, it is not its purpose to guarantee that values are incremental, merely that they are sequential and unique.

An auto-increment `INT` value will not reset to its default start value if all rows in the table are deleted, unless the table is truncated using [`TRUNCATE TABLE`](http://stackoverflow.com/documentation/sql/1466/truncate) statement.

### Defining one column as Primary Key (inline definition)

If the primary key consists of a single column, the `PRIMARY KEY` clause can be placed inline with the column definition:

```sql
CREATE TABLE Person (
    PersonID     INT UNSIGNED NOT NULL PRIMARY KEY,
    LastName     VARCHAR(66) NOT NULL,
    FirstName    VARCHAR(66),
    Address      VARCHAR(255),
    City         VARCHAR(66)
);

```

This form of the command is shorter and easier to read.

### Defining a multiple-column Primary Key

It is also possible to define a primary key comprising more than one column. This might be done e.g. on the child table of a foreign-key relationship. A multi-column primary key is defined by listing the participating columns in a separate `PRIMARY KEY` clause. Inline syntax is not permitted here, as only one column may be declared `PRIMARY KEY` inline. For example:

```sql
CREATE TABLE invoice_line_items (
    LineNum      SMALLINT UNSIGNED NOT NULL,
    InvoiceNum   INT UNSIGNED NOT NULL,
    -- Other columns go here
    PRIMARY KEY (InvoiceNum, LineNum),
    FOREIGN KEY (InvoiceNum) REFERENCES -- references to an attribute of a table
);

```

Note that the columns of the primary key **should** be specified in logical sort order, which **may** be different from the order in which the columns were defined, as in the example above.

Larger indexes require more disk space, memory, and I/O. Therefore keys should be as small as possible (especially regarding composed keys).  In InnoDB, every 'secondary index' includes a copy of the columns of the `PRIMARY KEY`.



## Basic table creation


The `CREATE TABLE` statement is used to create a table in a MySQL database.

```sql
CREATE TABLE Person (
    `PersonID`      INTEGER NOT NULL PRIMARY KEY,
    `LastName`      VARCHAR(80),
    `FirstName`     VARCHAR(80),
    `Address`       TEXT,
    `City`          VARCHAR(100)
) Engine=InnoDB;

```

Every field definition must have:

1. Field name: A valid field Name. Make sure to encolse the names in `-chars. This ensures that you can use eg space-chars in the fieldname.
1. Data type [Length]: If the field is `CHAR` or `VARCHAR`, it is mandatory to specify a field length.
1. Attributes  `NULL` | `NOT NULL`: If `NOT NULL` is specified, then any attempt to store a `NULL` value in that field will fail.
1. See more on data types and their attributes [here](http://dev.mysql.com/doc/refman/5.7/en/data-types.html).

`Engine=...` is an optional parameter used to specify the table's storage engine.
If no storage engine is specified, the table will be created using the server's default table storage engine (usually InnoDB or MyISAM).

### Setting defaults

Additionally, where it makes sense you can set a default value for each field by using `DEFAULT`:

```sql
CREATE TABLE Address (
    `AddressID`   INTEGER NOT NULL PRIMARY KEY,
    `Street`      VARCHAR(80),
    `City`        VARCHAR(80),
    `Country`     VARCHAR(80) DEFAULT "United States",
    `Active`      BOOLEAN DEFAULT 1,
) Engine=InnoDB;

```

If during inserts no `Street` is specified, that field will be `NULL` when retrieved. When no `Country` is specified upon insert, it will default to "United States".

You can set default values for all column types, [except](http://dev.mysql.com/doc/refman/5.7/en/data-type-defaults.html) for `BLOB`, `TEXT`, `GEOMETRY`, and `JSON` fields.



## Table creation with Foreign Key


```sql
CREATE TABLE Account (
    AccountID     INT UNSIGNED NOT NULL,
    AccountNo     INT UNSIGNED NOT NULL,
    PersonID    INT UNSIGNED,
    PRIMARY KEY (AccountID),
    FOREIGN KEY (PersonID) REFERENCES Person (PersonID)
) ENGINE=InnoDB;

```

**Foreign key:** A Foreign Key (`FK`) is either a single column, or multi-column composite of columns, in a **referencing** table. This `FK` is confirmed to exist in the **referenced** table. It is highly recommended that the **referenced** table key confirming the `FK` be a Primary Key, but that is not enforced. It is used as a fast-lookup into the **referenced** where it does not need to be unique, and in fact can be a left-most index there.

Foreign key relationships involve a parent table that holds the central data values, and a child table with identical values pointing back to its parent. The FOREIGN KEY clause is specified in the child table. The parent and child tables must use the same storage engine. They must not be [TEMPORARY](https://dev.mysql.com/doc/refman/5.7/en/create-table.html) tables.

Corresponding columns in the foreign key and the referenced key must have similar data types. The size and sign of integer types must be the same. The length of string types need not be the same. For nonbinary (character) string columns, the character set and collation must be the same.

**Note:** foreign-key constraints are supported under the InnoDB storage engine (not MyISAM or MEMORY). DB set-ups using other engines will accept this `CREATE TABLE` statement but will not respect foreign-key constraints.  (Although newer MySQL versions default to `InnoDB`, but it is good practice to be explicit.)



## Show Table Structure


If you want to see the schema information of your table, you can use one of the following:

```sql
SHOW CREATE TABLE child; -- Option 1

CREATE TABLE `child` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `fullName` varchar(100) NOT NULL,
  `myParent` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `mommy_daddy` (`myParent`),
  CONSTRAINT `mommy_daddy` FOREIGN KEY (`myParent`) REFERENCES `parent` (`id`) 
      ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

```

If used from the mysql commandline tool, this is less verbose:

```sql
SHOW CREATE TABLE child \G

```

A less descriptive way of showing the table structure:

```sql
mysql> CREATE TABLE Tab1(id int, name varchar(30));
Query OK, 0 rows affected (0.03 sec)

mysql> DESCRIBE Tab1; -- Option 2 

+-------+-------------+------+-----+---------+-------+ 
| Field | Type        | Null | Key | Default | Extra |  
+-------+-------------+------+-----+---------+-------+ 
| id    | int(11)     | YES  |     | NULL    |       | 
| name  | varchar(30) | YES  |     | NULL    |       |  
+-------+-------------+------+-----+---------+-------+ 

```

> 
Both **DESCRIBE** and **DESC** gives the same result.


To see `DESCRIBE` performed on all tables in a database at once, see this [Example](http://stackoverflow.com/a/38679580).



## Cloning an existing table


A table can be replicated as follows:

```sql
CREATE TABLE ClonedPersons LIKE Persons;

```

The new table will have exactly the same structure as the original table, including indexes and column attributes.

As well as manually creating a table, it is also possible to create table by selecting data from another table:

```sql
CREATE TABLE ClonedPersons SELECT * FROM Persons;

```

You can use any of the normal features of a `SELECT` statement to modify the data as you go:

```sql
CREATE TABLE ModifiedPersons
SELECT PersonID, FirstName + LastName AS FullName FROM Persons
WHERE LastName IS NOT NULL;

```

Primary keys and indexes will not be preserved when creating tables from `SELECT`. You must redeclare them:

```sql
CREATE TABLE ModifiedPersons (PRIMARY KEY (PersonID))
SELECT PersonID, FirstName + LastName AS FullName FROM Persons
WHERE LastName IS NOT NULL;

```



## CREATE TABLE FROM SELECT


You can create one table from another by adding a `SELECT` statement at the end of the `CREATE TABLE` statement:

```sql
CREATE TABLE stack (
    id_user INT,
    username VARCHAR(30),
    password VARCHAR(30)
);

```

**Create a table in the same database:**

```sql
-- create a table from another table in the same database with all attributes
CREATE TABLE stack2 AS SELECT * FROM stack;

-- create a table from another table in the same database with some attributes
CREATE TABLE stack3 AS SELECT username, password FROM stack;

```

**Create tables from different databases:**

```sql
-- create a table from another table from another database with all attributes
CREATE TABLE stack2 AS SELECT * FROM second_db.stack;

-- create a table from another table from another database with some attributes
CREATE TABLE stack3 AS SELECT username, password FROM second_db.stack;

```

**N.B**

To create a table same of another table that exist in another database, you need to specifies the name of the database like this:

```sql
FROM NAME_DATABASE.name_table

```



## Table Create With TimeStamp Column To Show Last Update


The TIMESTAMP column will show when the row was last updated.

```sql
CREATE TABLE `TestLastUpdate` (
    `ID` INT NULL,
    `Name` VARCHAR(50) NULL,
    `Address` VARCHAR(50) NULL,
    `LastUpdate` TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
)
COMMENT='Last Update'
;

```



#### Syntax


<li>
<p>CREATE TABLE table_name
(
column_name1 data_type(size),
column_name2 data_type(size),
column_name3 data_type(size),
....
); // Basic table creation</p>
</li>
<li>
<p>CREATE TABLE table_name [IF NOT EXISTS]
(
column_name1 data_type(size),
column_name2 data_type(size),
column_name3 data_type(size),
....
); // Table creation checking existing</p>
</li>
<li>
<p>CREATE [TEMPORARY] TABLE table_name [IF NOT EXISTS]
(
column_name1 data_type(size),
column_name2 data_type(size),
column_name3 data_type(size),
....
); // Temporary table creation</p>
</li>
<li>
CREATE TABLE new_tbl [AS] SELECT * FROM orig_tbl; // Table creation from SELECT
</li>



#### Remarks


The `CREATE TABLE` statement should end with an `ENGINE` specification:

```sql
CREATE TABLE table_name ( column_definitions ) ENGINE=engine;

```

**Some options are:**

- `InnoDB`: (Default since version 5.5.5) It's a transation-safe (ACID compliant) engine. It has transaction commit and roll-back, and crash-recovery capabilities and row-level locking.
- `MyISAM`: (Default before version 5.5.5) It's a plain-fast engine. It doesn't support transactions, nor foreign keys, but it's useful for data-warehousing.
- `Memory`: Stores all data in RAM for extremely fast operations but table date will be lost on database restart.

More engine options [here](https://dev.mysql.com/doc/refman/5.5/en/storage-engines.html).

