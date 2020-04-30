---
metaTitle: "Error codes"
description: "Error code 1064: Syntax error, Error code 1175: Safe Update, Error code 1215: Cannot add foreign key constraint, 1067, 1292, 1366, 1411 - Bad Value for number, date, default, etc., 1045 Access denied, 1236 impossible position in Replication, 2002, 2003 Cannot connect, 126, 127, 134, 144, 145, 139, 1366, 126, 1054, 1146, 1062, 24"
---

# Error codes



## Error code 1064: Syntax error


```sql
select LastName, FirstName,
from Person

```

Returns message:

> 
Error Code: 1064. You have an error in your SQL syntax; check the manual that corresponds to your MySQL server version for the right syntax to use near 'from Person' at line 2.


Getting a "1064 error" message from MySQL means the query cannot be parsed without syntax errors. In other words it can't make sense of the query.

The quotation in the error message begins with the first character of the query that MySQL can't figure out how to parse. In this example MySQL can't make sense, in context, of  `from Person`. In this case, there's an extra comma immediately before `from Person`. The comma tells MySQL to expect another column description in the `SELECT` clause

A syntax error always says `... near '...'`. The thing at the beginning of the quotes is very near where the error is. To locate an error, look at the first token in the quotes and at the last token before the quotes.

Sometimes you will get `... near ''`; that is, nothing in the quotes. That means the first character MySQL can't figure out is right at the end or the beginning of the statement. This suggests the query contains unbalanced quotes (`'` or `"`) or unbalanced parentheses or that you did not terminate the statement before correctly.

In the case of a Stored Routine, you may have forgotten to properly use `DELIMITER`.

So, when you get Error 1064, look at the text of the query, and find the point mentioned in the error message. Visually inspect the text of the query right around that point.

If you ask somebody to help you troubleshoot Error 1064, it's best to provide both the text of the whole query and the text of the error message.



## Error code 1175: Safe Update


This error appears while trying to update or delete records without including the `WHERE` clause that uses the `KEY` column.

To execute the delete or update anyway - type:

```sql
SET SQL_SAFE_UPDATES = 0;

```

To enable the safe mode again - type:

```sql
SET SQL_SAFE_UPDATES = 1;

```



## Error code 1215: Cannot add foreign key constraint


This error occurs when tables are not adequately structured to handle the speedy lookup verification of Foreign Key (`FK`) requirements that the developer is mandating.

```sql
CREATE TABLE `gtType` (
  `type` char(2) NOT NULL,
  `description` varchar(1000) NOT NULL,
  PRIMARY KEY (`type`)
) ENGINE=InnoDB;

CREATE TABLE `getTogethers` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `type` char(2) NOT NULL,
  `eventDT` datetime NOT NULL,
  `location` varchar(1000) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `fk_gt2type` (`type`), -- see Note1 below 
  CONSTRAINT `gettogethers_ibfk_1` FOREIGN KEY (`type`) REFERENCES `gtType` (`type`)
) ENGINE=InnoDB;

```

Note1: a KEY like this will be created automatically if needed due to the FK definition in the line
that follows it. The developer can skip it, and the KEY (a.k.a. index) will be added if necessary.
An example of it being skipped by the developer is shown below in `someOther`.

So far so good, until the below call.

```sql
CREATE TABLE `someOther` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `someDT` datetime NOT NULL,
  PRIMARY KEY (`id`),
  CONSTRAINT `someOther_dt` FOREIGN KEY (`someDT`) REFERENCES `getTogethers` (`eventDT`)
) ENGINE=InnoDB;

```

> 
Error Code: 1215. Cannot add foreign key constraint


In this case it fails due to the lack of an index in the **referenced** table `getTogethers` to
handle the speedy lookup of an `eventDT`. To be solved in next statement.

```sql
CREATE INDEX `gt_eventdt` ON getTogethers (`eventDT`);

```

Table `getTogethers` has been modified, and now the creation of `someOther` will succeed.

From the MySQL Manual Page [Using FOREIGN KEY Constraints](https://dev.mysql.com/doc/refman/5.6/en/create-table-foreign-keys.html):

> 
<p>MySQL requires indexes on foreign keys and referenced keys so that
foreign key checks can be fast and not require a table scan. In the
referencing table, there must be an index where the foreign key
columns are listed as the first columns in the same order. Such an
index is created on the referencing table automatically if it does not
exist.</p>
<p>Corresponding columns in the foreign key and the referenced key must
have similar data types. The size and sign of integer types must be
the same. The length of string types need not be the same. For
nonbinary (character) string columns, the character set and collation
must be the same.</p>
<p>InnoDB permits a foreign key to reference any index column or group of
columns. However, in the referenced table, there must be an index
where the referenced columns are listed as the first columns in the
same order.</p>


Note that last point above about first (left-most) columns and the lack of a Primary Key requirement (though highly advised).

Upon successful creation of a **referencing** (child) table, any keys that were automatically created for you are visible with a command such as the following:

```sql
SHOW CREATE TABLE someOther;

```

Other common cases of experiencing this error include, as mentioned above from the docs, but should be highlighted:

<li>
<p>Seemingly trivial differences in `INT` which is signed, pointing
toward `INT UNSIGNED`.</p>
</li>
<li>
Developers having trouble understanding multi-column (composite) KEYS and first (left-most) ordering requirements.
</li>



## 1067, 1292, 1366, 1411 - Bad Value for number, date, default, etc.


**1067** This is probably related to `TIMESTAMP` defaults, which have changed over time.  See `TIMESTAMP defaults` in the Dates & Times page.  (which does not exist yet)

**1292/1366 DOUBLE/Integer** Check for letters or other syntax errors.  Check that the columns align; perhaps you think you are putting into a `VARCHAR` but it is aligned with a numeric column.

**1292 DATETIME** Check for too far in past or future.  Check for between 2am and 3am on a morning when Daylight savings changed.  Check for bad syntax, such as `+00` timezone stuff.

**1292 VARIABLE** Check the allowed values for the `VARIABLE` you are trying to `SET`.

**1292 LOAD DATA** Look at the line that is 'bad'.  Check the escape symbols, etc.  Look at the datatypes.

**1411 STR_TO_DATE** Incorrectly formatted date?



## 1045 Access denied


See discussions in "GRANT" and "Recovering root password".



## 1236 "impossible position" in Replication


**Usually** this means that the Master crashed and that `sync_binlog` was OFF. The solution is to `CHANGE MASTER to POS=0` of the next binlog file (see the Master) on the Slave.

The cause: The Master sends replication items to the Slave before flushing to its binlog (when `sync_binlog=OFF`). If the Master crashes before the flush, the Slave has already logically moved past the end of file on the binlog. When the Master starts up again, it starts a new binlog, so CHANGEing to the beginning of that binlog is the best available solution.

A longer term solution is `sync_binlog=ON`, if you can afford the extra I/O that it causes.

(If you are running with GTID, ...?)



## 2002, 2003 Cannot connect


Check for a Firewall issue blocking port 3306.

Some possible diagnostics and/or solutions

- Is the server actually running?
- "service firewalld stop" and "systemctl disable firewalld"
- telnet master 3306
- Check the `bind-address`
- check `skip-name-resolve`
- check the socket.



## 126, 127, 134, 144, 145


When you try access the records from MySQL database, you may get these error messages. These error messages occurred due to corruption in MySQL database. Following are the types

```sql
MySQL error code 126 = Index file is crashed
MySQL error code 127 = Record-file is crashed
MySQL error code 134 = Record was already deleted (or record file crashed)
MySQL error code 144 = Table is crashed and last repair failed
MySQL error code 145 = Table was marked as crashed and should be repaired

```

MySQL bug, virus attack, server crash, improper shutdown, damaged table are the reason behind this corruption. When it gets corrupted, it becomes inaccessible and you cannot access them anymore. In order to get accessibility, the best way to retrieve data from an updated backup. However, if you do not have updated or any valid backup then you can go for MySQL Repair.

If the table engine type is `MyISAM`, apply `CHECK TABLE`, then `REPAIR TABLE` to it.

Then think seriously about converting to InnoDB, so this error won't happen again.

****Syntax****

```sql
CHECK TABLE <table name> ////To check the extent of database corruption
REPAIR TABLE <table name> ////To repair table

```



## 139


Error 139 may mean that the number and size of the fields in the table definition exceeds some limit.  Workarounds:

- Re-think the schema
- Normalize some fields
- Vertically partition the table



## 1366


This usually means that the character set handling was not consistent between client and server.  See ... for further assistance.



## 126, 1054, 1146, 1062, 24


(taking a break)  With the inclusion of those 4 error numbers, I think this page will have covered about 50% of the typical errors users get.

(Yes, this 'Example' needs revision.)

**24 Can't open file (Too many open files)**

`open_files_limit` comes from an OS setting.  `table_open_cache` needs to be less than that.

These can cause that error:

<li>
Failure to `DEALLOCATE PREPARE` in a stored procedure.
</li>
<li>
PARTITIONed table(s) with a large number of partitions and innodb_file_per_table = ON.  Recommend not having more than 50 partitions in a given table (for various reasons).  (When "Native Partitions" become available, this advice may change.)
</li>

The obvious workaround is to set increase the OS limit:
To allow more files, change `ulimit` or `/etc/security/limits.conf` or in `sysctl.conf` (kern.maxfiles & kern.maxfilesperproc) or something else (OS dependent).
Then increase `open_files_limit` and `table_open_cache`.

As of 5.6.8, `open_files_limit` is auto-sized based on `max_connections`, but it is OK to change it from the default.

**1062 - Duplicate Entry**

This error occur mainly because of the following two reasons

<li>
**Duplicate Value**  -  `Error Code: 1062. Duplicate entry ‘12’ for key ‘PRIMARY’`
The primary key column is unique and it will not accept the duplicate entry. So when you are trying to insert a new row which is already present in you table will produce this error.
</li>

> 
<p>To solve this, Set the primary key column as `AUTO_INCREMENT`. And
when you are trying to insert a new row,  ignore the primary key
column or insert `NULL` value to primary key.</p>


```sql
CREATE TABLE userDetails(
  userId INT(10) NOT NULL AUTO_INCREMENT,
  firstName VARCHAR(50),
  lastName VARCHAR(50),
  isActive INT(1) DEFAULT 0,
  PRIMARY KEY (userId) );

--->and now while inserting 
INSERT INTO userDetails VALUES (NULL ,'John', 'Doe', 1);

```


<li>
**Unique data field** - `Error Code: 1062. Duplicate entry ‘A’ for key ‘code’`
You may assigned a column as unique and trying to insert a new row with already existing value for that column will produce this error.
</li>

> 
To overcome this error, use `INSERT IGNORE` instead of normal `INSERT`. If the new row which you are trying to insert doesn't duplicate an existing record, MySQL inserts it as usual. If the record is a duplicate, the `IGNORE` keyword discard it without generating any error.


```sql
INSERT IGNORE INTO userDetails VALUES (NULL ,'John', 'Doe', 1);

```

