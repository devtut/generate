---
metaTitle: "Reserved Words"
description: "Errors due to reserved words"
---

# Reserved Words


MySQL has some special names called **reserved words**. A reserved word can be used as an identifier for a table, column, etc. only if it's wrapped in backticks (`), otherwise it will give rise to an error.

To avoid such errors, either don't use reserved words as identifiers or wrap the offending identifier in backticks.



## Errors due to reserved words


When trying to select from a table called `order` like this

```sql
select * from order

```

the error rises:

> 
Error Code: 1064. You have an error in your SQL syntax; check the manual that corresponds to your MySQL server version for the right syntax to use near 'order' at line 1


Reserved keywords in MySQL need to be escaped with backticks (```sql)

```sql
select * from `order`

```

to distinguish between a keyword and a table or column name.

See also: [Syntax error due to using a reserved word as a table or column name in MySQL](http://stackoverflow.com/questions/23446377/syntax-error-due-to-using-a-reserved-word-as-a-table-or-column-name-in-mysql).



#### Remarks


Listed below are all reserved words (from [the official documentation](http://dev.mysql.com/doc/refman/5.7/en/keywords.html)):

- ACCESSIBLE
- ADD
- ALL
- ALTER
- ANALYZE
- AND
- AS
- ASC
- ASENSITIVE
- BEFORE
- BETWEEN
- BIGINT
- BINARY
- BLOB
- BOTH
- BY
- CALL
- CASCADE
- CASE
- CHANGE
- CHAR
- CHARACTER
- CHECK
- COLLATE
- COLUMN
- CONDITION
- CONSTRAINT
- CONTINUE
- CONVERT
- CREATE
- CROSS
- CURRENT_DATE
- CURRENT_TIME
- CURRENT_TIMESTAMP
- CURRENT_USER
- CURSOR
- DATABASE
- DATABASES
- DAY_HOUR
- DAY_MICROSECOND
- DAY_MINUTE
- DAY_SECOND
- DEC
- DECIMAL
- DECLARE
- DEFAULT
- DELAYED
- DELETE
- DESC
- DESCRIBE
- DETERMINISTIC
- DISTINCT
- DISTINCTROW
- DIV
- DOUBLE
- DROP
- DUAL
- EACH
- ELSE
- ELSEIF
- ENCLOSED
- ESCAPED
- EXISTS
- EXIT
- EXPLAIN
- FALSE
- FETCH
- FLOAT
- FLOAT4
- FLOAT8
- FOR
- FORCE
- FOREIGN
- FROM
- FULLTEXT
- GENERATED
- GET
- GRANT
- GROUP
- HAVING
- HIGH_PRIORITY
- HOUR_MICROSECOND
- HOUR_MINUTE
- HOUR_SECOND
- IF
- IGNORE
- IN
- INDEX
- INFILE
- INNER
- INOUT
- INSENSITIVE
- INSERT
- INT
- INT1
- INT2
- INT3
- INT4
- INT8
- INTEGER
- INTERVAL
- INTO
- IO_AFTER_GTIDS
- IO_BEFORE_GTIDS
- IS
- ITERATE
- JOIN
- KEY
- KEYS
- KILL
- LEADING
- LEAVE
- LEFT
- LIKE
- LIMIT
- LINEAR
- LINES
- LOAD
- LOCALTIME
- LOCALTIMESTAMP
- LOCK
- LONG
- LONGBLOB
- LONGTEXT
- LOOP
- LOW_PRIORITY
- MASTER_BIND
- MASTER_SSL_VERIFY_SERVER_CERT
- MATCH
- MAXVALUE
- MEDIUMBLOB
- MEDIUMINT
- MEDIUMTEXT
- MIDDLEINT
- MINUTE_MICROSECOND
- MINUTE_SECOND
- MOD
- MODIFIES
- NATURAL
- NOT
- NO_WRITE_TO_BINLOG
- NULL
- NUMERIC
- ON
- OPTIMIZE
- OPTIMIZER_COSTS
- OPTION
- OPTIONALLY
- OR
- ORDER
- OUT
- OUTER
- OUTFILE
- PARTITION
- PRECISION
- PRIMARY
- PROCEDURE
- PURGE
- RANGE
- READ
- READS
- READ_WRITE
- REAL
- REFERENCES
- REGEXP
- RELEASE
- RENAME
- REPEAT
- REPLACE
- REQUIRE
- RESIGNAL
- RESTRICT
- RETURN
- REVOKE
- RIGHT
- RLIKE
- SCHEMA
- SCHEMAS
- SECOND_MICROSECOND
- SELECT
- SENSITIVE
- SEPARATOR
- SET
- SHOW
- SIGNAL
- SMALLINT
- SPATIAL
- SPECIFIC
- SQL
- SQLEXCEPTION
- SQLSTATE
- SQLWARNING
- SQL_BIG_RESULT
- SQL_CALC_FOUND_ROWS
- SQL_SMALL_RESULT
- SSL
- STARTING
- STORED
- STRAIGHT_JOIN
- TABLE
- TERMINATED
- THEN
- TINYBLOB
- TINYINT
- TINYTEXT
- TO
- TRAILING
- TRIGGER
- TRUE
- UNDO
- UNION
- UNIQUE
- UNLOCK
- UNSIGNED
- UPDATE
- USAGE
- USE
- USING
- UTC_DATE
- UTC_TIME
- UTC_TIMESTAMP
- VALUES
- VARBINARY
- VARCHAR
- VARCHARACTER
- VARYING
- VIRTUAL
- WHEN
- WHERE
- WHILE
- WITH
- WRITE
- XOR
- YEAR_MONTH
- ZEROFILL
- GENERATED
- OPTIMIZER_COSTS
- STORED
- VIRTUAL

