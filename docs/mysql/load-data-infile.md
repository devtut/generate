---
metaTitle: "MySQL - LOAD DATA INFILE"
description: "using LOAD DATA INFILE to load large amount of data to database, Load data with duplicates, Import a CSV file into a MySQL table, import / export"
---

# LOAD DATA INFILE



## using LOAD DATA INFILE to load large amount of data to database


Consider the following example assuming that you have a ';'-delimited CSV to load into your database.

```sql
1;max;male;manager;12-7-1985
2;jack;male;executive;21-8-1990
.
.
.
1000000;marta;female;accountant;15-6-1992

```

Create the table for insertion.

```sql
CREATE TABLE `employee` ( `id` INT NOT NULL ,
                          `name` VARCHAR NOT NULL, 
                          `sex` VARCHAR NOT NULL ,
                          `designation` VARCHAR NOT NULL ,
                          `dob` VARCHAR NOT NULL   );                              

```

Use the following query to insert the values in that table.

```sql
LOAD DATA INFILE 'path of the file/file_name.txt' 
INTO TABLE employee
FIELDS TERMINATED BY ';' //specify the delimiter separating the values
LINES TERMINATED BY '\r\n'
(id,name,sex,designation,dob)

```

Consider the case where the date format is non standard.

```sql
1;max;male;manager;17-Jan-1985
2;jack;male;executive;01-Feb-1992
.
.
.
1000000;marta;female;accountant;25-Apr-1993

```

In this case you can change the format of the `dob` column before inserting like this.

```sql
LOAD DATA INFILE 'path of the file/file_name.txt' 
INTO TABLE employee
FIELDS TERMINATED BY ';' //specify the delimiter separating the values
LINES TERMINATED BY '\r\n'
(id,name,sex,designation,@dob)
SET date = STR_TO_DATE(@date, '%d-%b-%Y');

```

This example of LOAD DATA INFILE does not specify all the available features.

You can see more references on LOAD DATA INFILE [here](http://dev.mysql.com/doc/refman/5.7/en/load-data.html).



## Load data with duplicates


If you use the `LOAD DATA INFILE` command to populate a table with existing data, you will often find that the import fails due to duplicates. There are several possible ways to overcome this problem.

### LOAD DATA LOCAL

If this option has been enabled in your server, it can be used to load a file that exists on the client computer rather than the server. A side effect is that duplicate rows for unique values are ignored.

```sql
LOAD DATA LOCAL INFILE 'path of the file/file_name.txt' 
INTO TABLE employee

```

### LOAD DATA INFILE 'fname' REPLACE

When the replace keyword is used duplicate unique or primary keys will result in the existing row being replaced with new ones

```sql
LOAD DATA INFILE 'path of the file/file_name.txt' 
REPLACE INTO TABLE employee

```

### LOAD DATA INFILE 'fname' IGNORE

The opposite of `REPLACE`, existing rows will be preserved and new ones ignored. This behavior is similar to `LOCAL` described above. However the file need not exist on the client computer.

```sql
LOAD DATA INFILE 'path of the file/file_name.txt' 
IGNORE INTO TABLE employee

```

### Load via intermediary table

Sometimes ignoring or replacing all duplicates may not be the ideal option. You may need to make decisions based on the contents of other columns. In that case the best option is to load into an intermediary table and transfer from there.

```sql
INSERT INTO employee SELECT * FROM intermediary WHERE ...

```



## Import a CSV file into a MySQL table


The following command imports CSV files into a MySQL table with the same columns while respecting CSV quoting and escaping rules.

```sql
load data infile '/tmp/file.csv'
into table my_table
fields terminated by ','
optionally enclosed by '"'
escaped by '"'
lines terminated by '\n'
ignore 1 lines; -- skip the header row

```



## import / export


import

```sql
SELECT a,b,c  INTO OUTFILE 'result.txt' FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"' LINES TERMINATED BY '\n' FROM table;

```

Export

```sql
LOAD DATA INFILE 'result.txt' INTO TABLE table;

```



#### Syntax


1. LOAD DATA [LOW_PRIORITY | CONCURRENT] [LOCAL] INFILE 'file_name'
1. INTO TABLE tbl_name
1. [CHARACTER SET charset]
<li>[{FIELDS | COLUMNS}
[TERMINATED BY 'string']
[[OPTIONALLY] ENCLOSED BY 'char']]</li>
<li>[LINES
[STARTING BY 'string']
[TERMINATED BY 'string']
]</li>
1. [IGNORE number {LINES | ROWS}]
1. [(col_name_or_user_var,...)]
1. [SET col_name = expr,...]

