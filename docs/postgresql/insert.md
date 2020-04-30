---
metaTitle: "INSERT"
description: "Insert data using COPY, Inserting multiple rows, INSERT data and RETURING values, Basic INSERT, Insert from select, SELECT data into file., UPSERT - INSERT ... ON CONFLICT DO UPDATE..."
---

# INSERT



## Insert data using COPY


COPY is PostgreSQL's bulk-insert mechanism. It's a convenient way to transfer data between files and tables, but it's also far faster than `INSERT` when adding more than a few thousand rows at a time.

Let's begin by creating sample data file.

```sql
cat > samplet_data.csv

1,Yogesh
2,Raunak
3,Varun
4,Kamal
5,Hari
6,Amit

```

And we need a two column table into which this data can be imported into.

```sql
CREATE TABLE copy_test(id int, name varchar(8));

```

Now the actual copy operation, this will create six records in the table.

```sql
COPY copy_test FROM '/path/to/file/sample_data.csv' DELIMITER ',';

```

Instead of using a file on disk, can insert data from `stdin`

```sql
COPY copy_test FROM stdin DELIMITER ',';
Enter data to be copied followed by a newline.
End with a backslash and a period on a line by itself.
>> 7,Amol
>> 8,Amar
>> \.
Time: 85254.306 ms

SELECT * FROM copy_test ;
 id |  name
----+--------
  1 | Yogesh
  3 | Varun
  5 | Hari
  7 | Amol
  2 | Raunak
  4 | Kamal
  6 | Amit
  8 | Amar

```

Also you can copy data from a table to file as below:

```sql
COPY copy_test TO 'path/to/file/sample_data.csv'  DELIMITER ',';

```

For more details on COPY you can check [here](https://www.postgresql.org/docs/9.2/static/sql-copy.html)



## Inserting multiple rows


You can insert multiple rows in the database at the same time:

```sql
INSERT INTO person (name, age) VALUES 
  ('john doe', 25),
  ('jane doe', 20);

```



## INSERT data and RETURING values


If you are inserting data into a table with an auto increment column and if you want to get the value of the auto increment column.

Say you have a table called `my_table`:

```sql
CREATE TABLE my_table
(
id serial NOT NULL, -- serial data type is auto incrementing four-byte integer
name character varying,
contact_number integer,
CONSTRAINT my_table_pkey PRIMARY KEY (id)
);

```

If you want to insert data into `my_table` and get the id of that row:

```sql
INSERT INTO my_table(name, contact_number) VALUES ( 'USER', 8542621) RETURNING id;

```

Above query will return the id of the row where the new record was inserted.



## Basic INSERT


Let's say we have a simple table called person:

```sql
CREATE TABLE person (
    person_id BIGINT,
    name VARCHAR(255).
    age INT,
    city VARCHAR(255)
);

```

The most basic insert involves inserting all values in the table:

```sql
INSERT INTO person VALUES (1, 'john doe', 25, 'new york');

```

If you want to insert only specific columns, you need to explicitly indicate which columns:

```sql
INSERT INTO person (name, age) VALUES ('john doe', 25);

```

Note that if any constraints exist on the table , such as NOT NULL, you will be required to include those columns in either case.



## Insert from select


You can insert data in a table as the result of a select statement:

```sql
INSERT INTO person SELECT * FROM tmp_person WHERE age < 30;

```

Note that the projection of the select must match the columns required for the insert. In this case, the `tmp_person` table has the same columns as `person`.



## SELECT data into file.


You can COPY table and paste it into a file.

```sql
postgres=# select * from my_table;
 c1 | c2 | c3 
----+----+----
  1 |  1 |  1
  2 |  2 |  2
  3 |  3 |  3
  4 |  4 |  4
  5 |  5 |   
(5 rows)

postgres=# copy my_table to '/home/postgres/my_table.txt' using delimiters '|' with null as 'null_string' csv header;
COPY 5
postgres=# \! cat my_table.txt
c1|c2|c3
1|1|1
2|2|2
3|3|3
4|4|4
5|5|null_string

```



## UPSERT - INSERT ... ON CONFLICT DO UPDATE...


since [version 9.5](https://www.postgresql.org/docs/9.5/static/sql-insert.html) postgres offers `UPSERT` functionality with `INSERT` statement.

Say you have a table called my_table, created in several previous examples. We insert a row, returning PK value of inserted row:

```sql
b=# INSERT INTO my_table (name,contact_number) values ('one',333) RETURNING id;
 id
----
  2
(1 row)

INSERT 0 1

```

Now if we try to insert row with existing unique key it will raise an exception:

```sql
b=# INSERT INTO my_table values (2,'one',333);
ERROR:  duplicate key value violates unique constraint "my_table_pkey"
DETAIL:  Key (id)=(2) already exists.

```

Upsert functionality offers ability to insert it anyway, solving the conflict:

```sql
b=# INSERT INTO my_table values (2,'one',333) ON CONFLICT (id) DO UPDATE SET name = my_table.name||' changed to: "two" at '||now() returning *;
 id |                       name                             | contact_number
----+-----------------------------------------------------------------------------------------------------------+----------------
  2 | one changed to: "two" at 2016-11-23 08:32:17.105179+00 |            333
(1 row)

INSERT 0 1

```

