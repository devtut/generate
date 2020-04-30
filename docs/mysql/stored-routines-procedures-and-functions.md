---
metaTitle: "Stored routines (procedures and functions)"
description: "Stored procedure with IN, OUT, INOUT parameters, Create a Function, Create Procedure with a Constructed Prepare , Cursors, Multiple ResultSets, Create a function"
---

# Stored routines (procedures and functions)



## Stored procedure with IN, OUT, INOUT parameters


```sql
DELIMITER $$
 
DROP PROCEDURE IF EXISTS sp_nested_loop$$
CREATE PROCEDURE sp_nested_loop(IN i INT, IN j INT, OUT x INT, OUT y INT, INOUT z INT)
BEGIN
    DECLARE a INTEGER DEFAULT 0;
    DECLARE b INTEGER DEFAULT 0;
    DECLARE c INTEGER DEFAULT 0;
    WHILE a < i DO
        WHILE b < j DO
            SET c = c + 1;
            SET b = b + 1;
        END WHILE;    
        SET a = a + 1;
        SET b = 0;        
    END WHILE;
    SET x = a, y = c;
    SET z = x + y + z;
END $$
DELIMITER ;

```

Invokes ([CALL](http://dev.mysql.com/doc/refman/5.7/en/call.html)) the stored procedure:

```sql
SET @z = 30;
call sp_nested_loop(10, 20, @x, @y, @z);
SELECT @x, @y, @z;

```

Result:

```sql
+------+------+------+
|  @x  |  @y  |  @z  |
+------+------+------+
|  10  |  200 |  240 |
+------+------+------+

```

An `IN` parameter passes a value into a procedure. The procedure might modify the value, but the modification is not visible to the caller when the procedure returns.

An `OUT` parameter passes a value from the procedure back to the caller. Its initial value is NULL within the procedure, and its value is visible to the caller when the procedure returns.

An `INOUT` parameter is initialized by the caller, can be modified by the procedure, and any change made by the procedure is visible to the caller when the procedure returns.

Ref: [http://dev.mysql.com/doc/refman/5.7/en/create-procedure.html](http://dev.mysql.com/doc/refman/5.7/en/create-procedure.html)



## Create a Function


The following (trivial) example function simply returns the constant `INT` value `12`.

```sql
DELIMITER ||
CREATE FUNCTION functionname()
RETURNS INT
BEGIN
    RETURN 12;
END;
||
DELIMITER ;

```

The first line defines what the delimiter character(`DELIMITER ||`) is to be changed to, this is needed to be set before a function is created otherwise if left it at its default `;` then the first `;` that is found in the function body will be taken as the end of the `CREATE` statement, which is usually not what is desired.

After the `CREATE FUNCTION` has run you should set the delimiter back to its default of `;` as is seen after the function code in the above example (`DELIMITER ;`).

Execution this function is as follows:

```sql
SELECT functionname();
+----------------+
| functionname() |
+----------------+
|             12 |
+----------------+

```

A slightly more complex (but still trivial) example takes a parameter and adds a constant to it:

```sql
DELIMITER $$
CREATE FUNCTION add_2 ( my_arg INT )
  RETURNS INT
BEGIN
  RETURN (my_arg + 2);
END;
$$
DELIMITER ;

SELECT add_2(12);
+-----------+
| add_2(12) |
+-----------+
|        14 |
+-----------+

```

Note the use of a different argument to the `DELIMITER` directive. You can actually use any character sequence that does not appear in the `CREATE` statement body, but the usual practice is to use a doubled non-alphanumeric character such as `\\`, `||` or `$$`.

It is good practice to always change the parameter before and after a function, procedure or trigger creation or update as some GUI's don't require the delimiter to change whereas running queries via the command line always require the delimiter to be set.



## Create Procedure with a Constructed Prepare 


```sql
DROP PROCEDURE if exists displayNext100WithName;
DELIMITER $$
CREATE PROCEDURE displayNext100WithName
(    nStart int,
    tblName varchar(100)
)
BEGIN
    DECLARE thesql varchar(500); -- holds the constructed sql string to execute

    -- expands the sizing of the output buffer to accomodate the output (Max value is at least 4GB)
    SET session group_concat_max_len = 4096; -- prevents group_concat from barfing with error 1160 or whatever it is

    SET @thesql=CONCAT("select group_concat(qid order by qid SEPARATOR '%3B') as nums ","from (    select qid from ");
    SET @thesql=CONCAT(@thesql,tblName," where qid>? order by qid limit 100 )xDerived");
    PREPARE stmt1 FROM @thesql; -- create a statement object from the construct sql string to execute
    SET @p1 = nStart; -- transfers parameter passed into a User Variable compatible with the below EXECUTE
    EXECUTE stmt1 USING @p1;
     
    DEALLOCATE PREPARE stmt1; -- deallocate the statement object when finished
END$$
DELIMITER ;

```

Creation of the stored procedure shows wrapping with a DELIMITER necessary in many client tools.

Calling example:

```sql
call displayNext100WithName(1,"questions_mysql");

```

Sample output with `%3B` (semi-colon) separator:
[<img src="http://i.stack.imgur.com/0ZADP.jpg" alt="enter image description here" />](http://i.stack.imgur.com/0ZADP.jpg)



## Cursors


Cursors enable you to itterate results of query one by line. `DECLARE`
command is used to init cursor and associate it with a specific SQL query:

```sql
DECLARE student CURSOR FOR SELECT name FROM studend;

```

Let's say we sell products of some types. We want to count how many products of each type are exists.

Our data:

```sql
CREATE TABLE product
(
  id   INT(10) UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
  type VARCHAR(50)      NOT NULL,
  name VARCHAR(255)     NOT NULL

);
CREATE TABLE product_type
(
  name VARCHAR(50) NOT NULL PRIMARY KEY
);
CREATE TABLE product_type_count
(
  type  VARCHAR(50)      NOT NULL PRIMARY KEY,
  count INT(10) UNSIGNED NOT NULL DEFAULT 0
);

INSERT INTO product_type (name) VALUES
  ('dress'),
  ('food');

INSERT INTO product (type, name) VALUES
  ('dress', 'T-shirt'),
  ('dress', 'Trousers'),
  ('food', 'Apple'),
  ('food', 'Tomatoes'),
  ('food', 'Meat');

```

We may achieve the goal using stored procedure with using cursor:

```sql
DELIMITER //
DROP PROCEDURE IF EXISTS product_count;
CREATE PROCEDURE product_count()
  BEGIN
    DECLARE p_type VARCHAR(255);
    DECLARE p_count INT(10) UNSIGNED;
    DECLARE done INT DEFAULT 0;
    DECLARE product CURSOR FOR
      SELECT
        type,
        COUNT(*)
      FROM product
      GROUP BY type;
    DECLARE CONTINUE HANDLER FOR SQLSTATE '02000' SET done = 1;

    TRUNCATE product_type;

    OPEN product;
    
    REPEAT
      FETCH product
      INTO p_type, p_count;
      IF NOT done
      THEN
        INSERT INTO product_type_count
        SET
          type  = p_type,
          count = p_count;
      END IF;
    UNTIL done
    END REPEAT;
    
    CLOSE product;
  END //
DELIMITER ;

```

When you may call procedure with:

```sql
CALL product_count();

```

Result would be in `product_type_count` table:

```sql
type   | count
----------------
dress  |   2
food   |   3

```

While that is a good example of a `CURSOR`, notice how the entire body of the procedure can be replaced by just

```sql
INSERT INTO product_type_count
        (type, count)
    SELECT type, COUNT(*)
        FROM product
        GROUP BY type;

```

This will run a lot faster.



## Multiple ResultSets


Unlike a `SELECT` statement, a `Stored Procedure` returns multiple result sets.  The requires different code to be used for gathering the results of a `CALL` in Perl, PHP, etc.

(Need specific code here or elsewhere!)



## Create a function


```sql
DELIMITER $$
CREATE
    DEFINER=`db_username`@`hostname_or_IP`
    FUNCTION `function_name`(optional_param data_type(length_if_applicable))
    RETURNS data_type
BEGIN
    /*
    SQL Statements goes here
    */
END$$
DELIMITER ;

```

The RETURNS data_type is any MySQL datatype.



#### Parameters


|Parameter|Details
|------
|RETURNS|Specifies the data type that can be returned from a function.
|RETURN|Actual variable or value following the `RETURN` syntax is what is returned to where the function was called from.



#### Remarks


> 
A stored routine is either a procedure or a function.
<p>A procedure is invoked using a CALL statement and can only pass back
values using output variables.</p>
<p>A function can be called from inside a statement just like any other
function and can return a scalar value.</p>


