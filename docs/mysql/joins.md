---
metaTitle: "MySQL - Joins"
description: "Joins visualized, JOIN with subquery (Derived table), Full Outer Join, Joining Examples, Retrieve customers with orders -- variations on a theme, Inner-join for 3 tables"
---

# Joins



## Joins visualized


If you are a visually oriented person, this Venn diagram may help you understand the different types of `JOIN`s that exist within MySQL.

[<img src="http://i.stack.imgur.com/Hy7hh.jpg" alt="Join Visualization Venn Diagram" />](http://i.stack.imgur.com/Hy7hh.jpg)



## JOIN with subquery ("Derived" table)


```sql
SELECT x, ...
    FROM ( SELECT y, ... FROM ... ) AS a
    JOIN tbl  ON tbl.x = a.y
    WHERE ...

```

This will evaluate the subquery into a temp table, then `JOIN` that to `tbl`.

Prior to 5.6, there could not be an index on the temp table.  So, this was potentially very inefficient:

```sql
SELECT ...
    FROM ( SELECT y, ... FROM ... ) AS a
    JOIN ( SELECT x, ... FROM ... ) AS b  ON b.x = a.y
    WHERE ...

```

With 5.6, the optimizer figures out the best index and creates it on the fly.  (This has some overhead, so it is still not 'perfect'.)

Another common paradigm is to have a subquery to initialize something:

```sql
SELECT 
        @n := @n + 1,
        ...
    FROM ( SELECT @n := 0 ) AS initialize
    JOIN the_real_table
    ORDER BY ...

```

(Note: this is technically a `CROSS JOIN` (Cartesian product), as indicated by the lack of `ON`.  However it is efficient because the subquery returns only one row that has to be matched to the n rows in `the_real_table`.)



## Full Outer Join


MySQL does not support the `FULL OUTER JOIN`, but there are ways to emulate one.

**Setting up the data**

```sql
-- ----------------------------
-- Table structure for `owners`
-- ----------------------------
DROP TABLE IF EXISTS `owners`;
CREATE TABLE `owners` (
`owner_id` int(11) NOT NULL AUTO_INCREMENT,
`owner` varchar(30) DEFAULT NULL,
PRIMARY KEY (`owner_id`)
) ENGINE=InnoDB AUTO_INCREMENT=10 DEFAULT CHARSET=latin1;

-- ----------------------------
-- Records of owners
-- ----------------------------
INSERT INTO `owners` VALUES ('1', 'Ben');
INSERT INTO `owners` VALUES ('2', 'Jim');
INSERT INTO `owners` VALUES ('3', 'Harry');
INSERT INTO `owners` VALUES ('6', 'John');
INSERT INTO `owners` VALUES ('9', 'Ellie');
-- ----------------------------
-- Table structure for `tools`
-- ----------------------------
DROP TABLE IF EXISTS `tools`;
CREATE TABLE `tools` (
`tool_id` int(11) NOT NULL AUTO_INCREMENT,
`tool` varchar(30) DEFAULT NULL,
`owner_id` int(11) DEFAULT NULL,
PRIMARY KEY (`tool_id`)
) ENGINE=InnoDB AUTO_INCREMENT=11 DEFAULT CHARSET=latin1;
-- ----------------------------
-- Records of tools
-- ----------------------------
INSERT INTO `tools` VALUES ('1', 'Hammer', '9');
INSERT INTO `tools` VALUES ('2', 'Pliers', '1');
INSERT INTO `tools` VALUES ('3', 'Knife', '1');
INSERT INTO `tools` VALUES ('4', 'Chisel', '2');
INSERT INTO `tools` VALUES ('5', 'Hacksaw', '1');
INSERT INTO `tools` VALUES ('6', 'Level', null);
INSERT INTO `tools` VALUES ('7', 'Wrench', null);
INSERT INTO `tools` VALUES ('8', 'Tape Measure', '9');
INSERT INTO `tools` VALUES ('9', 'Screwdriver', null);
INSERT INTO `tools` VALUES ('10', 'Clamp', null);

```

**What do we want to see?**

We want to get a list, in which we see who owns which tools, and which tools might not have an owner.

**The queries**

To accomplish this, we can combine two queries by using `UNION`.
In this first query we are joining the tools on the owners by using a `LEFT JOIN`. This will add all of our owners to our resultset, doesn't matter if they actually own tools.

In the second query we are using a `RIGHT JOIN` to join the tools onto the owners. This way we manage to get all the tools in our resultset, if they are owned by no one their owner column will simply contain `NULL`. By adding a `WHERE`-clause which is filtering by `owners.owner_id IS NULL` we are defining the result as those datasets, which have not already been returned by the first query, as we are only looking for the data in the right joined table.

Since we are using `UNION ALL` the resultset of the second query will be attached to the first queries resultset.

```sql
SELECT `owners`.`owner`, tools.tool
FROM `owners`
LEFT JOIN `tools` ON `owners`.`owner_id` = `tools`.`owner_id`
UNION ALL
SELECT `owners`.`owner`, tools.tool
FROM `owners`
RIGHT JOIN `tools` ON `owners`.`owner_id` = `tools`.`owner_id`
WHERE `owners`.`owner_id` IS NULL;

+-------+--------------+
| owner | tool         |
+-------+--------------+
| Ben   | Pliers       |
| Ben   | Knife        |
| Ben   | Hacksaw      |
| Jim   | Chisel       |
| Harry | NULL         |
| John  | NULL         |
| Ellie | Hammer       |
| Ellie | Tape Measure |
| NULL  | Level        |
| NULL  | Wrench       |
| NULL  | Screwdriver  |
| NULL  | Clamp        |
+-------+--------------+
12 rows in set (0.00 sec)

```



## Joining Examples


Query to create table on db

```sql
CREATE TABLE `user` (
`id` smallint(5) unsigned NOT NULL AUTO_INCREMENT,
`name` varchar(30) NOT NULL,
`course` smallint(5) unsigned DEFAULT NULL,
PRIMARY KEY (`id`)
) ENGINE=InnoDB;

CREATE TABLE `course` (
`id` smallint(5) unsigned NOT NULL AUTO_INCREMENT,
`name` varchar(50) NOT NULL,
PRIMARY KEY (`id`)
) ENGINE=InnoDB;

```

Since we’re using InnoDB tables and know that user.course and course.id are related, we can specify a foreign key relationship:

```sql
ALTER TABLE `user`
ADD CONSTRAINT `FK_course`
FOREIGN KEY (`course`) REFERENCES `course` (`id`)
ON UPDATE CASCADE;

```

Join Query (Inner Join)

```sql
SELECT user.name, course.name
FROM `user`
INNER JOIN `course` on user.course = course.id;

```



## Retrieve customers with orders -- variations on a theme


This will get all the orders for all customers:

```sql
SELECT c.CustomerName, o.OrderID
    FROM Customers AS c
    INNER JOIN Orders AS o
        ON c.CustomerID = o.CustomerID
    ORDER BY c.CustomerName, o.OrderID;

```

This will count the number of orders for each customer:

```sql
SELECT c.CustomerName, COUNT(*) AS 'Order Count'
    FROM Customers AS c
    INNER JOIN Orders AS o
        ON c.CustomerID = o.CustomerID
    GROUP BY c.CustomerID;
    ORDER BY c.CustomerName;

```

Also, counts, but probably faster:

```sql
SELECT  c.CustomerName,
        ( SELECT COUNT(*) FROM Orders WHERE CustomerID = c.CustomerID ) AS 'Order Count'
    FROM Customers AS c
    ORDER BY c.CustomerName;

```

List only the customer with orders.

```sql
SELECT  c.CustomerName,
    FROM Customers AS c
    WHERE EXISTS ( SELECT * FROM Orders WHERE CustomerID = c.CustomerID )
    ORDER BY c.CustomerName;

```



## Inner-join for 3 tables


let's assume we have three table which can be used for simple website with Tags.

- Fist table is for Posts.
- Second for Tags
- Third for Tags & Post relation

fist table "videogame"

|id|title|reg_date|Content
|---|---|---|---
|1|BioShock Infinite|2016-08-08|....

"tags" table

|id|name
|---|---|---|---
|1|yennefer
|2|elizabeth

"tags_meta" table

|post_id|tag_id
|---|---|---|---
|1|2

```sql
SELECT videogame.id,
    videogame.title,
    videogame.reg_date,
    tags.name,
    tags_meta.post_id
FROM tags_meta
INNER JOIN videogame ON videogame.id = tags_meta.post_id
INNER JOIN tags ON tags.id = tags_meta.tag_id
WHERE tags.name = "elizabeth"
ORDER BY videogame.reg_date

```

this code can return all posts which related to that tag "#elizabeth"



#### Syntax


<li>
`INNER` and `OUTER` are ignored.
</li>
<li>
`FULL` is not implemented in MySQL.
</li>
<li>
"commajoin" (`FROM a,b WHERE a.x=b.y`) is frowned on; use `FROM a JOIN b ON a.x=b.y` instead.
</li>
<li>
<p>FROM a
JOIN b ON a.x=b.y
includes rows that match in both tables.</p>
</li>
<li>
<p>FROM a
LEFT JOIN b ON a.x=b.y
includes all rows from `a`, plus matching data from `b`, or `NULLs` if there is no matching row.</p>
</li>

