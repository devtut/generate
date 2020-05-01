---
metaTitle: "Join"
description: "Inner Join, Outer Join, Using Join in an Update, Join on a Subquery, Cross Join, Self Join, Accidentally turning an outer join into an inner join, Delete using Join"
---

# Join


In Structured Query Language (SQL), a JOIN is a method of linking two data tables in a single query, allowing the database to return a set that contains data from both tables at once, or using data from one table to be used as a Filter on the second table. There are several types of JOINs defined within the ANSI SQL standard.



## Inner Join


`Inner join` returns only those records/rows that match/exists in both the tables based on one or more conditions (specified using `ON` keyword). It is the most common type of join. The general syntax for `inner join` is:

```sql
SELECT * 
FROM table_1
INNER JOIN table_2
  ON table_1.column_name = table_2.column_name

```

It can also be simplified as just `JOIN`:

```sql
SELECT * 
FROM table_1
JOIN table_2
  ON table_1.column_name = table_2.column_name

```

Example

```sql
/* Sample data. */
DECLARE @Animal table (
    AnimalId Int IDENTITY,
    Animal Varchar(20)
);

DECLARE @AnimalSound table (
    AnimalSoundId Int IDENTITY,
    AnimalId Int,
    Sound Varchar(20)
);

INSERT INTO @Animal (Animal) VALUES ('Dog');
INSERT INTO @Animal (Animal) VALUES ('Cat');
INSERT INTO @Animal (Animal) VALUES ('Elephant');

INSERT INTO @AnimalSound (AnimalId, Sound) VALUES (1, 'Barks');
INSERT INTO @AnimalSound (AnimalId, Sound) VALUES (2, 'Meows');
INSERT INTO @AnimalSound (AnimalId, Sound) VALUES (3, 'Trumpets');
/* Sample data prepared. */

SELECT 
    * 
FROM 
    @Animal
    JOIN @AnimalSound
        ON @Animal.AnimalId = @AnimalSound.AnimalId;

```

```sql
AnimalId    Animal               AnimalSoundId AnimalId    Sound
----------- -------------------- ------------- ----------- --------------------
1           Dog                  1             1           Barks
2           Cat                  2             2           Meows
3           Elephant             3             3           Trumpets

```

**Using inner join with left outer join (Substitute for Not exists)**

This query will return data from table 1 where fields matching with table2 with a key and data not in Table 1 when comparing with Table2 with a condition and key

```sql
select * 
  from Table1 t1
    inner join Table2 t2 on t1.ID_Column = t2.ID_Column 
    left  join Table3 t3 on t1.ID_Column = t3.ID_Column 
  where t2.column_name = column_value 
    and t3.ID_Column is null 
  order by t1.column_name;

```



## Outer Join


**Left Outer Join**

`LEFT JOIN` returns all rows from the left table, matched to rows from the right table where the `ON` clause conditions are met. Rows in which the `ON` clause is not met have `NULL` in all of the right table's columns. The syntax of a `LEFT JOIN` is:

```sql
SELECT * FROM table_1 AS t1
LEFT JOIN table_2 AS t2 ON t1.ID_Column = t2.ID_Column 

```

**Right Outer Join**

`RIGHT JOIN` returns all rows from the right table, matched to rows from the left table where the `ON` clause conditions are met. Rows in which the `ON` clause is not met have `NULL` in all of the left table's columns. The syntax of a `RIGHT JOIN` is:

```sql
SELECT * FROM table_1 AS t1
RIGHT JOIN table_2 AS t2 ON t1.ID_Column = t2.ID_Column 

```

**Full Outer Join**

`FULL JOIN` combines `LEFT JOIN` and `RIGHT JOIN`. All rows are returned from both tables, regardless of whether the conditions in the `ON` clause are met. Rows that do not satisfy the `ON` clause are returned with `NULL` in all of the opposite table's columns (that is, for a row in the left table, all columns in the right table will contain `NULL`, and vice versa). The syntax of a `FULL JOIN` is:

```sql
SELECT * FROM table_1 AS t1
FULL JOIN table_2 AS t2 ON t1.ID_Column = t2.ID_Column  

```

**Examples**

```sql
/* Sample test data. */
DECLARE @Animal table (
    AnimalId Int IDENTITY,
    Animal Varchar(20)
);

DECLARE @AnimalSound table (
    AnimalSoundId Int IDENTITY,
    AnimalId Int,
    Sound Varchar(20)
);

INSERT INTO @Animal (Animal) VALUES ('Dog');
INSERT INTO @Animal (Animal) VALUES ('Cat');
INSERT INTO @Animal (Animal) VALUES ('Elephant');
INSERT INTO @Animal (Animal) VALUES ('Frog');

INSERT INTO @AnimalSound (AnimalId, Sound) VALUES (1, 'Barks');
INSERT INTO @AnimalSound (AnimalId, Sound) VALUES (2, 'Meows');
INSERT INTO @AnimalSound (AnimalId, Sound) VALUES (3, 'Trumpet');
INSERT INTO @AnimalSound (AnimalId, Sound) VALUES (5, 'Roars');
/* Sample data prepared. */

```

**LEFT OUTER JOIN**

```sql
SELECT * 
FROM @Animal As t1 
LEFT JOIN @AnimalSound As t2 ON t1.AnimalId = t2.AnimalId;

```

Results for `LEFT JOIN`

```sql
AnimalId    Animal               AnimalSoundId AnimalId    Sound
----------- -------------------- ------------- ----------- --------------------
1           Dog                  1             1           Barks
2           Cat                  2             2           Meows
3           Elephant             3             3           Trumpet
4           Frog                 NULL          NULL        NULL

```

**RIGHT OUTER JOIN**

```sql
SELECT * 
FROM @Animal As t1 
RIGHT JOIN @AnimalSound As t2 ON t1.AnimalId = t2.AnimalId;

```

Results for `RIGHT JOIN`

```sql
AnimalId    Animal               AnimalSoundId AnimalId    Sound
----------- -------------------- ------------- ----------- --------------------
1           Dog                  1             1           Barks
2           Cat                  2             2           Meows
3           Elephant             3             3           Trumpet
NULL        NULL                 4             5           Roars

```

**FULL OUTER JOIN**

```sql
SELECT * 
FROM @Animal As t1 
FULL JOIN @AnimalSound As t2 ON t1.AnimalId = t2.AnimalId;

```

Results for `FULL JOIN`

```sql
AnimalId    Animal               AnimalSoundId AnimalId    Sound
----------- -------------------- ------------- ----------- --------------------
1           Dog                  1             1           Barks
2           Cat                  2             2           Meows
3           Elephant             3             3           Trumpet
4           Frog                 NULL          NULL        NULL
NULL        NULL                 4             5           Roars

```



## Using Join in an Update


Joins can also be used in an `UPDATE` statement:

```sql
CREATE TABLE Users (
    UserId int NOT NULL,
    AccountId int NOT NULL,
    RealName nvarchar(200) NOT NULL
)

CREATE TABLE Preferences (
    UserId int NOT NULL,
    SomeSetting bit NOT NULL
)

```

Update the `SomeSetting` column of the `Preferences` table filtering by a predicate on the `Users` table as follows:

```sql
UPDATE p
SET p.SomeSetting = 1
FROM Users u
JOIN Preferences p ON u.UserId = p.UserId
WHERE u.AccountId = 1234

```

`p` is an alias for `Preferences` defined in the `FROM` clause of the statement. Only rows with a matching `AccountId` from the `Users` table will be updated.

**Update with left outer join statements**

```sql
Update t 
SET  t.Column1=100
FROM Table1 t LEFT JOIN Table12 t2 
ON t2.ID=t.ID

```

**Update tables with inner join and aggregate function**

```sql
UPDATE t1
SET t1.field1 = t2.field2Sum
FROM table1 t1
INNER JOIN (select field3, sum(field2) as field2Sum
from table2
group by field3) as t2
on t2.field3 = t1.field3  

```



## Join on a Subquery


Joining on a subquery is often used when you want to get aggregate data (such as Count, Avg, Max, or Min) from a child/details table and display that along with records from the parent/header table. For example, you may want to retrieve the top/first child row based on Date or Id or maybe you want a Count of all Child Rows or an Average.

This example uses aliases which makes queries easier to read when you have multiple tables involved. In this case we are retrieving all rows from the parent table Purchase Orders and retrieving only the last (or most recent) child row from the child table PurchaseOrderLineItems. This example assumes the child table uses incremental numeric Id's.

```sql
SELECT po.Id, po.PODate, po.VendorName, po.Status, item.ItemNo, 
  item.Description, item.Cost, item.Price
FROM PurchaseOrders po
LEFT JOIN 
     (
       SELECT l.PurchaseOrderId, l.ItemNo, l.Description, l.Cost, l.Price, Max(l.id) as Id 
       FROM PurchaseOrderLineItems l
       GROUP BY l.PurchaseOrderId, l.ItemNo, l.Description, l.Cost, l.Price
     ) AS item ON item.PurchaseOrderId = po.Id

```



## Cross Join


`A cross join` is a Cartesian join, meaning a Cartesian product of both the tables. This join does not need any condition to join two tables. Each row in the left table will join to each row of the right table. Syntax for a cross join:

```sql
SELECT * FROM table_1
CROSS JOIN table_2 

```

Example:

```sql
/* Sample data. */
DECLARE @Animal table (
    AnimalId Int IDENTITY,
    Animal Varchar(20)
);

DECLARE @AnimalSound table (
    AnimalSoundId Int IDENTITY,
    AnimalId Int,
    Sound Varchar(20)
);

INSERT INTO @Animal (Animal) VALUES ('Dog');
INSERT INTO @Animal (Animal) VALUES ('Cat');
INSERT INTO @Animal (Animal) VALUES ('Elephant');

INSERT INTO @AnimalSound (AnimalId, Sound) VALUES (1, 'Barks');
INSERT INTO @AnimalSound (AnimalId, Sound) VALUES (2, 'Meows');
INSERT INTO @AnimalSound (AnimalId, Sound) VALUES (3, 'Trumpet');
/* Sample data prepared. */

SELECT 
    * 
FROM 
    @Animal 
    CROSS JOIN @AnimalSound;

```

Results:

```sql
AnimalId    Animal               AnimalSoundId AnimalId    Sound
----------- -------------------- ------------- ----------- --------------------
1           Dog                  1             1           Barks
2           Cat                  1             1           Barks
3           Elephant             1             1           Barks
1           Dog                  2             2           Meows
2           Cat                  2             2           Meows
3           Elephant             2             2           Meows
1           Dog                  3             3           Trumpet
2           Cat                  3             3           Trumpet
3           Elephant             3             3           Trumpet

```

Note that there are other ways that a CROSS JOIN can be applied. This is a an **"old style"** join (deprecated since ANSI SQL-92) with no condition, which results in a cross/Cartesian join:

```sql
SELECT * 
FROM @Animal, @AnimalSound;

```

This syntax also works due to an "always true" join condition, but is not recommended and should be avoided, in favor of explicit `CROSS JOIN` syntax, for the sake of readability.

```sql
SELECT * 
FROM 
    @Animal 
    JOIN @AnimalSound 
        ON 1=1

```



## Self Join


A table can be joined onto itself in what is known as a self join, combining records in the table with other records in the same table. Self joins are typically used in queries where a hierarchy in the table's columns is defined.

Consider the sample data in a table called `Employees`:

|ID|Name|Boss_ID
|---|---|---|---
|1|Bob|3
|2|Jim|1
|3|Sam|2

Each employee's `Boss_ID` maps to another employee's `ID`. To retrieve a list of employees with their respective boss' name, the table can be joined on itself using this mapping. Note that joining a table in this manner requires the use of an alias (`Bosses` in this case) on the second reference to the table to distinguish itself from the original table.

```sql
SELECT Employees.Name,
    Bosses.Name AS Boss
FROM Employees
INNER JOIN Employees AS Bosses 
    ON Employees.Boss_ID = Bosses.ID

```

Executing this query will output the following results:

|Name|Boss
|---|---|---|---
|Bob|Sam
|Jim|Bob
|Sam|Jim



## Accidentally turning an outer join into an inner join


Outer joins return all the rows from one or both tables, plus matching rows.

```sql
Table People
PersonID FirstName
       1 Alice
       2 Bob
       3 Eve

Table Scores
PersonID Subject Score
       1 Math    100
       2 Math     54
       2 Science  98

```

Left joining the tables:

```sql
Select * from People a
left join Scores b
on a.PersonID = b.PersonID

```

Returns:

```sql
PersonID FirstName PersonID Subject Score
       1 Alice            1 Math    100
       2 Bob              2 Math     54
       2 Bob              2 Science  98
       3 Eve           NULL NULL   NULL

```

If you wanted to return all the people, with any applicable math scores, a common mistake is to write:

```sql
Select * from People a
left join Scores b
on a.PersonID = b.PersonID
where Subject = 'Math'

```

This would remove Eve from your results, in addition to removing Bob's science score, as `Subject` is `NULL` for her.

The correct syntax to remove non-Math records while retaining all individuals in the `People` table would be:

```sql
Select * from People a
left join Scores b
on a.PersonID = b.PersonID
and b.Subject = 'Math'

```



## Delete using Join


Joins can also be used in a `DELETE` statement. Given a schema as follows:

```sql
CREATE TABLE Users (
    UserId int NOT NULL,
    AccountId int NOT NULL,
    RealName nvarchar(200) NOT NULL
)

CREATE TABLE Preferences (
    UserId int NOT NULL,
    SomeSetting bit NOT NULL
)

```

We can delete rows from the `Preferences` table, filtering by a predicate on the `Users` table as follows:

```sql
DELETE p
FROM Users u
INNER JOIN Preferences p ON u.UserId = p.UserId
WHERE u.AccountId = 1234

```

Here `p` is an alias for `Preferences` defined in the `FROM` clause of the statement and we only delete rows that have a matching `AccountId` from the `Users` table.

