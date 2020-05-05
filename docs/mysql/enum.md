---
metaTitle: "MySQL - ENUM"
description: "Why ENUM?, VARCHAR as an alternative, Adding a new option, NULL vs NOT NULL, TINYINT as an alternative"
---

# ENUM



## Why ENUM?


ENUM provides a way to provide an attribute for a row.  Attributes with a small number of non-numeric options work best.  Examples:

```sql
reply ENUM('yes', 'no')
gender ENUM('male', 'female', 'other', 'decline-to-state')

```

The values are strings:

```sql
INSERT ... VALUES ('yes', 'female')
SELECT ... --> yes female

```



## VARCHAR as an alternative


Let's say we have

```sql
type ENUM('fish','mammal','bird')

```

An alternative is

```sql
type VARCHAR(20)  COMENT "fish, bird, etc"

```

This is quite open-ended in that new types are trivially added.

Comparison, and whether better or worse than ENUM:

- (same) INSERT: simply provide the string
- (worse?) On INSERT a typo will go unnoticed
- (same) SELECT: the actual string is returned
- (worse) A lot more space is consumed



## Adding a new option


```sql
ALTER TABLE tbl MODIFY COLUMN type ENUM('fish','mammal','bird','insect');

```

Notes

- As with all cases of MODIFY COLUMN, you must include `NOT NULL`, and any other qualifiers that originally existed, else they will be lost.
- **If** you add to the **end** of the list **and** the list is under 256 items, the `ALTER` is done by merely changing the schema.  That is there will not be a lengthy table copy.  (Old versions of MySQL did not have this optimization.)



## NULL vs NOT NULL


Examples of what happens when NULL and 'bad-value' are stored into nullable and not nullable columns.  Also shows usage of casting to numeric via `+0`.

```sql
CREATE TABLE enum (
    e     ENUM('yes', 'no')   NOT NULL,
    enull ENUM('x', 'y', 'z')     NULL
        );
INSERT INTO enum (e, enull)
    VALUES
        ('yes', 'x'),
        ('no',  'y'),
        (NULL,  NULL),
        ('bad-value', 'bad-value');
Query OK, 4 rows affected, 3 warnings (0.00 sec)
Records: 4  Duplicates: 0  Warnings: 3

mysql>SHOW WARNINGS;
+---------+------+--------------------------------------------+
| Level   | Code | Message                                    |
+---------+------+--------------------------------------------+
| Warning | 1048 | Column 'e' cannot be null                  | 
| Warning | 1265 | Data truncated for column 'e' at row 4     |
| Warning | 1265 | Data truncated for column 'enull' at row 4 |
+---------+------+--------------------------------------------+
3 rows in set (0.00 sec)

```

What is in the table after those inserts.  This uses "+0" to cast to numeric see what is stored.

```sql
mysql>SELECT e, e+0 FROM enum;
+-----+-----+
| e   | e+0 |
+-----+-----+
| yes |   1 |
| no  |   2 |
|     |   0 |  -- NULL
|     |   0 |  -- 'bad-value'
+-----+-----+
4 rows in set (0.00 sec)

mysql>SELECT enull, enull+0 FROM enum;
+-------+---------+
| enull | enull+0 |
+-------+---------+
| x     |       1 |
| y     |       2 |
| NULL  |    NULL |
|       |       0 |  -- 'bad-value'
+-------+---------+
4 rows in set (0.00 sec)

```



## TINYINT as an alternative


Let's say we have

```sql
type ENUM('fish','mammal','bird')

```

An alternative is

```sql
type TINYINT UNSIGNED

```

plus

```sql
CREATE TABLE AnimalTypes (
    type TINYINT UNSIGNED NOT NULL AUTO_INCREMENT,
    name VARCHAR(20) NOT NULL  COMMENT "('fish','mammal','bird')",
    PRIMARY KEY(type),
    INDEX(name)
) ENGINE=InnoDB

```

which is very much like a many-to-many table.

Comparison, and whether better or worse than ENUM:

- (worse) INSERT:  need to lookup the `type`
- (worse) SELECT:  need to JOIN to get the string (ENUM gives you the string with no effort)
- (better) Adding new types:  Simply insert into this table.  With ENUM, you need to do an ALTER TABLE.
- (same) Either technique (for up to 255 values) takes only 1 byte.
- (mixed) There's also an issue of data integrity: `TINYINT` will admit invalid values; whereas `ENUM` sets them to a special empty-string value (unless strict SQL mode is enabled, in which case they are rejected). Better data integrity can be achieved with `TINYINT` by making it a foreign key into a lookup table: which, with appropriate queries/joins, but there is still the small cost of reaching into the other table.  (`FOREIGN KEYs` are not free.)

