---
metaTitle: "Backticks"
description: "Backticks usage"
---

# Backticks



## Backticks usage


There are many examples where backticks are used inside a query but for many it's still unclear when or where to use backticks ```sql`.

Backticks are mainly used to prevent an error called "**MySQL reserved word**". When making a table in PHPmyAdmin you are sometimes faced with a warning or alert that you are using a "**MySQL reserved word**".

For example when you create a table with a column named "`group`" you get a warning. This is because you can make the following query:

```sql
SELECT student_name, AVG(test_score) FROM student GROUP BY group

```

To make sure you don't get an error in your query you have to use backticks so your query becomes:

```sql
SELECT student_name, AVG(test_score) FROM student GROUP BY `group`

```

**Table**

Not only column names can be surrounded by backticks, but also table names. For example when you need to `JOIN` multiple tables.

```sql
SELECT `users`.`username`, `groups`.`group` FROM `users`

```

**Easier to read**

As you can see using backticks around table and column names also make the query easier to read.

For example when you are used to write querys all in lower case:

```sql
select student_name, AVG(test_score) from student group by group
select `student_name`, AVG(`test_score`) from `student` group by `group`

```

Please see the MySQL Manual page entitled [Keywords and Reserved Words](https://dev.mysql.com/doc/refman/5.5/en/keywords.html). The ones with an (R) are Reserved Words. The others are merely Keywords. The Reserved require special caution.

