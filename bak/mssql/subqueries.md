---
metaTitle: "Microsoft SQL Server - Subqueries"
description: "Subqueries"
---

# Subqueries



## Subqueries


A subquery is a query within another SQL query. A subquery is also called inner query or inner select and the statement containing a subquery is called an outer query or outer select.

**Note**

1. Subqueries must be enclosed within parenthesis,
1. An ORDER BY cannot be used in a subquery.
1. The image type such as BLOB, array, text datatypes are not allowed in subqueries.

Subqueries can be used with select, insert, update and delete statement within where, from, select clause along with IN, comparison operators, etc.

We have a table named ITCompanyInNepal on which we will perform queries to show subqueries examples:

[<img src="http://i.stack.imgur.com/KcmNV.png" alt="enter image description here" />](http://i.stack.imgur.com/KcmNV.png)

Examples: **SubQueries With Select Statement**

with **In** operator and **where** clause:

```sql
SELECT *
FROM ITCompanyInNepal
WHERE Headquarter IN (SELECT Headquarter 
                      FROM ITCompanyInNepal
                      WHERE Headquarter = 'USA');

```

with **comparison operator** and **where** clause

```sql
SELECT *
FROM ITCompanyInNepal
WHERE NumberOfEmployee < (SELECT AVG(NumberOfEmployee) 
                          FROM ITCompanyInNepal
                      )

```

with **select** clause

```sql
SELECT   CompanyName,
         CompanyAddress,
         Headquarter,
         (Select SUM(NumberOfEmployee)
         FROM ITCompanyInNepal
         Where Headquarter = 'USA') AS TotalEmployeeHiredByUSAInKathmandu
FROM     ITCompanyInNepal 
WHERE    CompanyAddress = 'Kathmandu' AND Headquarter = 'USA'

```

**Subqueries with insert statement**

We have to insert data from IndianCompany table to ITCompanyInNepal. The table for IndianCompany is shown below:

[<img src="http://i.stack.imgur.com/rpGi5.png" alt="enter image description here" />](http://i.stack.imgur.com/rpGi5.png)

```sql
INSERT INTO ITCompanyInNepal
SELECT * 
FROM IndianCompany

```

**Subqueries with update statement**

Suppose all the companies whose headquarter is USA decided to fire 50 employees from all US based companies of Nepal due to some change in policy of USA companies.

```sql
UPDATE ITCompanyInNepal
SET NumberOfEmployee = NumberOfEmployee - 50
WHERE Headquarter IN (SELECT Headquarter 
                      FROM ITCompanyInNepal 
                      WHERE Headquarter = 'USA')

```

**Subqueries with Delete Statement**

Suppose all the companies whose headquarter is Denmark decided to shutdown their companies from Nepal.

```sql
DELETE FROM ITCompanyInNepal
WHERE Headquarter IN (SELECT Headquarter 
                     FROM ITCompanyInNepal
                     WHERE Headquarter = 'Denmark')

```

