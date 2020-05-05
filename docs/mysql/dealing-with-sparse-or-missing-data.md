---
metaTitle: "MySQL - Dealing with sparse or missing data"
description: "Working with columns containg NULL values"
---

# Dealing with sparse or missing data



## Working with columns containg NULL values


In MySQL and other SQL dialects, `NULL` values have special properties.

Consider the following table containing job applicants, the companies they worked for, and the date they left the company. `NULL` indicates that an applicant still works at the company:

```sql
CREATE TABLE example
(`applicant_id` INT, `company_name` VARCHAR(255), `end_date` DATE);

+--------------+-----------------+------------+
| applicant_id | company_name    | end_date   |
+--------------+-----------------+------------+
|            1 | Google          | NULL       |
|            1 | Initech         | 2013-01-31 |
|            2 | Woodworking.com | 2016-08-25 |
|            2 | NY Times        | 2013-11-10 |
|            3 | NFL.com         | 2014-04-13 |
+--------------+-----------------+------------+

```

Your task is to compose a query that returns all rows after `2016-01-01`, including any employees that are still working at a company (those with `NULL` end dates). This select statement:

```sql
SELECT * FROM example WHERE end_date > '2016-01-01';

```

fails to include any rows with `NULL` values:

```sql
+--------------+-----------------+------------+
| applicant_id | company_name    | end_date   |
+--------------+-----------------+------------+
|            2 | Woodworking.com | 2016-08-25 |
+--------------+-----------------+------------+

```

Per the [MySQL documentation](http://dev.mysql.com/doc/refman/5.7/en/working-with-null.html), comparisons using the arithmetic operators <, >, =, and <> themselves return `NULL` instead of a boolean `TRUE` or `FALSE`. Thus a row with a `NULL` end_date is neither greater than 2016-01-01 nor less than 2016-01-01.

This can be solved by using the keywords IS NULL:

```sql
SELECT * FROM example WHERE end_date > '2016-01-01' OR end_date IS NULL;

+--------------+-----------------+------------+
| applicant_id | company_name    | end_date   |
+--------------+-----------------+------------+
|            1 | Google          | NULL       |
|            2 | Woodworking.com | 2016-08-25 |
+--------------+-----------------+------------+

```

Working with NULLs becomes more complex when the task involves aggregation functions like `MAX()` and a `GROUP BY` clause. If your task were to select the most recent employed date for each applicant_id, the following query would seem a logical first attempt:

```sql
SELECT applicant_id, MAX(end_date) FROM example GROUP BY applicant_id;

+--------------+---------------+
| applicant_id | MAX(end_date) |
+--------------+---------------+
|            1 | 2013-01-31    |
|            2 | 2016-08-25    |
|            3 | 2014-04-13    |
+--------------+---------------+

```

However, knowing that `NULL` indicates an applicant is still employed at a company, the first row of the result is inaccurate. Using `CASE WHEN` provides a workaround for the `NULL` issue:

```

SELECT
    applicant_id,
    CASE WHEN MAX(end_date is null) = 1 THEN 'present' ELSE MAX(end_date) END
    max_date
FROM example
GROUP BY applicant_id;

+--------------+------------+
| applicant_id | max_date   |
+--------------+------------+
|            1 | present    |
|            2 | 2016-08-25 |
|            3 | 2014-04-13 |
+--------------+------------+

```

This result can be joined back to the original `example` table to determine the company at which an applicant last worked:

```sql
SELECT 
  data.applicant_id,
  data.company_name,
  data.max_date
FROM (
  SELECT 
    *,
    CASE WHEN end_date is null THEN 'present' ELSE end_date END max_date
  FROM example
) data
INNER JOIN (
 SELECT
   applicant_id,
   CASE WHEN MAX(end_date is null) = 1 THEN 'present' ELSE MAX(end_date) END max_date
 FROM
   example
 GROUP BY applicant_id
) j
ON data.applicant_id = j.applicant_id AND data.max_date = j.max_date;

+--------------+-----------------+------------+
| applicant_id | company_name    | max_date   |
+--------------+-----------------+------------+
|            1 | Google          | present    |
|            2 | Woodworking.com | 2016-08-25 |
|            3 | NFL.com         | 2014-04-13 |
+--------------+-----------------+------------+

```

These are just a few examples of working with `NULL` values in MySQL.

