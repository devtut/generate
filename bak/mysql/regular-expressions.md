---
metaTitle: "MySQL - Regular Expressions"
description: "REGEXP / RLIKE"
---

# Regular Expressions


A regular expression is a powerful way of specifying a pattern for a complex search.



## REGEXP / RLIKE


The `REGEXP` (or its synonym, `RLIKE`) operator allows pattern matching based on regular expressions.

Consider the following `employee` table:

```sql
+-------------+-------------+-------------+--------------+----------+
| EMPLOYEE_ID | FIRST_NAME  | LAST_NAME   | PHONE_NUMBER | SALARY   |
+-------------+-------------+-------------+--------------+----------+
|         100 | Steven      | King        | 515.123.4567 | 24000.00 |
|         101 | Neena       | Kochhar     | 515.123.4568 | 17000.00 |
|         102 | Lex         | De Haan     | 515.123.4569 | 17000.00 |
|         103 | Alexander   | Hunold      | 590.423.4567 |  9000.00 |
|         104 | Bruce       | Ernst       | 590.423.4568 |  6000.00 |
|         105 | David       | Austin      | 590.423.4569 |  4800.00 |
|         106 | Valli       | Pataballa   | 590.423.4560 |  4800.00 |
|         107 | Diana       | Lorentz     | 590.423.5567 |  4200.00 |
|         108 | Nancy       | Greenberg   | 515.124.4569 | 12000.00 |
|         109 | Daniel      | Faviet      | 515.124.4169 |  9000.00 |
|         110 | John        | Chen        | 515.124.4269 |  8200.00 |
+-------------+-------------+-------------+--------------+----------+

```

### Pattern ^

Select all employees whose `FIRST_NAME` starts with **N**.

**Query**

```sql
SELECT * FROM employees WHERE FIRST_NAME REGEXP '^N'
-- Pattern start with----------------------------^

```

### Pattern $**

Select all employees whose `PHONE_NUMBER` ends with **4569**.

**Query**

```sql
SELECT * FROM employees WHERE PHONE_NUMBER REGEXP '4569$'
-- Pattern end with----------------------------------^

```

### NOT REGEXP

Select all employees whose `FIRST_NAME` **does not** start with **N**.

**Query**

```sql
SELECT * FROM employees WHERE FIRST_NAME NOT REGEXP '^N'
-- Pattern does not start with---------------^

```

### Regex Contain

Select all employees whose `LAST_NAME` contains **in** and whose `FIRST_NAME` contains `a`.

**Query**

```sql
SELECT * FROM employees WHERE FIRST_NAME REGEXP 'a' AND LAST_NAME REGEXP 'in'
-- No ^ or $, pattern can be anywhere -------------------------------------^

```

### Any character between [ ]

Select all employees whose `FIRST_NAME` starts with **A** or **B** or **C**.

**Query**

```sql
SELECT * FROM employees WHERE FIRST_NAME REGEXP '^[ABC]'
-------------------------------------------------^^---^

```

### Pattern or |

Select all employees whose `FIRST_NAME` starts with **A** or **B** or **C** and ends with **r**, **e**, or **i**.

**Query**

```sql
SELECT * FROM employees WHERE FIRST_NAME REGEXP '^[ABC]|[rei]$'
-- ----------------------------------------------^^---^^^---^^

```

### Counting regular expression matches

Consider the following query:

```sql
SELECT FIRST_NAME, FIRST_NAME REGEXP '^N' as matching FROM employees

```

`FIRST_NAME REGEXP '^N'` is **1** or **0** depending on the fact that `FIRST_NAME` matches `^N`.

To visualize it better:

```sql
SELECT 
FIRST_NAME, 
IF(FIRST_NAME REGEXP '^N', 'matches ^N', 'does not match ^N') as matching 
FROM employees

```

Finally, count total number of matching and non-matching rows with:

```sql
SELECT
IF(FIRST_NAME REGEXP '^N', 'matches ^N', 'does not match ^N') as matching,
COUNT(*) 
FROM employees 
GROUP BY matching

```

