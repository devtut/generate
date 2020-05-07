---
metaTitle: "Oracle Database - Working with Dates"
description: "Date Arithmetic, Add_months function"
---

# Working with Dates



## Date Arithmetic


Oracle supports `DATE` (includes time to the nearest second) and `TIMESTAMP` (includes time to fractions of a second) datatypes, which allow arithmetic (addition and subtraction) natively. For example:

To get the next day:

```sql
select to_char(sysdate + 1, 'YYYY-MM-DD') as tomorrow from dual;

```

To get the previous day:

```sql
select to_char(sysdate - 1, 'YYYY-MM-DD') as yesterday from dual;

```

To add 5 days to the current date:

```sql
select to_char(sysdate + 5, 'YYYY-MM-DD') as five_days_from_now from dual;

```

To add 5 hours to the current date:

```sql
select to_char(sysdate + (5/24), 'YYYY-MM-DD HH24:MI:SS') as five_hours_from_now from dual;

```

To add 10 minutes to the current date:

```sql
select to_char(sysdate + (10/1440), 'YYYY-MM-DD HH24:MI:SS') as ten_mintues_from_now from dual;

```

To add 7 seconds to the current date:

```sql
select to_char(sysdate + (7/86400), 'YYYY-MM-DD HH24:MI:SS') as seven_seconds_from_now from dual;

```

To select rows where `hire_date` is 30 days ago or more:

```sql
select * from emp where hire_date < sysdate - 30;

```

To select rows where `last_updated` column is in the last hour:

```sql
select * from logfile where last_updated >= sysdate - (1/24);

```

Oracle also provides the built-in datatype `INTERVAL` which represents a duration of time (e.g. 1.5 days, 36 hours, 2 months, etc.). These can also be used with arithmetic with `DATE` and `TIMESTAMP` expressions. For example:

```sql
select * from logfile where last_updated >= sysdate - interval '1' hour;

```



## Add_months function


Syntax: `add_months(p_date, integer) return date;`

Add_months function adds amt months to p_date date.

```sql
SELECT add_months(date'2015-01-12', 2) m FROM dual;

```

|M
|---|---|---|---|---|---|---|---|---|---
|2015-03-12

You can also substract months using a negative `amt`

```sql
SELECT add_months(date'2015-01-12', -2) m FROM dual;

```

|M
|---|---|---|---|---|---|---|---|---|---
|2014-11-12

When the calculated month has fewer days as the given date, the last day of the calculated month will be returned.

```sql
SELECT to_char( add_months(date'2015-01-31', 1),'YYYY-MM-DD') m FROM dual;

```

|M
|---|---|---|---|---|---|---|---|---|---
|2015-02-28

