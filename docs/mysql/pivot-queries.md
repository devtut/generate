---
metaTitle: "Pivot queries"
description: "Creating a pivot query"
---

# Pivot queries



## Creating a pivot query


MySQL does not provide a built-in way to create pivot queries. However, these can be created using prepared statements.

Assume the table `tbl_values`:

|Id|Name|Group|Value
|------
|1|Pete|A|10
|2|Pete|B|20
|3|John|A|10

Request: Create a query that shows the sum of `Value` for each `Name`; the `Group` must be column header and `Name` must be the row header.

```sql
-- 1. Create an expression that builds the columns
set @sql = (
    select group_concat(distinct 
        concat(
            "sum(case when `Group`='", Group, "' then `Value` end) as `", `Group`, "`"
        )
    ) 
    from tbl_values
);

-- 2. Complete the SQL instruction
set @sql = concat("select Name, ", @sql, " from tbl_values group by `Name`");

-- 3. Create a prepared statement
prepare stmt from @sql;

-- 4. Execute the prepared statement
execute stmt;

```

Result:

|Name|A|B
|------
|John|10|NULL
|Pete|10|20

**Important:** Deallocate the prepared statement once it's no longer needed:

```sql
deallocate prepare stmt;

```

[Example on SQL Fiddle](http://sqlfiddle.com/#!9/4a3e88/4)



#### Remarks


Pivot query creation in MySQL relies upon the `GROUP_CONCAT()` function. If the result of the expression that creates the columns of the pivot query is expected to be large, the value of the `group_concat_max_len` variable must be increased:

```sql
set session group_concat_max_len = 1024 * 1024; -- This should be enough for most cases

```

