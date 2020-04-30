---
metaTitle: "Querying results by page"
description: "Row_Number()"
---

# Querying results by page



## Row_Number()


```sql
SELECT Row_Number() OVER(ORDER BY UserName) As RowID, UserFirstName, UserLastName
FROM Users

```

From which it will yield a result set with a RowID field which you can use to page between.

```sql
SELECT * 
FROM 
    ( SELECT Row_Number() OVER(ORDER BY UserName) As RowID, UserFirstName, UserLastName
      FROM Users 
    ) As RowResults
WHERE RowID Between 5 AND 10

```

