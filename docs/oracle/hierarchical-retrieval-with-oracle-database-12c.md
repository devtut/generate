---
metaTitle: "Oracle Database - Hierarchical Retrieval With Oracle Database 12C"
description: "Using the CONNECT BY Caluse, Specifying the Direction of the Query From the Top Down"
---

# Hierarchical Retrieval With Oracle Database 12C


You can use hierarchical queries to retrieve data based on a natural hierarchical relationship between rows in a table



## Using the CONNECT BY Caluse


```sql
SELECT E.EMPLOYEE_ID,E.LAST_NAME,E.MANAGER_ID FROM HR.EMPLOYEES E
CONNECT BY PRIOR E.EMPLOYEE_ID = E.MANAGER_ID;

```

The `CONNECT BY` clause to define the relationship between employees and managers.



## Specifying the Direction of the Query From the Top Down


```sql
SELECT E.LAST_NAME|| ' reports to ' ||
PRIOR E.LAST_NAME "Walk Top Down"  
FROM HR.EMPLOYEES E
START WITH E.MANAGER_ID IS NULL
CONNECT BY PRIOR E.EMPLOYEE_ID = E.MANAGER_ID;

```

