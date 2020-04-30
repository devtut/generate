---
metaTitle: "Execution blocks"
description: "Using BEGIN ... END"
---

# Execution blocks



## Using BEGIN ... END


```sql
BEGIN
  UPDATE Employees SET PhoneNumber = '5551234567' WHERE Id = 1;
  UPDATE Employees SET Salary = 650 WHERE Id = 3;
END

```

