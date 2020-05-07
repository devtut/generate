---
metaTitle: "Oracle Database - Window Functions"
description: "Ratio_To_Report"
---

# Window Functions



## Ratio_To_Report


Provides the ratio of the current rows value to all the values within the window.

```sql
--Data
CREATE TABLE Employees (Name Varchar2(30), Salary Number(10));
INSERT INTO Employees Values ('Bob',2500);
INSERT INTO Employees Values ('Alice',3500);
INSERT INTO Employees Values ('Tom',2700);
INSERT INTO Employees Values ('Sue',2000);
--Query
SELECT Name, Salary, Ratio_To_Report(Salary) OVER () As Ratio
FROM Employees
ORDER BY Salary, Name, Ratio;
--Output
NAME                               SALARY      RATIO
------------------------------ ---------- ----------
Sue                                  2000 .186915888
Bob                                  2500  .23364486
Tom                                  2700 .252336449
Alice                                3500 .327102804

```



#### Syntax


- Ratio_To_Report ( expr ) OVER ( query_partition_clause )

