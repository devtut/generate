---
metaTitle: "Microsoft SQL Server - UNION"
description: "Union and union all"
---

# UNION



## Union and union all


**Union** operation combines the results of two or more queries into a single result set that includes all the rows that belong to all queries in the union and will ignore any duplicates that exist. **Union all** also does the same thing but include even the duplicate values. The concept of union operation will be clear from the example below.
Few things to consider while using union are:

1.The number and the order of the columns must be the same in all queries.

2.The data types must be compatible.

Example:

We have three tables : Marksheet1, Marksheet2 and Marksheet3. Marksheet3 is the duplicate table of Marksheet2 which contains same values as that of Marksheet2.

**Table1**: Marksheet1

[<img src="http://i.stack.imgur.com/N3RLb.png" alt="enter image description here" />](http://i.stack.imgur.com/N3RLb.png)

**Table2**: Marksheet2

[<img src="http://i.stack.imgur.com/WHDsX.png" alt="enter image description here" />](http://i.stack.imgur.com/WHDsX.png)

**Table3**: Marksheet3

[<img src="http://i.stack.imgur.com/ES51c.png" alt="enter image description here" />](http://i.stack.imgur.com/ES51c.png)

**Union on tables Marksheet1 and Marksheet2**

```sql
SELECT SubjectCode, SubjectName, MarksObtained 
FROM Marksheet1
UNION 
SELECT CourseCode, CourseName, MarksObtained 
FROM Marksheet2

```

**Note:** The output for union of the three tables will also be same as union on Marksheet1 and Marksheet2 because union operation does not take duplicate values.

```sql
SELECT SubjectCode, SubjectName, MarksObtained 
FROM Marksheet1
UNION 
SELECT CourseCode, CourseName, MarksObtained 
FROM Marksheet2   
UNION
SELECT SubjectCode, SubjectName, MarksObtained 
FROM Marksheet3

```

**OUTPUT**

[<img src="http://i.stack.imgur.com/2P9zv.png" alt="enter image description here" />](http://i.stack.imgur.com/2P9zv.png)

**Union All**

```sql
SELECT SubjectCode, SubjectName, MarksObtained 
FROM Marksheet1
UNION ALL 
SELECT CourseCode, CourseName, MarksObtained 
FROM Marksheet2
UNION ALL
SELECT SubjectCode, SubjectName, MarksObtained 
FROM Marksheet3

```

**OUTPUT**

[<img src="http://i.stack.imgur.com/luvTc.png" alt="enter image description here" />](http://i.stack.imgur.com/luvTc.png)

You will notice here that the duplicate values from Marksheet3 are also displayed using union all.

