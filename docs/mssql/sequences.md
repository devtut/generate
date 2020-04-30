---
metaTitle: "Sequences"
description: "Create Sequence, Use Sequence in Table, Insert Into Table with Sequence, Delete From & Insert New"
---

# Sequences



## Create Sequence


```sql
CREATE SEQUENCE [dbo].[CustomersSeq]
AS INT
START WITH 10001
INCREMENT BY 1
MINVALUE -1;

```



## Use Sequence in Table


```sql
CREATE TABLE [dbo].[Customers]
(
    CustomerID INT DEFAULT (NEXT VALUE FOR [dbo].[CustomersSeq]) NOT NULL,
    CustomerName VARCHAR(100),
);

```



## Insert Into Table with Sequence


```sql
INSERT INTO [dbo].[Customers]
       ([CustomerName])
 VALUES
       ('Jerry'),
       ('Gorge')

SELECT * FROM [dbo].[Customers]

```

**Results**

|CustomerID|CustomerName
|------
|10001|Jerry
|10002|Gorge



## Delete From & Insert New


```sql
DELETE FROM [dbo].[Customers]
WHERE CustomerName = 'Gorge';

INSERT INTO [dbo].[Customers]
       ([CustomerName])
 VALUES ('George')

SELECT * FROM [dbo].[Customers]

```

**Results**

|CustomerID|CustomerName
|------
|10001|Jerry
|10003|George

