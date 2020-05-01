---
metaTitle: "EXISTS CLAUSE"
description: "EXISTS CLAUSE"
---

# EXISTS CLAUSE




## EXISTS CLAUSE


Customer Table

|Id|FirstName|LastName
|---|---|---|---
|1|Ozgur|Ozturk
|2|Youssef|Medi
|3|Henry|Tai

Order Table

|Id|CustomerId|Amount
|---|---|---|---
|1|2|123.50
|2|3|14.80

### Get all customers with a least one order

```sql
SELECT * FROM Customer WHERE EXISTS (
    SELECT * FROM Order WHERE Order.CustomerId=Customer.Id
)

```

Result

|Id|FirstName|LastName
|---|---|---|---
|2|Youssef|Medi
|3|Henry|Tai

### Get all customers with no order

```sql
SELECT * FROM Customer WHERE NOT EXISTS (
    SELECT * FROM Order WHERE Order.CustomerId = Customer.Id
)

```

Result

|Id|FirstName|LastName
|---|---|---|---
|1|Ozgur|Ozturk

### Purpose

`EXISTS`, `IN` and `JOIN` could sometime be used for the same result, however, they are not equals :

- `EXISTS` should be used to check if a value exist in another table
- `IN` should be used for static list
- `JOIN` should be used to retrieve data from other(s) table(s)

