---
metaTitle: "Microsoft SQL Server - Row-level security"
description: "RLS filter predicate, Altering RLS security policy, Preventing updated using RLS block predicate"
---

# Row-level security



## RLS filter predicate


Sql Server 2016+ and Azure Sql database enables you to automatically filter rows that are returned in select statement using some predicate. This feature is called **Row-level security**.

First, you need a table-valued function that contains some predicate that describes what it the condition that will allow users to read data from some table:

```sql
DROP FUNCTION IF EXISTS dbo.pUserCanAccessCompany
GO
CREATE FUNCTION

dbo.pUserCanAccessCompany(@CompanyID int)

    RETURNS TABLE
    WITH SCHEMABINDING
AS RETURN (
    SELECT 1 as canAccess WHERE 

    CAST(SESSION_CONTEXT(N'CompanyID') as int) = @CompanyID

)

```

In this example, the predicate says that only users that have a value in SESSION_CONTEXT that is matching input argument can access the company. You can put any other condition e.g. that checks database role or database_id of the current user, etc.

> 
<p>Most of the code above is a template that you will copy-paste. The
only thing that will change here is the name and arguments of
predicate and condition in WHERE clause. Now you create security
policy that will apply this predicate on some table.</p>


Now you can create security policy that will apply predicate on some table:

```sql
CREATE SECURITY POLICY dbo.CompanyAccessPolicy
    ADD FILTER PREDICATE dbo.pUserCanAccessCompany(CompanyID) ON dbo.Company
    WITH (State=ON)

```

This security policy assigns predicate to company table. Whenever someone tries to read data from Company table , security policy will apply predicate on each row, pass CompanyID column as a parameter of the predicate, and predicate will evaluate should this row be returned in the result of SELECT query.



## Altering RLS security policy


Security policy is a group of predicates associated to tables that can be managed together. You can add, or remove predicates or turn on/off entire policy.

You can add more predicates on tables in the existing security policy.

```sql
ALTER SECURITY POLICY dbo.CompanyAccessPolicy
    ADD FILTER PREDICATE dbo.pUserCanAccessCompany(CompanyID) ON dbo.Company

```

You can drop some predicates from security policy:

```sql
ALTER SECURITY POLICY dbo.CompanyAccessPolicy
    DROP FILTER PREDICATE ON dbo.Company

```

You can disable security policy

```sql
ALTER SECURITY POLICY dbo.CompanyAccessPolicy WITH ( STATE = OFF );  

```

You can enable security policy that was disabled:

```sql
ALTER SECURITY POLICY dbo.CompanyAccessPolicy WITH ( STATE = ON );  

```



## Preventing updated using RLS block predicate


Row-level security enables you to define some predicates that will control who could update rows in the table.
First you need to define some table-value function that represents predicate that wll control access policy.

CREATE FUNCTION

dbo.pUserCanAccessProduct(@CompanyID int)

```sql
RETURNS TABLE
WITH SCHEMABINDING

```

AS RETURN (
SELECT 1 as canAccess WHERE

CAST(SESSION_CONTEXT(N'CompanyID') as int) = @CompanyID

)
In this example, the predicate says that only users that have a value in SESSION_CONTEXT that is matching input argument can access the company. You can put any other condition e.g. that checks database role or database_id of the current user, etc.

> 
<p>Most of the code above is a template that you will copy-paste. The
only thing that will change here is the name and arguments of
predicate and condition in WHERE clause. Now you create security
policy that will apply this predicate on some table.</p>


Now we can create security policy with the predicate that will block updates on product table if CompanyID column in table do not satisfies predicate.

CREATE SECURITY POLICY dbo.ProductAccessPolicy
ADD BLOCK PREDICATE dbo.pUserCanAccessProduct(CompanyID) ON dbo.Product

This predicate will be applied on all operations. If you want to apply predicate on some operation you can write something like:

CREATE SECURITY POLICY dbo.ProductAccessPolicy
ADD BLOCK PREDICATE dbo.pUserCanAccessProduct(CompanyID) ON dbo.Product AFTER INSERT

Possible options that you can add after block predicate definition are:

[ { AFTER { INSERT | UPDATE } }<br />
| { BEFORE { UPDATE | DELETE } } ]

