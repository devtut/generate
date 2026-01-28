---
metaTitle: "Microsoft SQL Server - Foreign Keys"
description: "Foreign key relationship/constraint, Maintaining relationship between parent/child rows, Adding foreign key relationship on existing table, Add foreign key on existing table, Getting information about foreign key constraints"
---

# Foreign Keys



## Foreign key relationship/constraint


Foreign keys enables you to define relationship between two tables. One (parent) table need to have primary key that uniquely identifies rows in the table. Other (child) table can have value of the primary key from the parent in one of the columns. FOREIGN KEY REFERENCES constraint ensures that values in child table must exist as a primary key value in the parent table.

In this example we have parent Company table with CompanyId primary key, and child Employee table that has id of the company where this employee works.

```sql
create table Company (
   CompanyId int primary key,
   Name nvarchar(200)
)
create table Employee (
    EmployeeId int,
    Name nvarchar(200),
    CompanyId int
        foreign key references Company(companyId)
)

```

**foreign key references** ensures that values inserted in Employee.CompanyId column must also exist in Company.CompanyId column. Also, nobody can delete company in company table if there is ate least one employee with a matching companyId in child table.

FOREIGN KEY relationship ensures that rows in two tables cannot be "unlinked".



## Maintaining relationship between parent/child rows


Let's assume that we have one row in Company table with companyId 1. We can insert row in employee table that has companyId 1:

```sql
insert into Employee values (17, 'John', 1)

```

However, we cannot insert employee that has non-existing CompanyId:

```sql
insert into Employee values (17, 'John', 111111)

```

Msg 547, Level 16, State 0, Line 12
The INSERT statement conflicted with the FOREIGN KEY constraint "FK__Employee__Compan__1EE485AA". The conflict occurred in database "MyDb", table "dbo.Company", column 'CompanyId'.
The statement has been terminated.

Also, we cannot delete parent row in company table as long as there is at least one child row in employee table that references it.

```sql
delete from company where CompanyId = 1

```

Msg 547, Level 16, State 0, Line 14
The DELETE statement conflicted with the REFERENCE constraint "FK__Employee__Compan__1EE485AA". The conflict occurred in database "MyDb", table "dbo.Employee", column 'CompanyId'.
The statement has been terminated.

Foreign key relationship ensures that Company and employee rows will not be "unlinked".



## Adding foreign key relationship on existing table


**FOREIGN KEY** constraint can be added on existing tables that are still not in relationship. Imagine that we have Company and Employee tables where Employee table CompanyId column but don't have foreign key relationship.
ALTER TABLE statement enables you to add **foreign key** constraint on an existing column that references some other table and primary key in that table:

```sql
alter table Employee
    add  foreign key (CompanyId) references Company(CompanyId)

```



## Add foreign key on existing table


**FOREIGN KEY** columns with constraint can be added on existing tables that are still not in relationship. Imagine that we have Company and Employee tables where Employee table don't have CompanyId column.
ALTER TABLE statement enables you to add new column with **foreign key** constraint that references some other table and primary key in that table:

```sql
alter table Employee
    add CompanyId int foreign key references Company(CompanyId)

```



## Getting information about foreign key constraints


sys.foreignkeys system view returns information about all foreign key relationships in database:

```sql
select name,
 OBJECT_NAME(referenced_object_id) as [parent table],
 OBJECT_NAME(parent_object_id) as [child table],
 delete_referential_action_desc,
 update_referential_action_desc
from sys.foreign_keys

```

