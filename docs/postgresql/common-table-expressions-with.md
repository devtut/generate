---
metaTitle: "PostgreSQL - Common Table Expressions (WITH)"
description: "Common Table Expressions in SELECT Queries, Traversing tree using WITH RECURSIVE"
---

# Common Table Expressions (WITH)



## Common Table Expressions in SELECT Queries


Common table expressions support extracting portions of larger queries. For example:

```sql
WITH sales AS (
  SELECT 
    orders.ordered_at,
    orders.user_id,
    SUM(orders.amount) AS total
  FROM orders
  GROUP BY orders.ordered_at, orders.user_id
)
SELECT 
  sales.ordered_at,
  sales.total,
  users.name
FROM sales 
JOIN users USING (user_id)

```



## Traversing tree using WITH RECURSIVE


```sql
create table empl (
    name text primary key,
    boss text null
        references name 
            on update cascade 
            on delete cascade 
        default null
);

insert into empl values ('Paul',null);
insert into empl values ('Luke','Paul');
insert into empl values ('Kate','Paul');
insert into empl values ('Marge','Kate');
insert into empl values ('Edith','Kate');
insert into empl values ('Pam','Kate');
insert into empl values ('Carol','Luke');
insert into empl values ('John','Luke');
insert into empl values ('Jack','Carol');
insert into empl values ('Alex','Carol');

with recursive t(level,path,boss,name) as (
        select 0,name,boss,name from empl where boss is null
    union
        select
            level + 1,
            path || ' > ' || empl.name,
            empl.boss,
            empl.name 
        from 
            empl join t 
                on empl.boss = t.name
) select * from t order by path;

```

