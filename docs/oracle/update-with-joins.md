---
metaTitle: "Oracle Database - Update with Joins"
description: "Examples: what works and what doesn't"
---

# Update with Joins




## Examples: what works and what doesn't


```sql
create table tgt ( id, val ) as 
  select 1, 'a' from dual union all
  select 2, 'b' from dual
;

Table TGT created.

create table src ( id, val ) as 
  select 1, 'x' from dual union all
  select 2, 'y' from dual
;

Table SRC created.

update
  ( select t.val as t_val, s.val as s_val
    from   tgt t inner join src s on t.id = s.id
  )
set t_val = s_val
;

SQL Error: ORA-01779: cannot modify a column which maps to a non key-preserved table
01779. 00000 -  "cannot modify a column which maps to a non key-preserved table"
*Cause:    An attempt was made to insert or update columns of a join view which
           map to a non-key-preserved table.
*Action:   Modify the underlying base tables directly.

```

Imagine what would happen if we had the value `1` in the column `src.id` more than once, with different values for `src.val`. Obviously, the update would make no sense (in ANY database - that's a logical issue). Now, **we** know that there are no duplicates in `src.id`, but the Oracle engine doesn't know that - so it's complaining. Perhaps this is why so many practitioners believe Oracle "doesn't have UPDATE with joins"?

What Oracle expects is that `src.id` should be unique, and that it, Oracle, would know that beforehand. Easily fixed! Note that the same works with composite keys (on more than one column), if the matching for the update needs to use more than one column. In practice, `src.id` may be PK and `tgt.id` may be FK pointing to this PK, but that is not relevant for updates with join; what **is** relevant is the unique constraint.

```sql
alter table src add constraint src_uc unique (id);

Table SRC altered.

update
  ( select t.val as t_val, s.val as s_val
    from   tgt t inner join src s on t.id = s.id
  )
set t_val = s_val
;

2 rows updated.

select * from tgt;

ID  VAL
--  ---
 1  x
 2  y

```

The same result could be achieved with a MERGE statement (which deserves its own Documentation article), and I personally prefer MERGE in these cases, but the reason is not that "Oracle doesn't do updates with joins." As this example shows, Oracle **does** do updates with joins.

