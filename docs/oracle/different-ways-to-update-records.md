---
metaTitle: "Oracle Database - Different ways to update records"
description: "Update using Merge, Update Syntax with example, Update Using Inline View, Merge with sample data"
---

# Different ways to update records



## Update using Merge


**Using Merge**

```sql
MERGE INTO
      TESTTABLE
USING
      (SELECT
            T1.ROWID AS RID,
            T2.TESTTABLE_ID
       FROM
                TESTTABLE T1
            INNER JOIN
                MASTERTABLE T2
            ON TESTTABLE.TESTTABLE_ID = MASTERTABLE.TESTTABLE_ID
       WHERE ID_NUMBER=11)
ON
      ( ROWID = RID )
WHEN MATCHED
THEN
    UPDATE SET TEST_COLUMN= 'Testvalue';

```



## Update Syntax with example


**Normal Update**

```sql
UPDATE
      TESTTABLE
SET
      TEST_COLUMN= 'Testvalue',TEST_COLUMN2= 123
WHERE
      EXISTS
          (SELECT MASTERTABLE.TESTTABLE_ID 
           FROM MASTERTABLE 
           WHERE ID_NUMBER=11);

```



## Update Using Inline View


**Using Inline View (If it is considered updateable by Oracle)**

**Note**: If you face a non key preserved row error add an index to resolve the same to make it update-able

```sql
UPDATE
      (SELECT
            TESTTABLE.TEST_COLUMN AS OLD,
            'Testvalue' AS NEW
       FROM
                TESTTABLE
            INNER JOIN
                MASTERTABLE
            ON TESTTABLE.TESTTABLE_ID = MASTERTABLE.TESTTABLE_ID
       WHERE ID_NUMBER=11) T
SET
      T.OLD        = T.NEW;

```



## Merge with sample data


```sql
drop table table01;
drop table table02;

create table table01 (
       code int,
       name varchar(50),
       old int
);

create table table02 (
       code int,
       name varchar(50),
       old int
);

truncate table table01;
insert into table01 values (1, 'A', 10);
insert into table01 values (9, 'B', 12);
insert into table01 values (3, 'C', 14);
insert into table01 values (4, 'D', 16);
insert into table01 values (5, 'E', 18);

truncate table table02;
insert into table02 values (1, 'AA', null);
insert into table02 values (2, 'BB', 123);
insert into table02 values (3, 'CC', null);
insert into table02 values (4, 'DD', null);
insert into table02 values (5, 'EE', null);

select * from table01 a order by 2;
select * from table02 a order by 2;

--

merge into table02 a using (
      select b.code, b.old from table01 b
) c on (
  a.code = c.code
)
when matched then update set a.old = c.old
;

--

select a.*, b.* from table01 a 
inner join table02 b on a.code = b.codetable01;

select * from table01 a
where 
       exists 
       (
         select 'x' from table02 b where a.code = b.codetable01
       );
       
select * from table01 a where a.code in (select b.codetable01 from table02 b);

--

select * from table01 a
where 
       not exists 
       (
         select 'x' from table02 b where a.code = b.codetable01
       );
       
select * from table01 a where a.code not in (select b.codetable01 from table02 b);

```



#### Syntax


- UPDATE table-Name [[AS] correlation-Name] SET column-Name = Value [ , column-Name = Value} ]* [WHERE clause]
- UPDATE table-Name SET column-Name = Value [ , column-Name = Value ]* WHERE CURRENT OF

