---
metaTitle: "Dynamic Un-Pivot Table using Prepared Statement"
description: "Un-pivot a dynamic set of columns based on condition"
---

# Dynamic Un-Pivot Table using Prepared Statement



## Un-pivot a dynamic set of columns based on condition


The following example is a very useful basis when you are trying to convert transaction data to un-pivoted data for BI/reporting reasons, where the dimensions which are to be un-pivoted can have a dynamic set of columns.

For our example, we suppose that the raw data table contains employee assessment data in the form of marked questions.

The raw data table is the following:

```sql
create table rawdata

(
 PersonId VARCHAR(255)
,Question1Id INT(11)
,Question2Id INT(11)
,Question3Id INT(11)
)  

```

The rawdata table is a temporary table as part of the ETL procedure and can have a varying number of questions. The goal is to use the same un-pivoting procedure for an arbitrary number of Questions, namely columns that are going to be un-pivoted.

Below is a toy example of rawdata table:

[<img src="http://i.stack.imgur.com/XC8KC.png" alt="enter image description here" />](http://i.stack.imgur.com/XC8KC.png)

The well-known,static way to unpivot the data, in MYSQL is by using UNION ALL:

```sql
create table unpivoteddata

(

 PersonId VARCHAR(255)
,QuestionId VARCHAR(255)
,QuestionValue INT(11)

);

INSERT INTO unpivoteddata SELECT PersonId, 'Question1Id' col, Question1Id 
FROM rawdata
UNION ALL
SELECT PersonId, 'Question2Id' col, Question2Id 
FROM rawdata
UNION ALL
SELECT PersonId, 'Question3Id' col, Question3Id 
FROM rawdata; 

```

In our case we want to define a way to unpivot an arbitrary number of QuestionId columns. For that we need to execute a prepared statement that is a dynamic select of the desired columns.
In order to be able to choose which columns need to be un-pivoted, we will use a GROUP_CONCAT statement and we will choose the columns for which the data type is set to 'int'. In the GROUP_CONCAT we also include all additional elements of our SELECT statement to-be executed.

```sql
set @temp2 = null; 

SELECT GROUP_CONCAT(' SELECT ', 'PersonId',',','''',COLUMN_NAME,'''', ' col     ',',',COLUMN_NAME,' FROM rawdata' separator ' UNION ALL' ) FROM     INFORMATION_SCHEMA.COLUMNS WHERE table_name = 'rawdata' AND DATA_TYPE = 'Int' INTO     @temp2;

select @temp2;

```

In another occasion we could have chosen columns that the column name matches a pattern, for example instead of

```sql
DATA_TYPE = 'Int'

```

use

```sql
COLUMN_NAME LIKE 'Question%'

```

or something suitable that can be controlled through the ETL phase.

The prepared statement is finalized as follows:

```sql
set @temp3 = null;

select concat('INSERT INTO unpivoteddata',@temp2) INTO @temp3;

select @temp3;

prepare stmt FROM @temp3;
execute stmt;
deallocate prepare stmt;

```

The unpivoteddata table is the following:

```sql
SELECT * FROM unpivoteddata

```

[<img src="http://i.stack.imgur.com/2DZRJ.png" alt="enter image description here" />](http://i.stack.imgur.com/2DZRJ.png)

Selecting columns according to a condition and then crafting a prepared statement is an efficient way of dynamically un-pivoting data.

