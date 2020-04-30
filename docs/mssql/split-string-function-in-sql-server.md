---
metaTitle: "Split String function in Sql Server"
description: "Split string in Sql Server 2008/2012/2014 using XML, Split a String in Sql Server 2016, T-SQL Table variable and XML"
---

# Split String function in Sql Server



## Split string in Sql Server 2008/2012/2014 using XML


Since there is no `STRING_SPLIT` function we need to use XML hack to split the string into rows:

**Example:**

```sql
SELECT split.a.value('.', 'VARCHAR(100)') AS Value 
FROM   (SELECT Cast ('<M>' + Replace('A|B|C', '|', '</M><M>')+ '</M>' AS XML) AS Data) AS A 
       CROSS apply data.nodes ('/M') AS Split(a); 

```

**Result:**

```sql
+-----+
|Value|
+-----+
|A    |
+-----+
|B    |
+-----+
|C    |
+-----+

```



## Split a String in Sql Server 2016


In **SQL Server 2016** finally they have introduced Split string function : [**`STRING_SPLIT`**](https://msdn.microsoft.com/en-gb/library/mt684588.aspx)

**Parameters:**
It accepts two parameters

****String****:

> 
<p>Is an expression of any character type (i.e. nvarchar, varchar,
nchar or char).</p>


****separator**** :

> 
<p>Is a single character expression of any character type (e.g.
nvarchar(1), varchar(1), nchar(1) or char(1)) that is used as
separator for concatenated strings.</p>


**Note:** You should always check if the expression is a non-empty string.

**Example:**

```sql
Select Value
From STRING_SPLIT('a|b|c','|')

```

In above example

```sql
String    : 'a|b|c'
separator : '|'

```

**Result :**

```sql
+-----+
|Value|
+-----+
|a    |
+-----+
|b    |
+-----+
|c    |
+-----+

```

**If it's an empty string:**

```sql
SELECT value
FROM STRING_SPLIT('',',')

```

**Result :**

```

 +-----+
  |Value|
  +-----+
1 |     |
  +-----+

```

You can avoid the above situation by adding a `WHERE` clause

```sql
SELECT value
FROM STRING_SPLIT('',',')
WHERE LTRIM(RTRIM(value))<>''

```



## T-SQL Table variable and XML


```sql
Declare @userList Table(UserKey VARCHAR(60))
Insert into @userList values ('bill'),('jcom'),('others')
--Declared a table variable and insert 3 records

Declare @text XML
Select  @text = (
        select UserKey  from @userList for XML Path('user'), root('group')
) 
--Set the XML value from Table 

Select @text

--View the variable value
XML: \<group>\<user>\<UserKey>bill\</UserKey>\</user>\<user>\<UserKey>jcom\</UserKey>\</user>\<user>\<UserKey>others\</UserKey>\</user>\</group>

```

