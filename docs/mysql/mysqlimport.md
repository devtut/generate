---
metaTitle: "MySQL - mysqlimport"
description: "Basic usage, Using a custom field-delimiter, Using a custom row-delimiter, Handling duplicate keys, Conditional import, Import a standard csv"
---

# mysqlimport



## Basic usage


Given the tab-separated file `employee.txt`

> 
<p>1 `\t` Arthur Dent<br/>
2 `\t` Marvin<br/>
3 `\t` Zaphod Beeblebrox<br/></p>


```sql
$ mysql --user=user --password=password mycompany -e 'CREATE TABLE employee(id INT, name VARCHAR(100), PRIMARY KEY (id))'

$ mysqlimport --user=user --password=password mycompany employee.txt

```



## Using a custom field-delimiter


Given the text file employee.txt

> 
<p>1|Arthur Dent<br/>
2|Marvin<br/>
3|Zaphod Beeblebrox<br/></p>


```sql
$ mysqlimport --fields-terminated-by='|' mycompany employee.txt

```



## Using a custom row-delimiter


This example is useful for windows-like endings:

```sql
$ mysqlimport --lines-terminated-by='\r\n' mycompany employee.txt

```



## Handling duplicate keys


Given the table `Employee`

|id|Name
|---|---|---|---
|3|Yooden Vranx

And the file `employee.txt`

> 
<p>1 `\t` Arthur Dent<br/>
2 `\t` Marvin<br/>
3 `\t` Zaphod Beeblebrox<br/></p>


The `--ignore` option will ignore the entry on duplicate keys

```sql
$ mysqlimport --ignore mycompany employee.txt

```

|id|Name
|---|---|---|---
|1|Arthur Dent
|2|Marvin
|3|Yooden Vranx

The `--replace` option will overwrite the old entry

```sql
$ mysqlimport --replace mycompany employee.txt

```

|id|Name
|---|---|---|---
|1|Arthur Dent
|2|Marvin
|3|Zaphod Beeblebrox



## Conditional import


```sql
$ mysqlimport --where="id>2" mycompany employee.txt

```



## Import a standard csv


```sql
$ mysqlimport
    --fields-optionally-enclosed-by='"'
    --fields-terminated-by=,
    --lines-terminated-by="\r\n"
    mycompany employee.csv

```



#### Parameters


|Parameter|Description
|---|---|---|---
|`--delete` `-D`|empty the table before importing the text file
|`--fields-optionally-enclosed-by`|define the character that quotes the fields
|`--fields-terminated-by`|field terminator
|`--ignore` `-i`|ignore the ingested row in case of duplicate-keys
|`--lines-terminated-by`|define row terminator
|`--password` `-p`|password
|`--port` `-P`|port
|`--replace` `-r`|overwrite the old entry row in case of duplicate-keys
|`--user` `-u`|username
|`--where` `-w`|specify a condition



#### Remarks


`mysqlimport` will use the name of the imported file, after stripping the extension, to determine the destination table.

