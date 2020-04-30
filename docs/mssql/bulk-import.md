---
metaTitle: "BULK Import"
description: "BULK INSERT with options, BULK INSERT, Reading entire content of file using OPENROWSET(BULK), Read file using OPENROWSET(BULK) and format file, Read json file using OPENROWSET(BULK)"
---

# BULK Import



## BULK INSERT with options


You can customize parsing rules using different options in WITH clause:

```sql
BULK INSERT People
FROM 'f:\orders\people.csv'  
WITH  (  CODEPAGE = '65001',  
         FIELDTERMINATOR =',',  
         ROWTERMINATOR ='\n'  
      ); 

```

In this example, CODEPAGE specifies that a source file in UTF-8 file, and TERMINATORS are coma and new line.



## BULK INSERT


BULK INSERT command can be used to import file into SQL Server:

```sql
BULK INSERT People
FROM 'f:\orders\people.csv'  

```

BULK INSERT command will map columns in files with columns in target table.



## Reading entire content of file using OPENROWSET(BULK)


You can read content of file using OPENROWSET(BULK) function and store content in some table:

```sql
INSERT INTO myTable(content)   
   SELECT BulkColumn
          FROM OPENROWSET(BULK N'C:\Text1.txt', SINGLE_BLOB) AS Document; 

```

SINGLE_BLOB option will read entire content from a file as single cell.



## Read file using OPENROWSET(BULK) and format file


Yu can define format of the file that will be imported using FORMATFILE option:

```sql
INSERT INTO mytable
SELECT a.* 
FROM OPENROWSET(BULK 'c:\test\values.txt',   
   FORMATFILE = 'c:\test\values.fmt') AS a;  

```

The format file, format_file.fmt, describes the columns in values.txt:

```sql
9.0  
2  
1  SQLCHAR  0  10 "\t"        1  ID                SQL_Latin1_General_Cp437_BIN  
2  SQLCHAR  0  40 "\r\n"      2  Description       SQL_Latin1_General_Cp437_BIN  

```



## Read json file using OPENROWSET(BULK)


You can use OPENROWSET to read content of file and pass it to some other function that will parse results.

The following example shows hot to read entire content of JSON file using OPENROWSET(BULK) and then provide BulkColumn to OPENJSON function that will parse JSON and return columns:

```sql
SELECT book.*
 FROM OPENROWSET (BULK 'C:\JSON\Books\books.json', SINGLE_CLOB) as j
 CROSS APPLY OPENJSON(BulkColumn)
       WITH( id nvarchar(100), name nvarchar(100), price float,
             pages int, author nvarchar(100)) AS book

```

