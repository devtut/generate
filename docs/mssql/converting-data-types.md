---
metaTitle: "Converting data types"
description: "TRY PARSE, TRY CONVERT, TRY CAST, Cast , Convert"
---

# Converting data types



## TRY PARSE


It converts string data type to target data type(Date or Numeric).

For example, source data is string type and we need to covert to date type. If conversion attempt fails it returns NULL value.

Syntax: TRY_PARSE (string_value AS data_type [ USING culture ])

String_value – This is argument is source value which is NVARCHAR(4000) type.
<br>Data_type – This argument is target data type either date or numeric.
<br>Culture – It is an optional argument which helps to convert the value to in Culture format. Suppose you want to display the date in French, then you need to pass culture type as ‘Fr-FR’. If you will not pass any valid culture name, then PARSE will raise an error.

```sql
DECLARE @fakeDate AS varchar(10);  
DECLARE @realDate AS VARCHAR(10);  
SET @fakeDate = 'iamnotadate'; 
SET @realDate = '13/09/2015';   

SELECT TRY_PARSE(@fakeDate AS DATE); --NULL  as the parsing fails

SELECT TRY_PARSE(@realDate AS DATE); -- NULL due to type mismatch

SELECT TRY_PARSE(@realDate AS DATE USING 'Fr-FR'); -- 2015-09-13 

```



## TRY CONVERT


It converts value to specified data type and if conversion fails it returns NULL. For example, source value in string format and we need date/integer format. Then this will help us to achieve the same.

Syntax: TRY_CONVERT ( data_type [ ( length ) ], expression [, style ] )

TRY_CONVERT()  returns a value cast to the specified data type if the cast succeeds; otherwise, returns null.

<br>Data_type - The datatype into which to convert. Here length is an optional parameter which helps to get result in specified length.
<br>Expression - The value to be convert
<br>Style - It is an optional parameter which determines formatting. Suppose you want date format like “May, 18 2013” then you need pass style as 111.

```sql
DECLARE @sampletext AS VARCHAR(10);  
SET @sampletext = '123456';  
DECLARE @ realDate AS VARCHAR(10);  
SET @realDate = '13/09/2015’;  
SELECT TRY_CONVERT(INT, @sampletext); -- 123456  
SELECT TRY_CONVERT(DATETIME, @sampletext); -- NULL  
SELECT TRY_CONVERT(DATETIME, @realDate, 111); -- Sep, 13 2015  

```



## TRY CAST


It converts value to specified data type and if conversion fails it returns NULL. For example, source value in string format and we need it in double/integer format. Then this will help us in achieving it.

Syntax: TRY_CAST ( expression AS data_type [ ( length ) ] )

TRY_CAST() returns a value cast to the specified data type if the cast succeeds; otherwise, returns null.

Expression - The source value which will go to cast.
<br>Data_type - The target data type the source value will cast.
<br>Length - It is an optional parameter that specifies the length of target data type.

```sql
DECLARE @sampletext AS VARCHAR(10);  
SET @sampletext = '123456';  
  
SELECT TRY_CAST(@sampletext AS INT); -- 123456  
SELECT TRY_CAST(@sampletext AS DATE); -- NULL  

```



## Cast 


The Cast() function is used to convert a data type variable or data from one data type to another data type.

Syntax

CAST ( [Expression] AS Datatype)

The data type to which you are casting an expression is the target type. The data type of the expression from which you are casting is the source type.

```sql
DECLARE @A varchar(2)    
DECLARE @B varchar(2)

set @A='25a'    
set @B='15'

Select CAST(@A as int) + CAST(@B as int)  as Result 
--'25a' is casted to 25 (string to int)
--'15' is casted to 15 (string to int)

--Result 
 --40

DECLARE @C varchar(2)  = 'a'    

select CAST(@C as int) as Result    
--Result
 --Conversion failed when converting the varchar value 'a' to data type int.

```

Throws error if failed



## Convert


When you convert expressions from one type to another, in many cases there will be a need within a stored procedure or other routine to convert data from a datetime type to a varchar type. The Convert function is used for such things. The CONVERT() function can be used to display date/time data in various formats.
Syntax

CONVERT(data_type(length), expression, style)

Style - style values for datetime or smalldatetime conversion to character data.  Add 100 to a style value to get a four-place year that includes the century (yyyy).

```sql
select convert(varchar(20),GETDATE(),108) 

13:27:16

```

