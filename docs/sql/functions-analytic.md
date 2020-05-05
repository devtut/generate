---
metaTitle: "SQL - Functions (Analytic)"
description: "LAG and LEAD, PERCENTILE_DISC and PERCENTILE_CONT, FIRST_VALUE, LAST_VALUE, PERCENT_RANK and CUME_DIST"
---

# Functions (Analytic)


You use analytic functions to determine values based on groups of values. For example, you can use this type of function to determine running totals, percentages, or the top result within a group.



## LAG and LEAD


The `LAG` function provides data on rows before the current row in the same result set. For example, in a `SELECT` statement, you can compare values in the current row with values in a previous row.

You use a scalar expression to specify the values that should be compared. The offset parameter is the number of rows before the current row that will be used in the comparison. If you don't specify the number of rows, the default value of one row is used.

The default parameter specifies the value that should be returned when the expression at offset has a `NULL` value. If you don't specify a value, a value of `NULL` is returned.

The `LEAD` function provides data on rows after the current row in the row set. For example, in a `SELECT` statement, you can compare values in the current row with values in the following row.

You specify the values that should be compared using a scalar expression. The offset parameter is the number of rows after the current row that will be used in the comparison.

You specify the value that should be returned when the expression at offset has a `NULL` value using the default parameter. If you don't specify these parameters, the default of one row is used and a value of `NULL` is returned.

```sql
SELECT BusinessEntityID, SalesYTD,
       LEAD(SalesYTD, 1, 0) OVER(ORDER BY BusinessEntityID) AS "Lead value",
       LAG(SalesYTD, 1, 0) OVER(ORDER BY BusinessEntityID) AS "Lag value"
FROM SalesPerson;

```

This example uses the LEAD and LAG functions to compare the sales values for each employee to date with those of the employees listed above and below, with records ordered based on the BusinessEntityID column.

|BusinessEntityID|SalesYTD|Lead value|Lag value
|---|---|---|---|---
|274|559697.5639|3763178.1787|0.0000
|275|3763178.1787|4251368.5497|559697.5639
|276|4251368.5497|3189418.3662|3763178.1787
|277|3189418.3662|1453719.4653|4251368.5497
|278|1453719.4653|2315185.6110|3189418.3662
|279|2315185.6110|1352577.1325|1453719.4653



## PERCENTILE_DISC and PERCENTILE_CONT


The `PERCENTILE_DISC` function lists the value of the first entry where the cumulative distribution is higher than the percentile that you provide using the `numeric_literal` parameter.

The values are grouped by rowset or partition, as specified by the `WITHIN GROUP` clause.

The `PERCENTILE_CONT` function is similar to the `PERCENTILE_DISC` function, but returns the average of the sum of the first matching entry and the next entry.

```sql
SELECT BusinessEntityID, JobTitle, SickLeaveHours,
       CUME_DIST() OVER(PARTITION BY JobTitle ORDER BY SickLeaveHours ASC)
       AS "Cumulative Distribution",
       PERCENTILE_DISC(0.5) WITHIN GROUP(ORDER BY SickLeaveHours)
          OVER(PARTITION BY JobTitle) AS "Percentile Discreet"
FROM Employee;

```

To find the exact value from the row that matches or exceeds the 0.5 percentile, you pass the percentile as the numeric literal in the `PERCENTILE_DISC` function. The Percentile Discreet column in a result set lists the value of the row at which the cumulative distribution is higher than the specified percentile.

|BusinessEntityID|JobTitle|SickLeaveHours|Cumulative Distribution|Percentile Discreet
|---|---|---|---|---
|272|Application Specialist|55|0.25|**56**
|268|Application Specialist|56|0.75|**56**
|269|Application Specialist|56|0.75|**56**
|267|Application Specialist|57|1|**56**

To base the calculation on a set of values, you use the `PERCENTILE_CONT` function. The "Percentile Continuous" column in the results lists the average value of the sum of the result value and the next highest matching value.

```sql
SELECT BusinessEntityID, JobTitle, SickLeaveHours,
       CUME_DIST() OVER(PARTITION BY JobTitle ORDER BY SickLeaveHours ASC)
       AS "Cumulative Distribution",
       PERCENTILE_DISC(0.5) WITHIN GROUP(ORDER BY SickLeaveHours) 
          OVER(PARTITION BY JobTitle) AS "Percentile Discreet",
       PERCENTILE_CONT(0.5) WITHIN GROUP(ORDER BY SickLeaveHours) 
          OVER(PARTITION BY JobTitle) AS "Percentile Continuous"
FROM Employee;

```

|BusinessEntityID|JobTitle|SickLeaveHours|Cumulative Distribution|Percentile Discreet|Percentile Continuous
|---|---|---|---|---|---|---|---|---|---
|272|Application Specialist|55|0.25|56|**56**
|268|Application Specialist|56|0.75|56|**56**
|269|Application Specialist|56|0.75|56|**56**
|267|Application Specialist|57|1|56|**56**



## FIRST_VALUE


You use the `FIRST_VALUE` function to determine the first value in an ordered result set, which you identify using a scalar expression.

```sql
SELECT StateProvinceID, Name, TaxRate,
       FIRST_VALUE(StateProvinceID)
        OVER(ORDER BY TaxRate ASC) AS FirstValue
FROM SalesTaxRate;

```

In this example, the `FIRST_VALUE` function is used to return the `ID` of the state or province with the lowest tax rate. The `OVER` clause is used to order the tax rates to obtain the lowest rate.

|StateProvinceID|Name|TaxRate|FirstValue
|---|---|---|---|---
|74|Utah State Sales Tax|5.00|74
|36|Minnesota State Sales Tax|6.75|74
|30|Massachusetts State Sales Tax|7.00|74
|1|Canadian GST|7.00|74
|57|Canadian GST|7.00|74
|63|Canadian GST|7.00|74



## LAST_VALUE


The `LAST_VALUE` function provides the last value in an ordered result set, which you specify using a scalar expression.

```sql
SELECT TerritoryID, StartDate, BusinessentityID,
       LAST_VALUE(BusinessentityID) 
        OVER(ORDER BY TerritoryID) AS LastValue
FROM SalesTerritoryHistory;

```

This example uses the `LAST_VALUE` function to return the last value for each rowset in the ordered values.

|TerritoryID|StartDate|BusinessentityID|LastValue
|---|---|---|---|---
|1|2005-07-01 00.00.00.000|280|283
|1|2006-11-01 00.00.00.000|284|283
|1|2005-07-01 00.00.00.000|283|283
|2|2007-01-01 00.00.00.000|277|275
|2|2005-07-01 00.00.00.000|275|275
|3|2007-01-01 00.00.00.000|275|277



## PERCENT_RANK and CUME_DIST


The `PERCENT_RANK` function calculates the ranking of a row relative to the row set. The percentage is based on the number of rows in the group that have a lower value than the current row.

The first value in the result set always has a percent rank of zero. The value for the highest-ranked – or last – value in the set is always one.

The `CUME_DIST` function calculates the relative position of a specified value in a group of values, by determining the percentage of values less than or equal to that value. This is called the cumulative distribution.

```sql
SELECT BusinessEntityID, JobTitle, SickLeaveHours,
PERCENT_RANK() OVER(PARTITION BY JobTitle ORDER BY SickLeaveHours DESC)
       AS "Percent Rank",
CUME_DIST() OVER(PARTITION BY JobTitle ORDER BY SickLeaveHours DESC)
       AS "Cumulative Distribution"
FROM Employee;

```

In this example, you use an `ORDER` clause to partition – or group – the rows retrieved by the `SELECT` statement based on employees' job titles, with the results in each group sorted based on the numbers of sick leave hours that employees have used.

|BusinessEntityID|JobTitle|SickLeaveHours|Percent Rank|Cumulative Distribution
|---|---|---|---|---
|267|Application Specialist|57|0|0.25
|268|Application Specialist|56|0.333333333333333|0.75
|269|Application Specialist|56|0.333333333333333|0.75
|272|Application Specialist|55|1|1
|262|Assitant to the Cheif Financial Officer|48|0|1
|239|Benefits Specialist|45|0|1
|252|Buyer|50|0|0.111111111111111
|251|Buyer|49|0.125|0.333333333333333
|256|Buyer|49|0.125|0.333333333333333
|253|Buyer|48|0.375|0.555555555555555
|254|Buyer|48|0.375|0.555555555555555

The `PERCENT_RANK` function ranks the entries within each group. For each entry, it returns the percentage of entries in the same group that have lower values.

The `CUME_DIST` function is similar, except that it returns the percentage of values less than or equal to the current value.



#### Syntax


<li>FIRST_VALUE ( scalar_expression )
OVER ( [ partition_by_clause ] order_by_clause [ rows_range_clause ] )</li>
<li>LAST_VALUE ( scalar_expression )
OVER ( [ partition_by_clause ] order_by_clause [ rows_range_clause ] )</li>
<li>LAG (scalar_expression [,offset] [,default])
OVER ( [ partition_by_clause ] order_by_clause )</li>
<li>LEAD ( scalar_expression [ ,offset ] , [ default ] )
 OVER ( [ partition_by_clause ] order_by_clause )</li>
1. PERCENT_RANK( ) OVER ( [ partition_by_clause ] order_by_clause )
<li>CUME_DIST( )
     OVER ( [ partition_by_clause ] order_by_clause )</li>
<li>PERCENTILE_DISC ( numeric_literal ) WITHIN GROUP ( ORDER BY order_by_expression [ ASC | DESC ] )
OVER ( [ <partition_by_clause> ] )</li>
<li>PERCENTILE_CONT ( numeric_literal ) WITHIN GROUP ( ORDER BY order_by_expression [ ASC | DESC ] )
OVER ( [ <partition_by_clause> ] )</li>

