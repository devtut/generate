---
metaTitle: "Microsoft SQL Server - Generating a range of dates"
description: "Generating Date Range With Recursive CTE, Generating a Date Range With a Tally Table"
---

# Generating a range of dates



## Generating Date Range With Recursive CTE


Using a Recursive CTE, you can generate an inclusive range of dates:

```sql
Declare  @FromDate    Date = '2014-04-21',
         @ToDate      Date = '2014-05-02'

;With DateCte (Date) As
(
    Select  @FromDate Union All
    Select  DateAdd(Day, 1, Date)
    From    DateCte
    Where   Date < @ToDate
)
Select  Date
From    DateCte
Option  (MaxRecursion 0)

```

The default `MaxRecursion` setting is 100.  Generating more than 100 dates using this method will require the `Option (MaxRecursion N)` segment of the query, where `N` is the desired `MaxRecursion` setting.  Setting this to `0` will remove the `MaxRecursion` limitation altogether.



## Generating a Date Range With a Tally Table


Another way you can generate a range of dates is by utilizing a Tally Table to create the dates between the range:

```sql
Declare   @FromDate   Date = '2014-04-21',
          @ToDate     Date = '2014-05-02'

;With 
   E1(N) As (Select 1 From (Values (1), (1), (1), (1), (1), (1), (1), (1), (1), (1)) DT(N)),
   E2(N) As (Select 1 From E1 A Cross Join E1 B),
   E4(N) As (Select 1 From E2 A Cross Join E2 B),
   E6(N) As (Select 1 From E4 A Cross Join E2 B),
   Tally(N) As
   (
        Select    Row_Number() Over (Order By (Select Null)) 
        From    E6
   )
Select   DateAdd(Day, N - 1, @FromDate) Date
From     Tally
Where    N <= DateDiff(Day, @FromDate, @ToDate) + 1

```



#### Parameters


|Parameter|Details
|---|---|---|---
|@FromDate|The inclusive lower boundary of the generated date range.
|@ToDate|The inclusive upper boundary of the generated date range.



#### Remarks


Most experts seem to recommend creating a Dates table instead of generating a sequence on the fly.  See [http://dba.stackexchange.com/questions/86435/filling-in-date-holes-in-grouped-by-date-sql-data](http://dba.stackexchange.com/questions/86435/filling-in-date-holes-in-grouped-by-date-sql-data)

