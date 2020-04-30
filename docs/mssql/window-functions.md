---
metaTitle: "Window functions"
description: "Centered Moving Average, Find the single most recent item in a list of timestamped events, Moving Average of last 30 Items"
---

# Window functions



## Centered Moving Average


Calculate a 6-month (126-business-day) centered moving average of a price:

```sql
SELECT TradeDate, AVG(Px) OVER (ORDER BY TradeDate ROWS BETWEEN 63 PRECEDING AND 63 FOLLOWING) AS PxMovingAverage
FROM HistoricalPrices

```

Note that, because it will take **up to** 63 rows before and after each returned row, at the beginning and end of the TradeDate range it will not be centered: When it reaches the largest TradeDate it will only be able to find 63 preceding values to include in the average.



## Find the single most recent item in a list of timestamped events


In tables recording events there is often a datetime field recording the time an event happened. Finding the single most recent event can be difficult because it's always possible that two events were recorded with exactly identical timestamps. You can use row_number() over (order by ...) to make sure all records are uniquely ranked, and select the top one (where my_ranking=1)

```sql
select *
from (
    select 
        *,
        row_number() over (order by crdate desc) as my_ranking
    from sys.sysobjects
) g
where my_ranking=1

```

This same technique can be used to return a single row from any dataset with potentially duplicate values.



## Moving Average of last 30 Items


Moving Average of last 30 Items sold

```sql
SELECT
    value_column1,
    (   SELECT
            AVG(value_column1) AS moving_average
        FROM Table1 T2
        WHERE ( SELECT
                    COUNT(*)
                FROM Table1 T3
                WHERE date_column1 BETWEEN T2.date_column1 AND T1.date_column1
                ) BETWEEN 1 AND 30
    ) as MovingAvg
FROM Table1 T1

```

