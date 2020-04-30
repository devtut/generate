---
metaTitle: "Dates, Timestamps, and Intervals"
description: "Cast a timestamp or interval to a string, SELECT the last day of month, Count the number of records per week"
---

# Dates, Timestamps, and Intervals



## Cast a timestamp or interval to a string


You can convert a `timestamp` or `interval` value to a string with the `to_char()` function:

```sql
SELECT to_char('2016-08-12 16:40:32'::timestamp, 'DD Mon YYYY HH:MI:SSPM');

```

This statement will produce the string "12 Aug 2016 04:40:32PM". The formatting string can be modified in many different ways; the full list of template patterns can be found [here](https://www.postgresql.org/docs/current/static/functions-formatting.html).

Note that you can also insert plain text into the formatting string and you can use the template patterns in any order:

```sql
SELECT to_char('2016-08-12 16:40:32'::timestamp, 
               '"Today is "FMDay", the "DDth" day of the month of "FMMonth" of "YYYY');

```

This will produce the string "Today is Saturday, the 12th day of the month of August of 2016". You should keep in mind, though, that any template patterns - even the single letter ones like "I", "D", "W" - are converted, unless the plain text is in double quotes. As a safety measure, you should put all plain text in double quotes, as done above.

You can localize the string to your language of choice (day and month names) by using the TM (translation mode) modifier. This option uses the localization setting of the server running PostgreSQL or the client connecting to it.

```sql
SELECT to_char('2016-08-12 16:40:32'::timestamp, 'TMDay, DD" de "TMMonth" del año "YYYY');

```

With a Spanish locale setting this produces "Sábado, 12 de Agosto del año 2016".



## SELECT the last day of month


You can select the last day of month.

```sql
SELECT (date_trunc('MONTH', ('201608'||'01')::date) + INTERVAL '1 MONTH - 1 day')::DATE; 

```

`201608` is replaceable with a variable.



## Count the number of records per week


SELECT date_trunc('week', <>) AS "Week" , count(*)
FROM <>
GROUP BY 1
ORDER BY 1;

