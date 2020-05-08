---
metaTitle: "Android - Localized Date/Time in Android"
description: "Custom localized date format with DateUtils.formatDateTime(), Standard date/time formatting in Android, Fully customized date/time"
---

# Localized Date/Time in Android



## Custom localized date format with DateUtils.formatDateTime()


[DateUtils.formatDateTime()](https://developer.android.com/reference/android/text/format/DateUtils.html#formatDateTime(android.content.Context,%20long,%20int)) allows you to supply a time, and based on the flags you provide, it creates a localized datetime string. The flags allow you to specify whether to include specific elements (like the weekday).

formatDateTime() automatically takes care about proper date formats.



## Standard date/time formatting in Android


Format a date:

Format a date and time. Date is in short format, time is in long format:



## Fully customized date/time


Commonly used patterns:

- HH: hour (0-23)
- hh: hour (1-12)
- a: AM/PM marker
- mm: minute (0-59)
- ss: second
- dd: day in month (1-31)
- MM: month
- yyyy: year



#### Remarks


It is recommended to use methods of the [DateUtils](https://developer.android.com/reference/android/text/format/DateUtils.html) class in order to format dates which are locale aware, i.e. which consider user preferences (e.g. 12h/24h clock time formats). These methods are most appropriate for dates that are displayed to the user.

For fully customized date representations, it is recommended to use the [SimpleDateFormat](https://developer.android.com/reference/java/text/SimpleDateFormat.html) class, as it allows to fully control all date elements.

