---
metaTitle: Date Formatting
description: Time between two date-times, Outputting datetime object to string, Parsing string to datetime object
---

# Date Formatting



## Time between two date-times


```
from datetime import datetime

a = datetime(2016,10,06,0,0,0)
b = datetime(2016,10,01,23,59,59)

a-b 
# datetime.timedelta(4, 1)

(a-b).days
# 4
(a-b).total_seconds()
# 518399.0

```



## Outputting datetime object to string


Uses C standard [format codes](https://docs.python.org/2/library/datetime.html#strftime-strptime-behavior).

```
from datetime import datetime
datetime_for_string = datetime(2016,10,1,0,0)
datetime_string_format = '%b %d %Y, %H:%M:%S'
datetime.strftime(datetime_for_string,datetime_string_format)
# Oct 01 2016, 00:00:00

```



## Parsing string to datetime object


Uses C standard [format codes](https://docs.python.org/2/library/datetime.html#strftime-strptime-behavior).

```
from datetime import datetime
datetime_string = 'Oct 1 2016, 00:00:00'
datetime_string_format = '%b %d %Y, %H:%M:%S'
datetime.strptime(datetime_string, datetime_string_format)
# datetime.datetime(2016, 10, 1, 0, 0)

```

