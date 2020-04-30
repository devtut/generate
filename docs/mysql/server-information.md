---
metaTitle: "Server Information"
description: "SHOW VARIABLES example, SHOW STATUS example"
---

# Server Information




## SHOW VARIABLES example


To get all the server variables run this query either in the SQL window of your preferred interface (PHPMyAdmin or other) or in the MySQL CLI interface

```sql
SHOW VARIABLES;

```

You can specify if you want the session variables or the global variables as follows:

Session variables:

```sql
SHOW SESSION VARIABLES;

```

Global variables:

```sql
SHOW GLOBAL VARIABLES;

```

Like any other SQL command you can add parameters to your query such as the LIKE command:

```sql
SHOW [GLOBAL | SESSION] VARIABLES LIKE 'max_join_size';

```

Or, using wildcards:

```sql
SHOW [GLOBAL | SESSION] VARIABLES LIKE '%size%';

```

You can also filter the results of the SHOW query using a WHERE parameter as follows:

```sql
SHOW [GLOBAL | SESSION] VARIABLES WHERE VALUE > 0;

```



## SHOW STATUS example


To get the database server status run this query in either the SQL window of your preferred interface (PHPMyAdmin or other) or on the MySQL CLI interface.

```sql
SHOW STATUS;

```

You can specify whether you wish to receive the SESSION or GLOBAL status of your sever like so:
Session status:

```sql
SHOW SESSION STATUS;

```

Global status:

```sql
SHOW GLOBAL STATUS;

```

Like any other SQL command you can add parameters to your query such as the LIKE command:

```sql
SHOW [GLOBAL | SESSION] STATUS LIKE 'Key%';

```

Or the Where command:

```sql
SHOW [GLOBAL | SESSION] STATUS WHERE VALUE > 0;

```

The main difference between GLOBAL and SESSION is that with the GLOBAL modifier the command displays aggregated information about the server and all of it's connections, while the SESSION modifier will only show the values for the current connection.



#### Parameters


|Parameters|Explanation
|------
|GLOBAL|Shows the variables as they are configured for the entire server. Optional.
|SESSION|Shows the variables that are configured for this session only. Optional.

