---
metaTitle: "Command line dot-commands"
description: "Exporting and importing a table as an SQL script"
---

# Command line dot-commands




## Exporting and importing a table as an SQL script


Exporting a database is a simple two step process:

```sql
sqlite> .output mydatabase_dump.sql
sqlite> .dump

```

Exporting a table is pretty similar:

```sql
sqlite> .output mytable_dump.sql
sqlite> .dump mytable

```

The output file needs to be defined with `.output` prior to using `.dump`; otherwise, the text is just output to the screen.

Importing is even simpler:

```sql
sqlite> .read mytable_dump.sql

```

