---
metaTitle: "SQL - Identifier"
description: "Unquoted identifiers"
---

# Identifier


This topic is about identifiers, i.e. syntax rules for names of tables, columns, and other database objects.

Where appropriate, the examples should cover variations used by different SQL implementations, or identify the SQL implementation of the example.



## Unquoted identifiers


Unquoted identifiers can use letters (`a`-`z`), digits (`0`-`9`), and underscore (`_`), and must start with a letter.

Depending on SQL implementation, and/or database settings, other characters may be allowed, some even as the first character, e.g.

- MS SQL: `@`, `$`, `#`, and other Unicode letters **([source](https://docs.microsoft.com/en-us/sql/relational-databases/databases/database-identifiers))**
- MySQL: `$` **([source](https://dev.mysql.com/doc/refman/5.7/en/identifiers.html))**
- Oracle: `$`, `#`, and other letters from database character set **([source](https://docs.oracle.com/database/121/SQLRF/sql_elements008.htm#SQLRF00223))**
- PostgreSQL: `$`, and other Unicode letters **([source](https://www.postgresql.org/docs/current/static/sql-syntax-lexical.html))**

Unquoted identifiers are case-insensitive. How this is handled depends greatly on SQL implementation:

<li>
MS SQL: Case-preserving, sensitivity defined by database character set, so can be case-sensitive.
</li>
<li>
MySQL: Case-preserving, sensitivity depends on database setting and underlying file system.
</li>
<li>
Oracle: Converted to uppercase, then handled like quoted identifier.
</li>
<li>
PostgreSQL: Converted to lowercase, then handled like quoted identifier.
</li>
<li>
SQLite: Case-preserving; case insensitivity only for ASCII characters.
</li>

