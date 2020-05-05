---
metaTitle: "PostgreSQL - Comments in postgresql"
description: "COMMENT on Table, Remove Comment"
---

# Comments in postgresql


****COMMMENT**** main purpose is to define or change a comment on database object.

Only a single comment(string) can be given on any database object.COMMENT will help us to know what for the particular database object has been defined whats its actual purpose is.

The rule for ****COMMENT ON ROLE**** is that you must be superuser to comment on a superuser role, or have the ****CREATEROLE**** privilege to comment on non-superuser roles. Of course, a **superuser can comment on anything**



## COMMENT on Table


COMMENT ON TABLE table_name IS 'this is student details table';



## Remove Comment


COMMENT on TABLE student IS NULL;

Comment will be removed with above statement execution.



#### Syntax


- COMMENT ON database_object object_name IS 'Text';



#### Remarks


Full syntax see: [http://www.postgresql.org/docs/current/static/sql-comment.html](http://www.postgresql.org/docs/current/static/sql-comment.html)

