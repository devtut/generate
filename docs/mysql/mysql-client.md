---
metaTitle: "MySQL client"
description: "Base login, Execute commands"
---

# MySQL client



## Base login


To access MySQL from the command line:

```sql
mysql --user=username --password=pwd --host=hostname test_db

```

This can be shortened to:

```sql
mysql -u username -p password -h hostname test_db

```

By omitting the `password` value MySQL will ask for any required password as the first input. If you specify `password` the client will give you an 'insecure' warning:

```sql
mysql -u=username -p -h=hostname test_db

```

For local connections `--socket` can be used to point to the socket file:

```sql
mysql --user=username --password=pwd --host=localhost --socket=/path/to/mysqld.sock test_db

```

Omitting the `socket` parameter will cause the client to attempt to attach to a server on the local machine. The server must be running to connect to it.



## Execute commands


This set of example show how to execute commands stored in strings or script files, without the need of the interactive prompt. This is especially useful to when a shell script needs to interact with a database.

### Execute command from a string

```sql
$ mysql -uroot -proot test -e'select * from people'

+----+-------+--------+
| id | name  | gender |
+----+-------+--------+
|  1 | Kathy | f      |
|  2 | John  | m      |
+----+-------+--------+

```

To format the output as a tab-separated grid, use the `--silent` parameter:

```sql
$ mysql -uroot -proot test -s -e'select * from people'

id      name    gender
1       Kathy   f
2       John    m

```

To omit the headers:

```sql
$ mysql -uroot -proot test -ss -e'select * from people'

1       Kathy   f
2       John    m

```

### Execute from script file:

```sql
$ mysql -uroot -proot test < my_script.sql

```

```sql
$ mysql -uroot -proot test -e'source my_script.sql'

```

### Write the output on a file

```sql
$ mysql -uroot -proot test < my_script.sql > out.txt

$ mysql -uroot -proot test -s -e'select * from people' > out.txt

```



#### Syntax


- mysql [OPTIONS] [database_name]



#### Parameters


|Parameter|Description
|------
|`-D` `--database=name`|name of the database
|`--delimiter=str`|set the statement delimiter. The default one is ';'
|`-e` `--execute='command'`|execute command
|`-h` `--host=name`|hostname to connect to
|`-p` `--password=name`|password **Note: there is no space between `-p` and the password**
|`-p` (without password)|the password will be prompted for
|`-P` `--port=#`|port number
|`-s` `--silent`|silent mode, produce less output. Use `\t` as column separator
|`-ss`|like `-s`, but omit column names
|`-S` `--socket=path`|specify the socket (Unix) or named pipe (Windows) to use when connecting to a local instance
|`--skip-column-names`|omit column names
|`-u` `--user=name`|username
|`-U` `--safe-updates` `--i-am-a-dummy`|login with the variable `sql_safe_updates=ON`. This will allow only `DELETE` and `UPDATE` that explicitly use keys
|`-V` `--version`|print the version and exit

