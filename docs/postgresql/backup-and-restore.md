---
metaTitle: "PostgreSQL - Backup and Restore"
description: "Backing up one database, Restoring backups, Backing up the whole cluster, Using psql to export data, Using Copy to import, Using Copy to export"
---

# Backup and Restore



## Backing up one database


```sql
pg_dump -Fc -f DATABASE.pgsql DATABASE

```

The `-Fc` selects the "custom backup format" which gives you more power than raw SQL; see `pg_restore` for more details. If you want a vanilla SQL file, you can do this instead:

```sql
pg_dump -f DATABASE.sql DATABASE

```

or even

```sql
pg_dump DATABASE > DATABASE.sql

```



## Restoring backups


```sql
psql < backup.sql

```

A safer alternative uses `-1` to wrap the restore in a transaction. The `-f` specifies the filename rather than using shell redirection.

```sql
psql -1f backup.sql

```

Custom format files must be restored using `pg_restore` with the `-d` option to specify the database:

```sql
pg_restore -d DATABASE DATABASE.pgsql

```

The custom format can also be converted back to SQL:

```sql
pg_restore backup.pgsql > backup.sql

```

Usage of the custom format is recommended because you can choose which things to restore and optionally enable parallel processing.

You may need to do a pg_dump followed by a pg_restore if you upgrade from one postgresql release to a newer one.



## Backing up the whole cluster


```sql
$ pg_dumpall -f backup.sql

```

This works behind the scenes by making multiple connections to the server once for each database and executing `pg_dump` on it.

Sometimes, you might be tempted to set this up as a cron job, so you want to see the date the backup was taken as part of the filename:

```sql
$ postgres-backup-$(date +%Y-%m-%d).sql

```

However, please note that this could produce large files on a daily basis. Postgresql has a much better mechanism for regular backups - [WAL archives](https://www.postgresql.org/docs/9.2/static/continuous-archiving.html)

The output from pg_dumpall is sufficient to restore to an identically-configured Postgres instance, but the configuration files in `$PGDATA` (`pg_hba.conf` and `postgresql.conf`) are not part of the backup, so you'll have to back them up separately.

```sql
postgres=# SELECT pg_start_backup('my-backup');
postgres=# SELECT pg_stop_backup();

```

To take a filesystem backup, you must use these functions to help ensure that Postgres is in a consistent state while the backup is prepared.



## Using psql to export data


Data can be exported using copy command or by taking use of command line options of psql command.

**To Export csv data from table user to csv file:**

```sql
psql -p \<port> -U \<username> -d \<database> -A -F<delimiter> -c\<sql to execute> \> \<output filename with path>

psql -p 5432 -U postgres -d test_database -A -F, -c "select * from user" > /home/user/user_data.csv

```

Here combination of -A and -F does the trick.

`-F` is to specify delimiter

```sql
-A or --no-align

```

Switches to unaligned output mode. (The default output mode is otherwise aligned.)



## Using Copy to import


### To Copy Data from a CSV file to a table

```sql
COPY <tablename> FROM '<filename with path>';

```

**To insert into table `user` from a file named `user_data.csv` placed inside `/home/user/`:**

```sql
COPY user FROM '/home/user/user_data.csv';

```

### To Copy data from pipe separated file to table

```sql
COPY user FROM '/home/user/user_data' WITH DELIMITER '|';

```

Note: In absence of the option `with delimiter`, the default delimiter is comma `,`

### To ignore header line while importing file

Use the Header option:

```sql
COPY user FROM '/home/user/user_data' WITH DELIMITER '|' HEADER;

```

Note: If data is quoted, by default data quoting characters are double quote. If the data is quoted using any other character use the `QUOTE` option; however, this option is allowed only when using CSV format.



## Using Copy to export


### To Copy table to standard o/p

COPY <tablename> TO STDOUT (DELIMITER '|');

To export table user to Standard ouput:

COPY user TO STDOUT (DELIMITER '|');

### To Copy table to file

COPY user FROM '/home/user/user_data' WITH DELIMITER '|';

### To Copy the output of SQL statement to file

COPY (sql statement) TO '<filename with path>';

COPY (SELECT * FROM user WHERE user_name LIKE 'A%') TO '/home/user/user_data';

### To Copy into a compressed file

COPY user TO PROGRAM 'gzip > /home/user/user_data.gz';

Here program gzip is executed to compress user table data.



#### Remarks


### Backing up the filesystem instead of using `pg_dumpall` and `pg_dump`

It's very important that if you use this, you call the `pg_start_backup()` function before and `pg_stop_backup()` function after. Doing filesystem backups is not safe otherwise; even a ZFS or FreeBSD snapshot of the filesystem backed up without those function calls will place the database in recovery mode and may lose transactions.

I would avoid doing filesystem backups instead of regular Postgres backups, both for this reason, and because Postgres backup files (especially in the custom format) are extremely versatile in supporting alternate restores. Since they're single files, they're also less hassle to manage.

