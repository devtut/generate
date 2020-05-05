---
metaTitle: "MySQL - Replication"
description: "Master - Slave Replication Setup, Replication Errors"
---

# Replication



## Master - Slave Replication Setup


Consider 2 MySQL Servers for replication setup, one is a Master and the other is a Slave.

We are going to configure the Master that it should keep a log of every action performed on it. We are going to configure the Slave server that it should look at the log on the Master and whenever changes happens in log on the Master, it should do the same thing.

****Master Configuration****

First of all, we need to create a user on the Master. This user is going to be used by Slave to create a connection with the Master.

```sql
CREATE USER 'user_name'@'%' IDENTIFIED BY 'user_password';
GRANT REPLICATION SLAVE ON *.* TO 'user_name'@'%';
FLUSH PRIVILEGES;

```

Change `user_name` and `user_password` according to your Username and Password.

Now `my.inf` (my.cnf in Linux) file should be edited. Include the following lines in [mysqld] section.

```sql
server-id = 1
log-bin = mysql-bin.log
binlog-do-db = your_database

```

The first line is used to assign an ID to this MySQL server.

The second line tells MySQL to start writing a log in the specified log file. In Linux this can be configured like `log-bin = /home/mysql/logs/mysql-bin.log`.
If you are starting replication in a MySQL server in which replication has already been used, make sure this directory is empty of all replication logs.

The third line is used to configure the database for which we are going to write log. You should replace `your_database` with your database name.

Make sure `skip-networking` has not been enabled and restart the MySQL server(Master)

****Slave Configuration****

`my.inf` file should be edited in Slave also. Include the following lines in [mysqld] section.

```sql
server-id = 2
master-host = master_ip_address
master-connect-retry = 60

master-user = user_name
master-password = user_password
replicate-do-db = your_database

relay-log = slave-relay.log
relay-log-index = slave-relay-log.index

```

The first line is used to assign an ID to this MySQL server. This ID should be unique.

The second line is the I.P address of the Master server. Change this according to your Master system I.P.

The third line is used to set a retry limit in seconds.

The next two lines tell the username and password to the Slave, by using which it connect the Master.

Next line set the database it needs to replicate.

The last two lines used to assign `relay-log` and `relay-log-index` file names.

Make sure `skip-networking` has not been enabled and restart the MySQL server(Slave)

****Copy Data to Slave****

If data is constantly being added to the Master, we will have to prevent all database access on the Master so nothing can be added. This can be achieved by run the following statement in Master.

```sql
FLUSH TABLES WITH READ LOCK;

```

If no data is being added to the server, you can skip the above step.

We are going to take data backup of the Master by using `mysqldump`

```sql
mysqldump your_database -u root -p > D://Backup/backup.sql;

```

Change `your_database` and backup directory according to your setup. You wll now have a file called `backup.sql` in the given location.

If your database not exists in your Slave, create that by executing the following

```sql
CREATE DATABASE `your_database`;

```

Now we have to import backup into Slave MySQL server.

```sql
mysql -u root -p your_database  <D://Backup/backup.sql
--->Change `your_database` and backup directory according to your setup

```

****Start Replication****

To start replication, we need to find the log file name and log position in the Master. So, run the following in Master

```sql
SHOW MASTER STATUS;

```

This will give you an output like below

```sql
+---------------------+----------+-------------------------------+------------------+
| File                | Position | Binlog_Do_DB                  | Binlog_Ignore_DB |
+---------------------+----------+-------------------------------+------------------+
| mysql-bin.000001    | 130      | your_database                 |                  |
+---------------------+----------+-------------------------------+------------------+

```

Then run the following in Slave

```sql
SLAVE STOP;
CHANGE MASTER TO MASTER_HOST='master_ip_address', MASTER_USER='user_name', 
   MASTER_PASSWORD='user_password', MASTER_LOG_FILE='mysql-bin.000001', MASTER_LOG_POS=130;
SLAVE START;

```

First we stop the Slave. Then we tell it exactly where to look in the Master log file.
For `MASTER_LOG_FILE` name and `MASTER_LOG_POS`, use the values which we got by running `SHOW MASTER STATUS` command on the Master.

You should change the I.P of the Master in `MASTER_HOST`, and change the user and password accordingly.

The Slave will now be waiting. The status of the Slave can be viewed by run the following

```sql
SHOW SLAVE STATUS;

```

If you previously executed `FLUSH TABLES WITH READ LOCK` in Master, release the tables from lock by run the following

```sql
UNLOCK TABLES;

```

Now the Master keep a log for every action performed on it and the Slave server look at the log on the Master. Whenever changes happens in log on the Master, Slave replicate that.



## Replication Errors


Whenever there is an error while running a query on the slave, MySQL stop replication automatically to identify the problem and fix it. This mainly because an event caused a duplicate key or a row was not found and it cannot be updated or deleted. You can skip such errors, even if this is not recommended

To skip just one query that is hanging the slave, use the following syntax

```sql
SET GLOBAL sql_slave_skip_counter = N;

```

This statement skips the next N events from the master. This statement is valid only when the slave threads are not running. Otherwise, it produces an error.

```sql
STOP SLAVE;
SET GLOBAL sql_slave_skip_counter=1;
START SLAVE;

```

In some cases this is fine. But if the statement is part of a multi-statement transaction, it becomes more complex, because skipping the error producing statement will cause the whole transaction to be skipped.

If you want to skip more queries which producing same error code and if you are sure that skipping those errors will not bring your slave inconsistent and you want to skip them all, you would add a line to skip that error code in your `my.cnf`.

For example you might want to skip all duplicate errors you might be getting

```sql
1062 | Error 'Duplicate entry 'xyz' for key 1' on query

```

Then add the following to your `my.cnf`

```sql
slave-skip-errors = 1062

```

You can skip also other type of errors or all error codes, but make sure that skipping those errors will not bring your slave inconsistent. The following are the syntax and examples

```sql
slave-skip-errors=[err_code1,err_code2,...|all]

slave-skip-errors=1062,1053
slave-skip-errors=all
slave-skip-errors=ddl_exist_errors

```



#### Remarks


Replication is used to copy[Backup] data from one MySQL database server to one or more MySQL database servers.

****Master****  -- The MySQL database server, which is serving data to be copied

****Slave****  -- The MySQL database server, copies data which is served by Master

With MySQL, replication is asynchronous by default. This means slaves do not need to be connected permanently to receive updates from the master. For example, if your slave is switched OFF or not connected with master and you are switching slave ON or connect with Master at a later time, then it will automatically synchronize with the Master.

Depending on the configuration, you can replicate all databases, selected databases, or even selected tables within a database.

**Replication Formats**

There are two core types of replication formats

**Statement Based Replication (SBR)** -- which replicates entire SQL statements. In this, the master writes SQL statements to the binary log. Replication of the master to the slave works by executing that SQL statements on the slave.

**Row Based Replication (RBR)** -- which replicates only the changed rows. In this, the master writes events to the binary log that indicate how individual table rows are changed. Replication of the master to the slave works by copying the events representing the changes to the table rows to the slave.

You can also use a third variety, **Mixed Based Replication (MBR)**. In this, both statement-based and row-based logging is used. Log will be created depending on which is most appropriate for the change.

Statement-based format was the default in MySQL versions older than 5.7.7. In MySQL 5.7.7 and later, row-based format is the default.

