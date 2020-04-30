---
metaTitle: "Log files"
description: "Slow Query Log, A List, General Query Log, Error Log"
---

# Log files



## Slow Query Log


The Slow Query Log consists of log events for queries taking up to `long_query_time` seconds to finish. For instance, up to 10 seconds to complete. To see the time threshold currently set, issue the following:

```sql
SELECT @@long_query_time;
+-------------------+
| @@long_query_time |
+-------------------+
|         10.000000 |
+-------------------+

```

It can be set as a GLOBAL variable, in `my.cnf` or `my.ini` file.  Or it can be set by the connection, though this is unusual. The value can be set between 0 to 10 (seconds).  What value to use?

- 10 is so high as to be almost useless;
- 2 is a compromise;
- 0.5 and other fractions are possible;
- 0 captures everything; this could fill up disk dangerously fast, but can be very useful.

The capturing of slow queries is either turned on or off. And the file logged to is also specified. The below captures these concepts:

```sql
SELECT @@slow_query_log; -- Is capture currently active? (1=On, 0=Off)
SELECT @@slow_query_log_file; -- filename for capture. Resides in datadir
SELECT @@datadir; -- to see current value of the location for capture file

SET GLOBAL slow_query_log=0; -- Turn Off
-- make a backup of the Slow Query Log capture file. Then delete it.
SET GLOBAL slow_query_log=1; -- Turn it back On (new empty file is created)

```

For more information, please see the MySQL Manual Page [The Slow Query Log](http://dev.mysql.com/doc/refman/5.7/en/slow-query-log.html)

Note:  The above information on turning on/off the slowlog was changed in 5.6(?); older version had another mechanism.

The "best" way to see what is slowing down your system:

```sql
long_query_time=...
turn on the slowlog
run for a few hours
turn off the slowlog (or raise the cutoff)
run pt-query-digest to find the 'worst' couple of queries.  Or mysqldumpslow -s t

```



## A List


- General log - all queries - see VARIABLE general_log
- Slow log - queries slower than long_query_time - slow_query_log_file
- Binlog - for replication and backup - log_bin_basename
- Relay log - also for replication
- general errors - mysqld.err
- start/stop - mysql.log (not very interesting) - log_error
- InnoDB redo log - iblog*

See the variables `basedir` and `datadir` for default location for many logs

Some logs are turned on/off by other VARIABLES. Some are either written to a file or to a table.

(Note to reviewers:  This needs more details and more explanation.)

**Documenters**: please include the default location and name for each log type, for both Windows and *nix.  (Or at least as much as you can.)



## General Query Log


The General Query Log contains a listing of general information from client connects, disconnects, and queries. It is invaluable for debugging, yet it poses as a hindrance to performance (citation?).

An example view of a General Query Log is seen below:

[<img src="http://i.stack.imgur.com/3dWFH.jpg" alt="enter image description here" />](http://i.stack.imgur.com/3dWFH.jpg)

To determine if the General Log is currently being captured:

```sql
SELECT @@general_log; -- 1 = Capture is active; 0 = It is not.

```

To determine the filename of the capture file:

```sql
SELECT @@general_log_file; -- Full path to capture file

```

If the fullpath to the file is not shown, the file exists in the `datadir`.

Windows example:

```sql
+----------------------------------------------------------+
| @@general_log_file                                       |
+----------------------------------------------------------+
| C:\ProgramData\MySQL\MySQL Server 5.7\Data\GuySmiley.log |
+----------------------------------------------------------+

```

Linux:

```sql
+-----------------------------------+
| @@general_log_file                |
+-----------------------------------+
| /var/lib/mysql/ip-ww-xx-yy-zz.log |
+-----------------------------------+

```

When changes are made to the `general_log_file` GLOBAL variable, the new log is saved in the `datadir`. However, the fullpath may no longer be reflected by examining the variable.

In the case of no entry for `general_log_file` in the configuration file, it will default to `@@hostname`.log in the `datadir`.

Best practices are to turn OFF capture. Save the log file to a backup directory with a filename reflecting the begin/end datetime of the capture. Deleting the prior file if a filesystem **move** did not occur of that file. Establish a new filename for the log file and turn capture ON (all show below). Best practices also include a careful determination if you even want to capture at the moment. Typically, capture is ON for debugging purposes only.

A typical filesystem filename for a backed-up log might be:

```sql
/LogBackup/GeneralLog_20160802_1520_to_20160802_1815.log

```

where the date and time are part to the filename as a range.

For Windows note the following sequence with setting changes.

```sql
SELECT @@general_log; -- 0. Not being captured
SELECT @@general_log_file; -- C:\ProgramData\MySQL\MySQL Server 5.6\Data\GuySmiley.log
SELECT @@datadir; -- C:\ProgramData\MySQL\MySQL Server 5.7\Data\
SET GLOBAL general_log_file='GeneralLogBegin_20160803_1420.log'; -- datetime clue
SET GLOBAL general_log=1; -- Turns on actual log capture. File is created under `datadir`
SET GLOBAL general_log=0; -- Turn logging off

```

Linux is similar. These would represent dynamic changes. Any restart of the server would pick up configuration file settings.

As for the configuration file, consider the following relevant variable settings:

```sql
[mysqld]
general_log_file = /path/to/currentquery.log
general_log      = 1

```

In addition, the variable `log_output` can be configured for `TABLE` output, not just `FILE`. For that, please see [Destinations](http://dev.mysql.com/doc/refman/5.7/en/log-destinations.html).

Please see the MySQL Manual Page [The General Query Log](http://dev.mysql.com/doc/refman/5.7/en/query-log.html).



## Error Log


The Error Log is populated with start and stop information, and critical events encountered by the server.

The following is an example of its contents:

[<img src="http://i.stack.imgur.com/upW0z.jpg" alt="enter image description here" />](http://i.stack.imgur.com/upW0z.jpg)

The variable `log_error` holds the path to the log file for error logging.

In the absence of a configuration file entry for `log_error`, the system will default its values to `@@hostname`.err in the `datadir`. Note that `log_error` is not a dynamic variable. As such, changes are done through a cnf or ini file changes and a server restart (or by seeing "Flushing and Renaming the Error Log File" in the Manual Page link at the bottom here).

Logging cannot be disabled for errors. They are important for system health while troubleshooting problems. Also, entries are infrequent compared to the General Query Log.

The GLOBAL variable `log_warnings` sets the level for verbosity which varies by server version. The following snippet illustrates:

```sql
SELECT @@log_warnings; -- make a note of your prior setting
SET GLOBAL log_warnings=2; -- setting above 1 increases output (see server version)

```

`log_warnings` as seen above is a dynamic variable.

Configuration file changes in `cnf` and `ini` files might look like the following.

```sql
[mysqld]
log_error        = /path/to/CurrentError.log
log_warnings     = 2

```

MySQL 5.7.2 expanded the warning level verbosity to 3 and added the GLOBAL `log_error_verbosity`. Again, it was [introduced](http://dev.mysql.com/doc/refman/5.7/en/server-system-variables.html#sysvar_log_error_verbosity) in 5.7.2. It can be set dynamically and checked as a variable or set via `cnf` or `ini` configuration file settings.

As of MySQL 5.7.2:

```sql
[mysqld]
log_error           = /path/to/CurrentError.log
log_warnings        = 2
log_error_verbosity = 3 

```

Please see the MySQL Manual Page entitled [The Error Log](http://i.stack.imgur.com/upW0z.jpg) especially for Flushing and Renaming the Error Log file, and its **Error Log Verbosity** section with versions related to `log_warnings` and `error_log_verbosity`.

