---
metaTitle: "Recover and reset the default root password for MySQL 5.7+"
description: "What happens when the initial start up of the server, How to change the root password by using the default password, reset root password when  /var/run/mysqld' for UNIX socket file don't exists"
---

# Recover and reset the default root password for MySQL 5.7+


After MySQL 5.7, when we install MySQL sometimes we don't need to create a root account or give a root password. By default when we start the server, the default password is stored in the `mysqld.log` file. We need to login in to the system using that password and we need to change it.



## What happens when the initial start up of the server


Given that the data directory of the server is empty:

- The server is initialized.
- SSL certificate and key files are generated in the data directory.
- The validate_password plugin is installed and enabled.
- The superuser account 'root'@'localhost' is created. The password for the superuser is set and stored in the error log file.



## How to change the root password by using the default password


To reveal the default "root" password:

```sql
shell> sudo grep 'temporary password' /var/log/mysqld.log

```

Change the root password as soon as possible by logging in with the generated temporary password and set a custom password for the superuser account:

```sql
shell> mysql -uroot -p 

mysql> ALTER USER 'root'@'localhost' IDENTIFIED BY 'MyNewPass5!'; 

```

**Note:** MySQL's validate_password plugin is installed by default. This will require that passwords contain at least one upper case letter, one lower case letter, one digit, and one special character, and that the total password length is at least 8 characters.



## reset root password when " /var/run/mysqld' for UNIX socket file don't exists"


if I forget the password then I'll get error.

```sql
$ mysql -u root -p    

```

Enter password:

ERROR 1045 (28000): Access denied for user 'root'@'localhost' (using password: YES)

I tried to solve the issue by first knowing the status:

```sql
$ systemctl status mysql.service

```

mysql.service - MySQL Community Server
Loaded: loaded (/lib/systemd/system/mysql.service; enabled; vendor preset: en
Active: active (running) since Thu 2017-06-08 14:31:33 IST; 38s ago

Then I used the code `mysqld_safe --skip-grant-tables &` but I get the error:

> 
mysqld_safe Directory '/var/run/mysqld' for UNIX socket file don't exists.


```sql
$ systemctl stop  mysql.service
$ ps -eaf|grep mysql
$ mysqld_safe --skip-grant-tables &

```

> 
I solved:


```sql
$ mkdir -p /var/run/mysqld
$ chown mysql:mysql /var/run/mysqld

```

Now I use the same code `mysqld_safe --skip-grant-tables &` and get

> 
mysqld_safe Starting mysqld daemon with databases from /var/lib/mysql


If I use `$ mysql -u root` I'll get :

Server version: 5.7.18-0ubuntu0.16.04.1 (Ubuntu)

Copyright (c) 2000, 2017, Oracle and/or its affiliates. All rights reserved.

Oracle is a registered trademark of Oracle Corporation and/or its
affiliates. Other names may be trademarks of their respective
owners.

Type 'help;' or '\h' for help. Type '\c' to clear the current input statement.

mysql>

> 
Now time to change password:


```sql
mysql> use mysql
mysql> describe user;

```

Reading table information for completion of table and column names
You can turn off this feature to get a quicker startup with -A

Database changed

```sql
mysql> FLUSH PRIVILEGES;
mysql> SET PASSWORD FOR root@'localhost' = PASSWORD('newpwd');

```

or If you have a mysql root account that can connect from everywhere, you should also do:

```sql
UPDATE mysql.user SET Password=PASSWORD('newpwd') WHERE User='root';

```

Alternate Method:

```

  USE mysql
   UPDATE user SET Password = PASSWORD('newpwd')
   WHERE Host = 'localhost' AND User = 'root';

```

And if you have a root account that can access from everywhere:

```

USE mysql
 UPDATE user SET Password = PASSWORD('newpwd')
 WHERE Host = '%' AND User = 'root';`enter code here

```

now need to `quit` from mysql and stop/start

```sql
FLUSH PRIVILEGES;
sudo /etc/init.d/mysql stop
sudo /etc/init.d/mysql start

```

now again ` mysql -u root -p' and use the new password to get

> 
mysql>




#### Remarks


Recovering and resetting the default root password using this method is applicable only for MySQL 5.7+

