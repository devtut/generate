---
metaTitle: "MySQL - Change Password"
description: "Change MySQL root password in Linux, Change MySQL root password in Windows, Process"
---

# Change Password



## Change MySQL root password in Linux


To change MySQL's root user password:

**Step 1:** Stop the MySQL server.

<li>in Ubuntu or Debian:<br />
`sudo /etc/init.d/mysql stop`</li>
<li>in CentOS, Fedora or Red Hat Enterprise Linux:<br />
`sudo /etc/init.d/mysqld stop`</li>

**Step 2:**  Start the MySQL server without the privilege system.

```sql
sudo mysqld_safe --skip-grant-tables &

```

or, if `mysqld_safe` is unavailable,

```sql
sudo mysqld --skip-grant-tables &

```

**Step 3:** Connect to the MySQL server.

```sql
mysql -u root

```

**Step 4:** Set a new password for root user.

```sql
FLUSH PRIVILEGES;
ALTER USER 'root'@'localhost' IDENTIFIED BY 'new_password';
FLUSH PRIVILEGES;
exit;

```

```sql
FLUSH PRIVILEGES;
SET PASSWORD FOR 'root'@'localhost' = PASSWORD('new_password');
FLUSH PRIVILEGES;
exit;

```

Note: The `ALTER USER` syntax was introduced in MySQL 5.7.6.

**Step 5:** Restart the MySQL server.

<li>in Ubuntu or Debian:<br />
`sudo /etc/init.d/mysql stop`<br />
`sudo /etc/init.d/mysql start`</li>
<li>in CentOS, Fedora or Red Hat Enterprise Linux:<br />
`sudo /etc/init.d/mysqld stop`<br />
`sudo /etc/init.d/mysqld start`</li>



## Change MySQL root password in Windows


When we want to change root password in windows, We need to follow following steps :

**Step 1 :** Start your Command Prompt by using any of below method :

Perss `Crtl+R` or Goto `Start Menu > Run` and then type `cmd` and hit enter

**Step 2 :**
Change your directory to where `MYSQL` is installed, In my case it's

```sql
C:\> cd C:\mysql\bin

```

**Step 3 :** Now we need to start `mysql` command prompt

```sql
C:\mysql\bin> mysql -u root mysql

```

**Step 4 :** Fire query to change `root` password

```sql
mysql> SET PASSWORD FOR root@localhost=PASSWORD('my_new_password');

```



## Process


1. Stop the MySQL (mysqld) server/daemon process.
1. Start the MySQL server process the --skip-grant-tables option so that it will not prompt for a password: `mysqld_safe --skip-grant-tables &`
1. Connect to the MySQL server as the root user: `mysql -u root`
1. Change password:

- (5.7.6 and newer):  `ALTER USER 'root'@'localhost' IDENTIFIED BY 'new-password';`
- (5.7.5 and older, or MariaDB): `SET PASSWORD FOR 'root'@'localhost' = PASSWORD('new-password); flush privileges; quit;`

1. Restart the MySQL server.

Note: this will work only if you are physically on the same server.

Online Doc:  [http://dev.mysql.com/doc/refman/5.7/en/resetting-permissions.html](http://dev.mysql.com/doc/refman/5.7/en/resetting-permissions.html)

