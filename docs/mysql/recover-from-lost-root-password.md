---
metaTitle: "MySQL - Recover from lost root password"
description: "Set root password, enable root user for socket and http access"
---

# Recover from lost root password




## Set root password, enable root user for socket and http access


Solves problem of: access denied for user root using password YES
Stop mySQL:

```sql
sudo systemctl stop mysql

```

Restart mySQL, skipping grant tables:

```sql
sudo mysqld_safe --skip-grant-tables

```

Login:

```sql
mysql -u root

```

In SQL shell, look if users exist:

```sql
select User, password,plugin FROM mysql.user ;

```

Update the users (plugin null enables for all plugins):

```sql
update mysql.user set password=PASSWORD('mypassword'), plugin = NULL WHERE User = 'root';
exit;

```

In Unix shell stop mySQL without grant tables, then restart with grant tables:

```sql
sudo service mysql stop
sudo service mysql start

```

