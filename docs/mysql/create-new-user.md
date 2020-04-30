---
metaTitle: "Create New User"
description: "Create a MySQL User, Specify the password, Create new user and grant all priviliges to schema, Renaming user"
---

# Create New User



## Create a MySQL User


For creating new user, We need to follow simple steps as below :

**Step 1:** Login to `MySQL` as `root`

```sql
$ mysql -u root -p

```

**Step 2 :** We will see mysql command prompt

```sql
mysql> CREATE USER 'my_new_user'@'localhost' IDENTIFIED BY 'test_password';

```

Here, We have successfully created new user, But this user won't have any `permissions`, So to assign `permissions` to user use following command :

```sql
mysql> GRANT ALL PRIVILEGES ON my_db.* TO 'my_new_user'@'localhost' identified by 'my_password';

```



## Specify the password


The basic usage is:

```sql
mysql> CREATE USER 'my_new_user'@'localhost' IDENTIFIED BY 'test_password';

```

However for situations where is not advisable to hard-code the password in cleartext  it is also possible to specify directly, using the directive `PASSWORD`, the hashed value as returned by the `PASSWORD()` function:

```sql
mysql> select PASSWORD('test_password'); -- returns *4414E26EDED6D661B5386813EBBA95065DBC4728
mysql> CREATE USER 'my_new_user'@'localhost' IDENTIFIED BY PASSWORD '*4414E26EDED6D661B5386813EBBA95065DBC4728';

```



## Create new user and grant all priviliges to schema


```sql
grant all privileges on schema_name.* to 'new_user_name'@'%' identified by 'newpassword';

```

Attention: This can be used to create new root user



## Renaming user


```sql
rename user 'user'@'%' to 'new_name`@'%';

```

If you create a user by mistake, you can change his name



#### Remarks


To view a List of MySQL Users, we use the following command :

```sql
SELECT User,Host FROM mysql.user;

```

