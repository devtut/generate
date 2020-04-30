---
metaTitle: "Customize PS1"
description: "Customize the MySQL PS1 with current database, Custom PS1 via MySQL configuration file"
---

# Customize PS1



## Customize the MySQL PS1 with current database


In the .bashrc or .bash_profile, adding:

```sql
export MYSQL_PS1="\u@\h [\d]>"

```

make the MySQL client PROMPT show current user@host [database].

[<img src="http://i.stack.imgur.com/lHXU6.png" alt="enter image description here" />](http://i.stack.imgur.com/lHXU6.png)



## Custom PS1 via MySQL configuration file


In `mysqld.cnf` or equivalent:

```sql
[mysql]
prompt = '\u@\h [\d]> '

```

This achieves a similar effect, without having to deal with `.bashrc`'s.

