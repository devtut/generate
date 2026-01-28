---
metaTitle: "MySQL - Security via GRANTs"
description: "Best Practice, Host (of user@host)"
---

# Security via GRANTs



## Best Practice


Limit root (and any other SUPER-privileged user) to

```sql
GRANT ... TO root@localhost ...

```

That prevents access from other servers. You should hand out SUPER to very few people, and they should be aware of their responsibility. The application should not have SUPER.

Limit application logins to the one database it uses:

```sql
GRANT ... ON dbname.* ...

```

That way, someone who hacks into the application code can't get past dbname. This can be further refined via either of these:

```sql
GRANT SELECT ON dname.* ...    -- "read only"
GRANT ... ON dname.tblname ... -- "just one table"

```

The readonly may also need 'safe' things like

```sql
GRANT SELECT, CREATE TEMPORARY TABLE ON dname.* ...    -- "read only"

```

As you say, there is no absolute security. My point here is there you can do a few things to slow hackers down. (Same goes for honest people goofing.)

In rare cases, you may need the application to do something available only to root. this can be done via a "Stored Procedure" that has `SECURITY DEFINER` (and root defines it). That will expose only what the SP does, which might, for example, be one particular action on one particular table.



## Host (of user@host)


The "host" can be either a host name or an IP address.  Also, it can involve wild cards.

```sql
GRANT SELECT ON db.* TO sam@'my.domain.com' IDENTIFIED BY 'foo';

```

Examples:  Note:  these usually need to be quoted

```sql
localhost -- the same machine as mysqld
'my.domain.com' -- a specific domain; this involves a lookup
'11.22.33.44' -- a specific IP address
'192.168.1.%' -- wild card for trailing part of IP address.  (192.168.% and 10.% and 11.% are "internal" ip addresses.)

```

Using `localhost` relies on the security of the server.  For best practice `root` should only be allowed in through localhost.  In some cases, these mean the same thing:  `0.0.0.1` and `::1`.

