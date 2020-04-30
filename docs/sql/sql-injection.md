---
metaTitle: "SQL Injection"
description: "SQL injection sample, simple injection sample"
---

# SQL Injection


SQL injection is an attempt to access a website's database tables by injecting SQL into a form field. If a web server does not protect against SQL injection attacks, a hacker can trick the database into running the additional SQL code. By executing their own SQL code, hackers can upgrade their account access, view someone else's private information, or make any other modifications to the database.



## SQL injection sample


Assuming the call to your web application's login handler looks like this:

```sql
https://somepage.com/ajax/login.ashx?username=admin&password=123

```

Now in login.ashx, you read these values:

```sql
strUserName = getHttpsRequestParameterString("username");
strPassword = getHttpsRequestParameterString("password");

```

and query your database to determine whether a user with that password exists.

So you construct an SQL query string:

```sql
txtSQL = "SELECT * FROM Users WHERE username = '" + strUserName + "' AND password = '"+ strPassword +"'";

```

This will work if the username and password do not contain a quote.

However, if one of the parameters does contain a quote, the SQL that gets sent to the database will look like this:

```sql
-- strUserName = "d'Alambert";
txtSQL = "SELECT * FROM Users WHERE username = 'd'Alambert' AND password = '123'";

```

This will result in a syntax error, because the quote after the `d` in `d'Alambert` ends the SQL string.

You could correct this by escaping quotes in username and password, e.g.:

```sql
strUserName = strUserName.Replace("'", "''");
strPassword = strPassword.Replace("'", "''");

```

However, it's more appropriate to use parameters:

```sql
cmd.CommandText = "SELECT * FROM Users WHERE username = @username AND password = @password";

cmd.Parameters.Add("@username", strUserName);
cmd.Parameters.Add("@password", strPassword);

```

If you do not use parameters, and forget to replace quote in even one of the values, then a malicious user (aka hacker) can use this to execute SQL commands on your database.

For example, if an attacker is evil, he/she will set the password to

```sql
lol'; DROP DATABASE master; -- 

```

and then the SQL will look like this:

```sql
"SELECT * FROM Users WHERE username = 'somebody' AND password = 'lol'; DROP DATABASE master; --'";

```

Unfortunately for you, this is valid SQL, and the DB will execute this!

This type of exploit is called an SQL injection.

There are many other things a malicious user could do, such as stealing every user's email address, steal everyone's password, steal credit card numbers, steal any amount of data in your database, etc.

This is why you always need to escape your strings.<br />
And the fact that you'll invariably forget to do so sooner or later is exactly why you should use parameters. Because if you use parameters, then your programming language framework will do any necessary escaping for you.



## simple injection sample


If the SQL statement is constructed like this:

```sql
SQL = "SELECT * FROM Users WHERE username = '" + user + "' AND password ='" + pw + "'";
db.execute(SQL);

```

Then a hacker could retrieve your data by giving a password like `pw' or '1'='1`; the resulting SQL statement will be:

```sql
SELECT * FROM Users WHERE username = 'somebody' AND password ='pw' or '1'='1'

```

This one will pass the password check for all rows in the `Users` table because `'1'='1'` is always true.

To prevent this, use SQL parameters:

```sql
SQL = "SELECT * FROM Users WHERE username = ? AND password = ?";
db.execute(SQL, [user, pw]);

```

