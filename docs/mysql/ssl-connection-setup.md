---
metaTitle: "MySQL - SSL Connection Setup"
description: "Setup for Debian-based systems, Setup for CentOS7 / RHEL7"
---

# SSL Connection Setup




## Setup for Debian-based systems


(This assumes MySQL has been installed and that `sudo` is being used.)

### Generating a CA and SSL keys

Make sure OpenSSL and libraries are installed:

```sql
apt-get -y install openssl
apt-get -y install libssl-dev

```

Next make and enter a directory for the SSL files:

```sql
mkdir /home/ubuntu/mysqlcerts
cd /home/ubuntu/mysqlcerts

```

To generate keys, create a certificate authority (CA) to sign the keys (self-signed):

```sql
openssl genrsa 2048 > ca-key.pem
openssl req -new -x509 -nodes -days 3600 -key ca-key.pem -out ca.pem

```

The values entered at each prompt won't affect the configuration. Next create a key for the server, and sign using the CA from before:

```sql
openssl req -newkey rsa:2048 -days 3600 -nodes -keyout server-key.pem -out server-req.pem
openssl rsa -in server-key.pem -out server-key.pem

openssl x509 -req -in server-req.pem -days 3600 -CA ca.pem -CAkey ca-key.pem -set_serial 01 -out server-cert.pem

```

Then create a key for a client:

```sql
openssl req -newkey rsa:2048 -days 3600 -nodes -keyout client-key.pem -out client-req.pem
openssl rsa -in client-key.pem -out client-key.pem
openssl x509 -req -in client-req.pem -days 3600 -CA ca.pem -CAkey ca-key.pem -set_serial 01 -out client-cert.pem

```

To make sure everything was set up correctly, verify the keys:

```sql
openssl verify -CAfile ca.pem server-cert.pem client-cert.pem

```

### Adding the keys to MySQL

Open the [MySQL configuration file](http://stackoverflow.com/documentation/mysql/3134/configuration-and-tuning/25294/minimal-innodb-configuration#t=201611070613476171669). For example:

```sql
vim /etc/mysql/mysql.conf.d/mysqld.cnf

```

Under the `[mysqld]` section, add the following options:

```sql
ssl-ca = /home/ubuntu/mysqlcerts/ca.pem
ssl-cert = /home/ubuntu/mysqlcerts/server-cert.pem
ssl-key = /home/ubuntu/mysqlcerts/server-key.pem

```

Restart MySQL. For example:

```sql
service mysql restart

```

### Test the SSL connection

Connect in the same way, passing in the extra options `ssl-ca`, `ssl-cert`, and `ssl-key`, using the generated client key. For example, assuming `cd /home/ubuntu/mysqlcerts`:

```sql
mysql --ssl-ca=ca.pem --ssl-cert=client-cert.pem --ssl-key=client-key.pem -h 127.0.0.1 -u superman -p

```

After logging in, verify the connection is indeed secure:

```sql
superman@127.0.0.1 [None]> SHOW VARIABLES LIKE '%ssl%';
+---------------+-----------------------------------------+
| Variable_name | Value                                   |
+---------------+-----------------------------------------+
| have_openssl  | YES                                     |
| have_ssl      | YES                                     |
| ssl_ca        | /home/ubuntu/mysqlcerts/ca.pem          |
| ssl_capath    |                                         |
| ssl_cert      | /home/ubuntu/mysqlcerts/server-cert.pem |
| ssl_cipher    |                                         |
| ssl_crl       |                                         |
| ssl_crlpath   |                                         |
| ssl_key       | /home/ubuntu/mysqlcerts/server-key.pem  |
+---------------+-----------------------------------------+

```

You could also check:

```sql
superman@127.0.0.1 [None]> STATUS;
...
SSL:                    Cipher in use is DHE-RSA-AES256-SHA
...

```

### Enforcing SSL

This is via `GRANT`, using `REQUIRE SSL`:

```sql
GRANT ALL PRIVILEGES ON *.* TO 'superman'@'127.0.0.1' IDENTIFIED BY 'pass' REQUIRE SSL;
FLUSH PRIVILEGES;

```

Now, `superman` **must** connect via SSL.

If you don't want to manage client keys, use the client key from earlier and automatically use that for all clients. Open [MySQL configuration file](http://stackoverflow.com/documentation/mysql/3134/configuration-and-tuning/25294/minimal-innodb-configuration#t=201611070613476171669), for example:

```sql
vim /etc/mysql/mysql.conf.d/mysqld.cnf

```

Under the `[client]` section, add the following options:

```sql
ssl-ca = /home/ubuntu/mysqlcerts/ca.pem
ssl-cert = /home/ubuntu/mysqlcerts/client-cert.pem
ssl-key = /home/ubuntu/mysqlcerts/client-key.pem

```

Now `superman` only has to type the following to login via SSL:

```sql
mysql -h 127.0.0.1 -u superman -p

```

Connecting from another program, for example in Python, typically only requires an additional parameter to the connect function. A Python example:

```sql
import MySQLdb
ssl = {'cert': '/home/ubuntu/mysqlcerts/client-cert.pem', 'key': '/home/ubuntu/mysqlcerts/client-key.pem'}
conn = MySQLdb.connect(host='127.0.0.1', user='superman', passwd='imsoawesome', ssl=ssl)

```

### References and further reading:

- [https://www.percona.com/blog/2013/06/22/setting-up-mysql-ssl-and-secure-connections/](https://www.percona.com/blog/2013/06/22/setting-up-mysql-ssl-and-secure-connections/)
- [https://lowendbox.com/blog/getting-started-with-mysql-over-ssl/](https://lowendbox.com/blog/getting-started-with-mysql-over-ssl/)
- [http://xmodulo.com/enable-ssl-mysql-server-client.html](http://xmodulo.com/enable-ssl-mysql-server-client.html)
- [https://ubuntuforums.org/showthread.php?t=1121458](https://ubuntuforums.org/showthread.php?t=1121458)



## Setup for CentOS7 / RHEL7


This example assumes two servers:

1. dbserver  (where our database lives)
1. appclient    (where our applications live)

**FWIW, both servers are SELinux enforcing.**

### First, log on to dbserver

Create a temporary directory for creating the certificates.

```sql
mkdir /root/certs/mysql/ && cd /root/certs/mysql/

```

Create the server certificates

```sql
openssl genrsa 2048 > ca-key.pem
openssl req -sha1 -new -x509 -nodes -days 3650 -key ca-key.pem > ca-cert.pem
openssl req -sha1 -newkey rsa:2048 -days 730 -nodes -keyout server-key.pem > server-req.pem
openssl rsa -in server-key.pem -out server-key.pem
openssl x509 -sha1 -req -in server-req.pem -days 730  -CA ca-cert.pem -CAkey ca-key.pem -set_serial 01 > server-cert.pem

```

Move server certificates to /etc/pki/tls/certs/mysql/

Directory path assumes CentOS or RHEL (adjust as needed for other distros):

```sql
mkdir /etc/pki/tls/certs/mysql/

```

Be sure to set permissions on the folder and files.  mysql needs full ownership and access.

```sql
chown -R mysql:mysql /etc/pki/tls/certs/mysql

```

Now configure MySQL/MariaDB

```sql
# vi /etc/my.cnf
# i
[mysqld]
bind-address=*
ssl-ca=/etc/pki/tls/certs/ca-cert.pem
ssl-cert=/etc/pki/tls/certs/server-cert.pem
ssl-key=/etc/pki/tls/certs/server-key.pem
# :wq 

```

Then

```sql
systemctl restart mariadb

```

Don't forget to open your firewall to allow connections from appclient (using IP 1.2.3.4)

```sql
firewall-cmd --zone=drop --permanent --add-rich-rule 'rule family="ipv4" source address="1.2.3.4" service name="mysql" accept'
# I force everything to the drop zone.  Season the above command to taste.

```

Now restart firewalld

```sql
service firewalld restart

```

Next, log in to dbserver's mysql server:

```sql
mysql -uroot -p 

```

Issue the following to create a user for the client.  note REQUIRE SSL in GRANT statement.

```sql
GRANT ALL PRIVILEGES ON *.* TO ‘iamsecure’@’appclient’ IDENTIFIED BY ‘dingdingding’ REQUIRE SSL;
FLUSH PRIVILEGES; 
# quit mysql

```

You should still be in /root/certs/mysql from the first step. If not, cd back to it for one of the commands below.

Create the client certificates

```sql
openssl req -sha1 -newkey rsa:2048 -days 730 -nodes -keyout client-key.pem > client-req.pem
openssl rsa -in client-key.pem -out client-key.pem
openssl x509 -sha1 -req -in client-req.pem -days 730 -CA ca-cert.pem -CAkey ca-key.pem -set_serial 01 > client-cert.pem

```

**Note**: I used the same common name for both server and client certificates. YMMV.

**Be sure you're still /root/certs/mysql/ for this next command**

Combine server and client CA certificate into a single file:

```sql
cat server-cert.pem client-cert.pem > ca.pem

```

Make sure you see two certificates:

```sql
cat ca.pem 

```

### END OF SERVER SIDE WORK FOR NOW.

Open another terminal and

```sql
ssh appclient

```

As before, create a permanent home for the client certificates

```sql
mkdir /etc/pki/tls/certs/mysql/

```

Now, place the client certificates (created on dbserver) on appclient.
You can either scp them over, or just copy and paste the files one by one.

```sql
scp dbserver
# copy files from dbserver to appclient
# exit scp

```

Again, be sure to set permissions on the folder and files.  mysql needs full ownership and access.

```sql
chown -R mysql:mysql /etc/pki/tls/certs/mysql

```

You should have three files, each owned by user mysql:

```sql
/etc/pki/tls/certs/mysql/ca.pem
/etc/pki/tls/certs/mysql/client-cert.pem
/etc/pki/tls/certs/mysql/client-key.pem

```

Now edit appclient's MariaDB/MySQL config in the `[client]` section.

```sql
vi /etc/my.cnf
# i
[client]
ssl-ca=/etc/pki/tls/certs/mysql/ca.pem
ssl-cert=/etc/pki/tls/certs/mysql/client-cert.pem
ssl-key=/etc/pki/tls/certs/mysql/client-key.pem
# :wq 

```

Restart appclient's mariadb service:

```sql
systemctl restart mariadb

```

### still on the client here

This should return: ssl TRUE

```sql
mysql --ssl --help

```

Now, log in to appclient's mysql instance

```sql
mysql -uroot -p

```

Should see YES to both variables below

```sql
show variables LIKE '%ssl';
    have_openssl    YES
    have_ssl              YES

```

Initially I saw

```

have_openssl NO

```

A quick look into mariadb.log revealed:

> 
<p>SSL error: Unable to get certificate from
'/etc/pki/tls/certs/mysql/client-cert.pem'</p>


The problem was that root owned client-cert.pem and the containing folder.
The solution was to set ownership of /etc/pki/tls/certs/mysql/ to mysql.

```sql
chown -R mysql:mysql /etc/pki/tls/certs/mysql

```

Restart mariadb if needed from the step immediately above

### NOW WE ARE READY TO TEST THE SECURE CONNECTION

### We're still on appclient here

Attempt to connect to dbserver's mysql instance using the account created above.

```sql
mysql -h dbserver -u iamsecure -p
# enter password dingdingding (hopefully you changed that to something else)

```

With a little luck you should be logged in without error.

To confirm you are connected with SSL enabled, issue the following command from the MariaDB/MySQL prompt:

```sql
\s 

```

**That's a backslash s, aka status**

That will show the status of your connection, which should look something like this:

```sql
Connection id:        4
Current database:    
Current user:        iamsecure@appclient
SSL:            Cipher in use is DHE-RSA-AES256-GCM-SHA384
Current pager:        stdout
Using outfile:        ''
Using delimiter:    ;
Server:            MariaDB
Server version:        5.X.X-MariaDB MariaDB Server
Protocol version:    10
Connection:        dbserver via TCP/IP
Server characterset:    latin1
Db     characterset:    latin1
Client characterset:    utf8
Conn.  characterset:    utf8
TCP port:        3306
Uptime:            42 min 13 sec

```

If you get permission denied errors on your connection attempt, check your GRANT statement above to make sure there aren't any stray characters or ' marks.

If you have SSL errors, go back through this guide to make sure the steps are orderly.

This worked on RHEL7 and will likely work on CentOS7, too.  Cannot confirm whether these exact steps will work elsewhere.

Hope this saves someone else a little time and aggravation.

