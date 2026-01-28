---
metaTitle: "Linux - LAMP Stack"
description: "Installing LAMP on Arch Linux, Installing LAMP on Ubuntu, Installing LAMP stack on CentoOS"
---

# LAMP Stack


LAMP (**L**inux **A**pache **M**ySQL **P**HP) consists of the Linux operating system as development environment, the Apache HTTP Server as web server, the MySQL relational database management system (RDBMS) as DB(**D**ata **B**ase) system, and the PHP programming language as Server side (Back End) programming language.

LAMP is used as a Open Source stack of technologies solution to web development area. Windows version of this stack is called WAMP(**W**indows **A**pache **M**ySQL **P**HP)



## Installing LAMP on Arch Linux


With this line we will install all the necessary packages in one step, and the last update:

```bash
pacman -Syu apache php php-apache mariadb

```

### **HTTP**

Edit

`/etc/httpd/conf/httpd.conf`

Change `ServerAdmin you@example.com` as you need.

The folder of the WEB Pages by default is `ServerRoot "/etc/httpd"`. Directory must be set to the same folder, so change the line

`<Directory "/etc/httpd">`

This folder must have read and execution access, so

`chmod o+x /etc/httpd`

Change  `AllowOverride from none (default) to All` so .htaccess will works.

Now you need the `~/public_html` folder for each user. (to get the root page of each user as [http://localhost/~yourusername/](http://localhost/%7Eyourusername/). Unremark this line:

`Include conf/extra/httpd-userdir.conf`

Now as root you need to create the `~/public_html` for each user and change the access to (755) of each one.

```bash
chmod 755 /home
chmod 755 /home/username
chmod 755 /home/username/public_html

```

You can comment out this line if you want to use SSL:

`LoadModule ssl_module modules/mod_ssl.so`

If you need to use virtual domains, uncomment the line:

`Include conf/extra/httpd-vhosts.conf`

and in `/etc/httpd/conf/extra/httpd-vhosts.conf` you must to add all the virtual domains. (plus into `/etc/hosts` if you want to test those virtuals domains)

Edit `/etc/httpd/conf/extra/httpd-default.conf` and change **ServerSignature** to Off and **ServerToken** to Prod for hiding critical data

### **PHP**

Edit: `/etc/httpd/conf/httpd.conf`

Comment out: `LoadModule mpm_event_module modules/mod_mpm_event.so`

Uncomment: `LoadModule mpm_prefork_module modules/mod_mpm_prefork.so`

As last item in the LoadModule list, add `LoadModule php7_module modules/libphp7.so`

As last item in the include list, add `Include conf/extra/php7_module.conf`

Edit `/etc/php/php.ini`

Uncomment `extension=mysqli.so` and `extension=pdo_mysql.so`

Change the timezone as you need, for example:

`date.timezone = America/Argentina/Buenos_Aires, date.default_latitude = 0.0, date.default_longitude = 0.0`

### **MySQL**

Run as root:

`mysql_install_db --user=mysql --basedir=/usr --datadir=/var/lib/mysql`

Now you have the root of the MySQL Server.

Start MySQL daemon:

```bash
systemctl enable mysqld
systemctl start mysqld

```

At last, run:

`sh /usr/bin/mysql_secure_installation`

That all to get a web server ready to be customized as you need.



## Installing LAMP on Ubuntu


Install apache:

```bash
sudo apt-get install apache2

```

Install MySql:

```bash
sudo apt-get install mysql-server

```

Install PHP:

```bash
sudo apt-get install php5 libapache2-mod-php5

```

Restart system:

```bash
sudo systemctl restart apache2

```

Check PHP installation:

```

php -r 'echo "\n\nYour PHP installation is working fine.\n\n\n";'

```



## Installing LAMP stack on CentoOS


### Install Apache Web Server

First step is to install web server Apache.

`sudo yum -y install httpd`

Once it is installed, enable (to run on startup) and start Apache web server service.

`sudo systemctl enable --now httpd`

Point your browser to:

[http://localhost](http://localhost)

You will see the default Apache web server page.

### Install MariaDB Server

Second step is to install MariaDB:

`sudo yum -y install mariadb-server`

Then start and enable (on startup) the MariaDB server:

`sudo systemctl enable --now mariadb`

As needed, use **mysql_secure_installation** to secure your database.

This script will allow you to do the following:

- Change the root user's password
- Remove test databases
- Disable remote access

### Install PHP

`sudo yum -y install php php-common`

Then restart Apache's httpd service.

`sudo systemctl restart httpd`

To test PHP, create a file called **index.php** in **/var/www/html**.

Then add the following line to the file:

Then point your browser to:

[http://localhost/index.php](http://localhost/index.php)

You should see information related to your server. If you do not, ensure that php is for sure installed correctly by running the following command:

`php --version`

If you receive something like:

`PHP 5.4.16 (cli) (built: Nov 6 2016 00:29:02) Copyright (c) 1997-2013 The PHP Group`

Then PHP is installed correctly. If this is the case, please ensure that you've restarted your web server.

