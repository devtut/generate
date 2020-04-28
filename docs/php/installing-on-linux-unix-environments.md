---
metaTitle: "Installing on Linux/Unix Environments"
description: "Command Line Install Using APT for PHP 7, Installing in Enterprise Linux distributions (CentOS, Scientific Linux, etc)"
---

# Installing on Linux/Unix Environments



## Command Line Install Using APT for PHP 7


> 
<p>This will only install PHP. If you wish to serve a PHP file to the web you will
also need to install a web-server such as [Apache](http://www.apache.org/), [Nginx](https://www.nginx.com/), or use [PHP's built in web-server](http://php.net/manual/en/features.commandline.webserver.php) (**php version 5.4+**).</p>


> 
<p>If you are in a Ubuntu version below 16.04 and want to use PHP 7 anyway, you can add [Ondrej's PPA repository](https://launchpad.net/%7Eondrej/+archive/ubuntu/php/) by doing:
`sudo add-apt-repository ppa:ondrej/php`</p>


Make sure that all of your [repositories](https://en.wikipedia.org/wiki/Software_repository) are up to date:

```php
sudo apt-get update

```

After updating your system's repositories, install PHP:

```php
sudo apt-get install php7.0

```

Let's test the installation by checking the PHP version:

```php
php --version

```

This should output something like this.

**Note: Your output will be slightly different.**

```php
PHP 7.0.8-0ubuntu0.16.04.1 (cli) ( NTS )
Copyright (c) 1997-2016 The PHP Group
Zend Engine v3.0.0, Copyright (c) 1998-2016 Zend Technologies
with Zend OPcache v7.0.8-0ubuntu0.16.04.1, Copyright (c) 1999-2016, by Zend Technologies
with Xdebug v2.4.0, Copyright (c) 2002-2016, by Derick Rethans

```

You now have the capability to run PHP from the command line.



## Installing in Enterprise Linux distributions (CentOS, Scientific Linux, etc)


Use the `yum` command to manage packages in Enterprise Linux-based operating systems:

```php
yum install php

```

This installs a minimal install of PHP including some common features. If you need additional modules, you will need to install them separately. Once again, you can use `yum` to search for these packages:

```php
yum search php-*

```

Example output:

```php
php-bcmath.x86_64 : A module for PHP applications for using the bcmath library
php-cli.x86_64 : Command-line interface for PHP
php-common.x86_64 : Common files for PHP
php-dba.x86_64 : A database abstraction layer module for PHP applications
php-devel.x86_64 : Files needed for building PHP extensions
php-embedded.x86_64 : PHP library for embedding in applications
php-enchant.x86_64 : Human Language and Character Encoding Support
php-gd.x86_64 : A module for PHP applications for using the gd graphics library
php-imap.x86_64 : A module for PHP applications that use IMAP

```

To install the gd library:

```php
yum install php-gd

```

Enterprise Linux distributions have always been conservative with updates, and typically do not update beyond the point release they shipped with. A number of third party repositories provide current versions of PHP:

- [IUS](https://ius.io/)
- [Remi Colette](http://www.remirepo.net)
- [Webtatic](https://webtatic.com/)

IUS and Webtatic provide replacement packages with different names (e.g. `php56u` or `php56w` to install PHP 5.6) while Remi's repository provides in-place upgrades by using the same names as the system packages.

Following are instructions on installing PHP 7.0 from Remi's repository. This is the simplest example, as uninstalling the system packages is not required.

```php
# download the RPMs; replace 6 with 7 in case of EL 7
wget https://dl.fedoraproject.org/pub/epel/epel-release-latest-6.noarch.rpm
wget http://rpms.remirepo.net/enterprise/remi-release-6.rpm
# install the repository information
rpm -Uvh remi-release-6.rpm epel-release-latest-6.noarch.rpm
# enable the repository
yum-config-manager --enable epel --enable remi --enable remi-safe --enable remi-php70
# install the new version of PHP
# NOTE: if you already have the system package installed, this will update it
yum install php

```

