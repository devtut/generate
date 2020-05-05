---
metaTitle: "PostgreSQL - Getting started with postgresql"
description: "Installing PostgreSQL on Windows, Install PostgreSQL from Source on Linux, Installation on GNU+Linux, How to install PostgreSQL via MacPorts on OSX, Install postgresql with brew on Mac, Postgres.app for Mac OSX"
---

# Getting started with postgresql



## Installing PostgreSQL on Windows


While it's good practice to use a Unix based operating system (ex. Linux or BSD) as a production server you can easily install PostgreSQL on Windows (hopefully only as a development server).

Download the Windows installation binaries from EnterpriseDB: [http://www.enterprisedb.com/products-services-training/pgdownload](http://www.enterprisedb.com/products-services-training/pgdownload) This is a third-party company started by core contributors to the PostgreSQL project who have optimized the binaries for Windows.

Select the latest stable (non-Beta) version (9.5.3 at the time of writing). You will most likely want the Win x86-64 package, but if you are running a 32 bit version of Windows, which is common on older computers, select Win x86-32 instead.

Note: Switching between Beta and Stable versions will involve complex tasks like dump and restore. Upgrading within beta or stable version only needs a service restart.

You can check if your version of Windows is 32 or 64 bit by going to Control Panel -> System and Security -> System -> System type, which will say "##-bit Operating System". This is the path for Windows 7, it may be slightly different on other versions of Windows.

In the installer select the packages you would like to use. For example:

- pgAdmin ( [https://www.pgadmin.org](https://www.pgadmin.org) ) is a free GUI for managing your database and I highly recommend it. In 9.6 this will be installed by default .
- PostGIS ( [http://postgis.net](http://postgis.net) ) provides geospatial analysis features on GPS coordinates, distances etc. very popular among GIS developers.
- The Language Package provides required libraries for officially supported procedural language PL/Python, PL/Perl and PL/Tcl.
- Other packages like pgAgent, pgBouncer and Slony are useful for larger production servers, only checked as needed.

All those optional packages can be later installed through "Application Stack Builder".

Note: There are also other non-officially supported language such as [PL/V8](http://www.postgresonline.com/journal/archives/360-PLV8-binaries-for-PostgreSQL-9.5-windows-both-32-bit-and-64-bit.html), [PL/Lua](https://github.com/pllua/pllua) PL/Java available.

Open pgAdmin and connect to your server by double clicking on its name, ex. "PostgreSQL 9.5 (localhost:5432).

From this point you can follow guides such as the excellent book PostgreSQL: Up and Running, 2nd Edition ( [http://shop.oreilly.com/product/0636920032144.do](http://shop.oreilly.com/product/0636920032144.do) ).

**Optional: Manual Service Startup Type**

PostgreSQL runs as a service in the background which is slightly different than most programs. This is common for databases and web servers. Its default Startup Type is Automatic which means it will always run without any input from you.

Why would you want to manually control the PostgreSQL service? If you're using your PC as a development server some of the time and but also use it to play video games for example, PostegreSQL could slow down your system a bit while its running.

Why wouldn't you want manual control? Starting and stopping the service can be a hassle if you do it often.

If you don't notice any difference in speed and prefer avoiding the hassle then leave its Startup Type as Automatic and ignore the rest of this guide. Otherwise...

Go to Control Panel -> System and Security -> Administrative Tools.

Select "Services" from the list, right click on its icon, and select Send To -> Desktop to create a desktop icon for more convenient access.

Close the Administrative Tools window then launch Services from the desktop icon you just created.

Scroll down until you see a service with a name like postgresql-x##-9.# (ex. "postgresql-x64-9.5").

Right click on the postgres service, select Properties -> Startup type -> Manual -> Apply -> OK. You can change it back to automatic just as easily.

If you see other PostgreSQL related services in the list such "pgbouncer" or "PostgreSQL Scheduling Agent - pgAgent" you can also change their Startup Type to Manual because they're not much use if PostgreSQL isn't running. Although this will mean more hassle each time you start and stop so it's up to you. They don't use as many resources as PostgreSQL itself and may not have any noticeable impact on your systems performance.

If the service is running its Status will say Started, otherwise it isn't running.

To start it right click and select Start. A loading prompt will be displayed and should disappear on its own soon after. If it gives you an error try a second time. If that doesn't work then there was some problem with the installation, possibly because you changed some setting in Windows most people don't change, so finding the problem might require some sleuthing.

To stop postgres right click on the service and select Stop.

If you ever get an error while attempting to connect to your database check Services to make sure its running.

For other very specific details about the EDB PostgreSQL installation, e.g. the python runtime version in the official language pack of a specific PostgreSQL version, always refer to [the official EBD installation guide](https://www.enterprisedb.com/docs/en/9.6/instguide/toc.html) , change the version in link to your installer's major version.



## Install PostgreSQL from Source on Linux


Dependencies:

- GNU Make Version > 3.80
- an ISO/ ANSI C-Compiler (e.g. gcc)
- an extractor like tar or gzip
- zlib-devel
- readline-devel oder libedit-devel

Sources:
[Link to the latest source (9.6.3)](https://ftp.postgresql.org/pub/source/v9.6.3/postgresql-9.6.3.tar.gz)

Now you can extract the source files:

```sql
tar -xzvf postgresql-9.6.3.tar.gz

```

There are a large number of different options for the configuration of PostgreSQL:

[Full Link to the full installation procedure](https://www.postgresql.org/docs/9.6/static/install-procedure.html)

Small list of available options:

- `--prefix=PATH`          path for all files
- `--exec-prefix=PATH`     path for architectur-dependet file
- `--bindir=PATH`            path for executable programs
- `--sysconfdir=PATH`        path for configuration files
- `--with-pgport=NUMBER` specify a port for your server
- `--with-perl` add perl support
- `--with-python` add python support
- `--with-openssl` add openssl support
- `--with-ldap` add ldap support
<li>`--with-blocksize=BLOCKSIZE` set pagesize in KB
<ul>
- `BLOCKSIZE` must a power of 2 and between 1 and 32

- `SEGSIZE` must be a power of 2 between 1 and 64

Go into the new created folder and run the cofigure script with the desired options:

```sql
./configure --exec=/usr/local/pgsql

```

Run `make` to create the objectfiles

Run `make install` to install PostgreSQL from the built files

Run `make clean` to tidy up

For the extension switch the directory `cd contrib`, run `make` and `make install`



## Installation on GNU+Linux


On most GNU+Linux operating systems, PostgreSQL can easily be installed using the operating system package manager.

### Red Hat family

Respositories can be found here: [https://yum.postgresql.org/repopackages.php](https://yum.postgresql.org/repopackages.php)

Download the repository to local machine with the command

```sql
yum -y install https://download.postgresql.org/pub/repos/yum/X.X/redhat/rhel-7-x86_64/pgdg-redhatXX-X.X-X.noarch.rpm

```

View available packages:

```sql
yum list available | grep postgres*

```

Neccesary packages are:
postgresqlXX
postgresqlXX-server
postgresqlXX-libs
postgresqlXX-contrib

These are installed with the following command:
yum -y install postgresqlXX postgresqlXX-server postgresqlXX-libs postgresqlXX-contrib

Once installed you will need to start the database service as the service owner (Default is postgres).
This is done with the pg_ctl command.

```sql
sudo -su postgres
./usr/pgsql-X.X/bin/pg_ctl -D /var/lib/pgsql/X.X/data start 

```

To access the DB in CLI enter `psql`

### Debian family

On [Debian and derived](https://www.postgresql.org/download/linux/ubuntu/) operating systems, type:

```sql
sudo apt-get install postgresql

```

This will install the PostgreSQL server package, at the default version offered by the operating system's package repositories.

If the version that's installed by default is not the one that you want, you can use the package manager to search for specific versions which may simultaneously be offered.

You can also use the Yum repository provided by the PostgreSQL project (known as [PGDG](http://yum.postgresql.org/repopackages.php)) to get a different version. This may allow versions not yet offered by operating system package repositories.



## How to install PostgreSQL via MacPorts on OSX


In order to install PostgreSQL on OSX, you need to know which versions are currently supported.

Use this command to see what versions you have available.

```sql
sudo port list | grep "^postgresql[[:digit:]]\{2\}[[:space:]]"

```

You should get a list that looks something like the following:

```sql
postgresql80                   @8.0.26         databases/postgresql80
postgresql81                   @8.1.23         databases/postgresql81
postgresql82                   @8.2.23         databases/postgresql82
postgresql83                   @8.3.23         databases/postgresql83
postgresql84                   @8.4.22         databases/postgresql84
postgresql90                   @9.0.23         databases/postgresql90
postgresql91                   @9.1.22         databases/postgresql91
postgresql92                   @9.2.17         databases/postgresql92
postgresql93                   @9.3.13         databases/postgresql93
postgresql94                   @9.4.8          databases/postgresql94
postgresql95                   @9.5.3          databases/postgresql95
postgresql96                   @9.6beta2       databases/postgresql96

```

In this example, the most recent version of PostgreSQL that is supported in 9.6, so we will install that.

```sql
sudo port install postgresql96-server postgresql96

```

You will see an installation log like this:

```sql
--->  Computing dependencies for postgresql96-server
--->  Dependencies to be installed: postgresql96
--->  Fetching archive for postgresql96
--->  Attempting to fetch postgresql96-9.6beta2_0.darwin_15.x86_64.tbz2 from         https://packages.macports.org/postgresql96
--->  Attempting to fetch postgresql96-9.6beta2_0.darwin_15.x86_64.tbz2.rmd160 from https://packages.macports.org/postgresql96
--->  Installing postgresql96 @9.6beta2_0
--->  Activating postgresql96 @9.6beta2_0

To use the postgresql server, install the postgresql96-server port

--->  Cleaning postgresql96
--->  Fetching archive for postgresql96-server
--->  Attempting to fetch postgresql96-server-9.6beta2_0.darwin_15.x86_64.tbz2 from https://packages.macports.org/postgresql96-server
--->  Attempting to fetch postgresql96-server-9.6beta2_0.darwin_15.x86_64.tbz2.rmd160 from https://packages.macports.org/postgresql96-server
--->  Installing postgresql96-server @9.6beta2_0
--->  Activating postgresql96-server @9.6beta2_0

To create a database instance, after install do
 sudo mkdir -p /opt/local/var/db/postgresql96/defaultdb
 sudo chown postgres:postgres /opt/local/var/db/postgresql96/defaultdb
 sudo su postgres -c '/opt/local/lib/postgresql96/bin/initdb -D /opt/local/var/db/postgresql96/defaultdb'

--->  Cleaning postgresql96-server
--->  Computing dependencies for postgresql96
--->  Cleaning postgresql96
--->  Updating database of binaries
--->  Scanning binaries for linking errors
--->  No broken files found.

```

The log provides instructions on the rest of the steps for installation, so we do that next.

```sql
sudo mkdir -p /opt/local/var/db/postgresql96/defaultdb
sudo chown postgres:postgres /opt/local/var/db/postgresql96/defaultdb
sudo su postgres -c '/opt/local/lib/postgresql96/bin/initdb -D /opt/local/var/db/postgresql96/defaultdb'

```

Now we start the server:

```sql
sudo port load -w postgresql96-server

```

Verify that we can connect to the server:

```sql
su postgres -c psql

```

You will see a prompt from postgres:

```sql
psql (9.6.1)
Type "help" for help.

postgres=#

```

Here you can type a query to see that the server is running.

```sql
postgres=#SELECT setting FROM pg_settings WHERE name='data_directory';

```

And see the response:

```

               setting
------------------------------------------
/opt/local/var/db/postgresql96/defaultdb
(1 row)
postgres=#

```

Type \q to quit:

```sql
postgres=#\q

```

And you will be back at your shell prompt.

Congratulations!   You now have a running PostgreSQL instance on OS/X.



## Install postgresql with brew on Mac


Homebrew calls itself '**the missing package manager for macOS**'. It can be used to build and install applications and libraries. Once [installed](http://brew.sh), you can use the `brew` command to install PostgreSQL and it's dependencies as follows:

```sql
brew update
brew install postgresql

```

Homebrew generally installs the latest stable version. If you need a different one then `brew search postgresql` will list the versions available. If you need PostgreSQL built with particular options then `brew info postgresql` will list which options are supported. If you require an unsupported build option, you may have to do the build yourself, but can still use Homebrew to install the common dependencies.

**Start the server:**

```sql
brew services start postgresql

```

**Open the PostgreSQL prompt**

```sql
psql

```

If psql complains that there's no corresponding database for your user, run `createdb`.



## Postgres.app for Mac OSX


An extremely simple tool for installing PostgreSQL on a Mac is available by downloading [Postgres.app](http://postgresapp.com/). <br> You can change preferences to have PostgreSQL run in the background or only when the application is running.<br>



#### Remarks


This section provides an overview of what postgresql is, and why a developer might want to use it.

It should also mention any large subjects within postgresql, and link out to the related topics.  Since the Documentation for postgresql is new, you may need to create initial versions of those related topics.

