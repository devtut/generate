---
metaTitle: "Linux - Package Managers"
description: "How to update packages with the apt package manager, How to update packages with the pacman package manager, How to install a package with the pacman package manager, How to update packages with yum"
---

# Package Managers



## How to update packages with the apt package manager


The **A**dvanced **P**ackage **T**ool, aptly named the 'apt' package manager can handle the installation and removal of software on the Debian, Slackware, and other Linux Distributions.  Below are some simple examples of use:

**update**<br />
This option retrieves and scans the Packages.gz files, so that information about new and updated packages is available.  To do so, enter the following command:

`sudo apt-get update`

**upgrade**<br />
This option is used to install the newest versions of all packages currently installed on the system.   Packages currently installed with new versions available are retrieved and upgraded; under no circumstances are currently installed packages removed, or packages not already installed retrieved and installed.  To upgrade, enter the following command:

`sudo apt-get upgrade`

**dist-upgrade**<br />
In addition to performing the function of upgrade, dist-upgrade also intelligently handles changing dependencies with new versions of packages.  It will attempt to upgrade the most important packages at the expense of less important ones if necessary.  To do so, enter the following command:

`sudo apt-get dist-upgrade`



## How to update packages with the pacman package manager


To update a specific program:

```

sudo pacman -S <programName>

```

To update entire the system:

```bash
sudo pacman -Syu

```



## How to install a package with the pacman package manager


In order to search for packages in the databse, searching both in packages' names and descriptions:

```

pacman -Ss string1 string2 ...

```

To install a single package or list of packages (including dependencies), issue the following command:

```bash
sudo pacman -S package_name1 package_name2 ...

```

[source](https://wiki.archlinux.org/index.php/Pacman#Querying_package_databases)



## How to update packages with yum


**Y**ellowdog **U**pdater, **M**odified, one of the last remaining vestiges of Yellow Dog Linux, is the package manager used by Red Hat, Fedora, and CentOS systems and their derivatives. It can handle the installation and removal of software packaged as **rpms** for these Linux distributions.  Below are some simple examples of use:

**search**<br />
This command will attempt to locate software packages in the configured software repositories that match the given search criteria, and display the name / version / repository location of the matches it finds.  To use it, enter the following command:

```bash
yum search <queryString>

```

**install**<br />
This command will attempt to locate and install the named software from the configured software repositories, recursively locating and installing any needed prerequisite software as well.  To use it, enter the following command:

```bash
sudo yum install <packageName>

```

**update**<br />
This option is used to install the newest versions of all packages currently installed on the system. Packages currently installed with new versions available are retrieved and upgraded; new prerequisites are also retrieved and installed as necessary, and replaced or obsoleted packages are removed. To upgrade, enter the following command:

```bash
sudo yum update 

```

Unlike **apt**, most **yum** commands will also automatically check for updates to repository metadata if a check has not been done recently (or if forced to do so) and will retrieve and scan updated metadata so that information about new and updated packages is available before the requested operation is performed.

