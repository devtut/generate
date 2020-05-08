---
metaTitle: "Android - Create Android Custom ROMs"
description: "Making Your Machine Ready for Building!"
---

# Create Android Custom ROMs




## Making Your Machine Ready for Building!


Before you can build anything, you are required to make your machine ready for building. For this you need to install a lot of libraries and modules. The most recommended Linux distribution is Ubuntu, so this example will focus on installing everything that is needed on Ubuntu.

### Installing Java

First, add the following Personal Package Archive (PPA): `sudo apt-add-repository ppa:openjdk-r/ppa`.

Then, update the sources by executing: `sudo apt-get update`.

### Installing Additional Dependencies

All required additional dependencies can be installed by the following command:

```java
sudo apt-get install git-core python gnupg flex bison gperf libsdl1.2-dev libesd0-dev libwxgtk2.8-dev squashfs-tools build-essential zip curl libncurses5-dev zlib1g-dev openjdk-8-jre openjdk-8-jdk pngcrush schedtool libxml2 libxml2-utils xsltproc lzop libc6-dev schedtool g++-multilib lib32z1-dev lib32ncurses5-dev gcc-multilib liblz4-* pngquant ncurses-dev texinfo gcc gperf patch libtool automake g++ gawk subversion expat libexpat1-dev python-all-dev binutils-static bc libcloog-isl-dev libcap-dev autoconf libgmp-dev build-essential gcc-multilib g++-multilib pkg-config libmpc-dev libmpfr-dev lzma* liblzma* w3m android-tools-adb maven ncftp figlet

```

### Preparing the system for development

Now that all the dependencies are installed, let us prepare the system for development by executing:

```java
sudo curl --create-dirs -L -o /etc/udev/rules.d/51-android.rules -O -L https://raw.githubusercontent.com/snowdream/51-android/master/51-android.rules
sudo chmod 644   /etc/udev/rules.d/51-android.rules
sudo chown root /etc/udev/rules.d/51-android.rules
sudo service udev restart
adb kill-server
sudo killall adb

```

Finally, let us set up the cache and the repo by the following commands:

```java
sudo install utils/repo /usr/bin/
sudo install utils/ccache /usr/bin/

```

**Please note:** We can also achieve this setup by running the automated scripts made by Akhil Narang (**akhilnarang**), one of the maintainers of [Resurrection Remix OS](http://www.resurrectionremix.com/). These scripts can be found [on GitHub](https://github.com/akhilnarang/scripts).

