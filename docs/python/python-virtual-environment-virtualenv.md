# Python Virtual Environment - virtualenv


A Virtual Environment (=virtualenv=) is a tool to create isolated Python environments. It keeps the dependencies required by different projects in separate places, by creating virtual Python env for them. It solves the “project A depends on version 2.xxx but, project B needs 2.xxx” dilemma, and keeps your global site-packages directory clean and manageable.

=virtualenv= creates a folder which contains all the necessary libs and bins to use the packages that a Python project would need.



## Installation


Install virtualenv via pip / (apt-get):

```
pip install virtualenv

```

OR

```
apt-get install python-virtualenv

```

Note: In case you are getting permission issues, use sudo.



## Usage


```
$ cd test_proj

```

Create virtual environment:

```
$ virtualenv test_proj

```

To begin using the virtual environment, it needs to be activated:

```
$ source test_project/bin/activate

```

To exit your virtualenv just type “deactivate”:

```
$ deactivate

```



## Install a package in your Virtualenv


If you look at the bin directory in your virtualenv, you’ll see easy_install which
has been modified to put eggs and packages in the virtualenv’s site-packages
directory. To install an app in your virtual environment:

```
$ source test_project/bin/activate
$ pip install flask

```

At this time, you don't have to use sudo since the files will all be installed in the local virtualenv site-packages directory. This was created as your own user account.



## Other useful virtualenv commands


**lsvirtualenv** : List all of the environments.

**cdvirtualenv** : Navigate into the directory of the currently activated virtual environment, so you can browse its site-packages, for example.

**cdsitepackages** : Like the above, but directly into site-packages directory.

**lssitepackages** : Shows contents of site-packages directory.

