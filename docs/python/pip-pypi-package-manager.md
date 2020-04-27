---
metaTitle: pip: PyPI Package Manager
description: Install Packages, To list all packages installed using `pip`, Upgrade Packages, Uninstall Packages, Updating all outdated packages on Linux, Updating all outdated packages on Windows, Create a requirements.txt file of all packages on the system, Create a requirements.txt file of packages only in the current virtualenv, Using a certain Python version with pip, Installing packages not yet on pip as wheels
---

# pip: PyPI Package Manager


pip is the most widely-used package manager for the Python Package Index, installed by default with recent versions of Python.



## Install Packages


To install the latest version of a package named `SomePackage`:

```
$ pip install SomePackage

```

To install a specific version of a package:

```
$ pip install SomePackage==1.0.4

```

To specify a minimum version to install for a package:

```
$ pip install SomePackage>=1.0.4

```

If commands shows permission denied error on Linux/Unix then use `sudo` with the commands

### Install from requirements files

```
$ pip install -r requirements.txt

```

Each line of the requirements file indicates something to be installed, and like arguments to pip install, Details on the format of the files are here: [Requirements File Format](https://pip.pypa.io/en/stable/reference/pip_install/#requirements-file-format).

After install the package you can check it using `freeze` command:

```
$ pip freeze

```



## To list all packages installed using `pip`


To list installed packages:

```
$ pip list
# example output
docutils (0.9.1)
Jinja2 (2.6)
Pygments (1.5)
Sphinx (1.1.2)

```

To list outdated packages, and show the latest version available:

```
$ pip list --outdated
# example output
docutils (Current: 0.9.1 Latest: 0.10)
Sphinx (Current: 1.1.2 Latest: 1.1.3)

```



## Upgrade Packages


Running

```
$ pip install --upgrade SomePackage 

```

will upgrade package `SomePackage` and all its dependencies. Also, pip automatically removes older version of the package before upgrade.

To upgrade pip itself, do

```
$ pip install --upgrade pip

```

on Unix or

```
$ python -m pip install --upgrade pip

```

on Windows machines.



## Uninstall Packages


To uninstall a package:

```
$ pip uninstall SomePackage

```



## Updating all outdated packages on Linux


`pip` doesn't current contain a flag to allow a user to update all outdated packages in one shot. However, this can be accomplished by piping commands together in a Linux environment:

```
pip list --outdated --local | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip install -U

```

This command takes all packages in the local virtualenv and checks if they are outdated. From that list, it gets the package name and then pipes that to a `pip install -U` command. At the end of this process, all local packages should be updated.



## Updating all outdated packages on Windows


`pip` doesn't current contain a flag to allow a user to update all outdated packages in one shot. However, this can be accomplished by piping commands together in a Windows environment:

```
for /F "delims= " %i in ('pip list --outdated --local') do pip install -U %i

```

This command takes all packages in the local virtualenv and checks if they are outdated. From that list, it gets the package name and then pipes that to a `pip install -U` command. At the end of this process, all local packages should be updated.



## Create a requirements.txt file of all packages on the system


`pip` assists in creating `requirements.txt` files by providing the [`freeze`](https://pip.pypa.io/en/stable/reference/pip_freeze/) option.

```
pip freeze > requirements.txt

```

This will save a list of all packages and their version installed on the system to a file named `requirements.txt` in the current folder.



## Create a requirements.txt file of packages only in the current virtualenv


`pip` assists in creating `requirements.txt` files by providing the [`freeze`](https://pip.pypa.io/en/stable/reference/pip_freeze/#cmdoption-l) option.

```
pip freeze --local > requirements.txt

```

The [`--local`](https://pip.pypa.io/en/stable/reference/pip_freeze/#cmdoption-l) parameter will only output a list of packages and versions that are installed locally to a virtualenv. Global packages will not be listed.



## Using a certain Python version with pip


If you have both Python 3 and Python 2 installed, you can specify which version of Python you would like pip to use. This is useful when packages only support Python 2 or 3 or when you wish to test with both.

If you want to install packages for Python 2, run either:

```
pip install [package]

```

or:

```
pip2 install [package]

```

If you would like to install packages for Python 3, do:

```
pip3 install [package]

```

You can also invoke installation of a package to a specific python installation with:

```
\path\to\that\python.exe -m pip install some_package # on Windows OR
/usr/bin/python25 -m pip install some_package # on OS-X/Linux

```

On OS-X/Linux/Unix platforms it is important to be aware of the distinction between the system version of python, (which upgrading make render your system inoperable), and the user version(s) of python.  You **may**, **depending on which you are trying to upgrade**, need to prefix these commands with `sudo` and input a password.

Likewise on Windows some python installations, especially those that are a part of another package, can end up installed in system directories - those you will have to upgrade from a command window running in Admin mode - if you find that it looks like you need to do this it is a **very** good idea to check which python installation you are trying to upgrade with a command such as `python -c"import sys;print(sys.path);"` or `py -3.5 -c"import sys;print(sys.path);"` you can also check which pip you are trying to run with `pip --version`

On Windows, if you have both python 2 and python 3 installed, and on your path and your python 3 is greater than 3.4 then you will probably also have the python launcher `py` on your system path. You can then do tricks like:

```
py -3 -m pip install -U some_package # Install/Upgrade some_package to the latest python 3
py -3.3 -m pip install -U some_package # Install/Upgrade some_package to python 3.3 if present
py -2 -m pip install -U some_package # Install/Upgrade some_package to the latest python 2 - 64 bit if present
py -2.7-32 -m pip install -U some_package # Install/Upgrade some_package to python 2.7 - 32 bit if present

```

If you are running & maintaining multiple versions of python I would strongly recommend reading up about the python `virtualenv` or `venv` [virtual enviroments](https://virtualenv.pypa.io/en/stable/) which allow you to isolate both the version of python and which packages are present.



## Installing packages not yet on pip as wheels


Many, pure python, packages are not yet available on the Python Package Index as wheels but still install fine. However, some packages on Windows give the dreaded vcvarsall.bat not found error.

The problem is that the package that you are trying to install contains a C or C++ extension and is not **currently** available as a pre-built wheel from the python package index, **pypi**, and on windows you do not have the tool chain needed to build such items.

The simplest answer is to go to [Christoph Gohlke's](http://www.lfd.uci.edu/%7Egohlke/pythonlibs/) excellent site and locate the **appropriate** version of the libraries that you need. By appropriate in the package name a **-cp******NN******-** has to match your version of python, i.e. if you are using windows 32 bit python **even on win64** the name must include **-win32-** and if using the 64 bit python it must include **-win_amd64-** and then the python version must match, i.e. for Python 34 the filename **must** include **-cp******34-****, etc. this is basically the magic that pip does for you on the pypi site.

Alternatively, you need to get the appropriate windows development kit for the version of python that you are using, the headers for any library that the package you are trying to build interfaces to, possibly the python headers for the version of python, etc.

Python 2.7 used Visual Studio 2008, Python 3.3 and 3.4 used Visual Studio 2010, and Python 3.5+ uses Visual Studio 2015.

- Install “[Visual C++ Compiler Package for Python 2.7](https://www.microsoft.com/en-gb/download/details.aspx?id=44266)”, which is available from Microsoft’s website **or**
- Install “[Windows SDK for Windows 7 and .NET Framework 4](https://www.microsoft.com/en-gb/download/details.aspx?id=8279)” (v7.1), which is available from Microsoft’s website **or**
- Install [Visual Studio 2015 Community Edition](https://www.visualstudio.com/en-us/downloads/download-visual-studio-vs.aspx), **(or any later version, when these are released)**, **ensuring you select the options to install C & C++ support** **no longer the default** -**I am told that this can take up to** ****8 hours**** to download and install so make **sure** that those options are set on the first try.

**Then** you **may need** to locate the header files, **at the matching revision** for any libraries that your desired package links to and download those to an appropriate locations.

**Finally** you can let pip do your build - of course if the package has dependencies that you don't yet have you may also need to find the header files for them as well.

**Alternatives:** It is also worth looking out, **both on pypi or Christop's site**, for any slightly earlier version of the package that you are looking for that is either pure python or pre-built for your platform and python version and possibly using those, if found, until your package does become available.  Likewise if you are using the very latest version of python you may find that it takes the package maintainers a little time to catch up so for projects that really **need** a specific package you may have to use a slightly older python for the moment.  You can also check the packages source site to see if there is a forked version that is available pre-built or as pure python and searching for alternative packages that provide the functionality that you require but are available - one example that springs to mind is the [Pillow](https://pypi.python.org/pypi/Pillow/3.4.2), **actively maintained**, drop in replacement for [PIL](https://pypi.python.org/pypi/PIL/1.1.6) **currently not updated in 6 years and not available for python 3**.

**Afterword**, I would encourage anybody who is having this problem to go to the bug tracker for the package and add to, or raise if there isn't one already, a ticket **politely** requesting that the package maintainers provide a wheel on pypi for your specific combination of platform and python, if this is done then normally things will get better with time, some package maintainers don't realise that they have missed a given combination that people may be using.

### Note on Installing Pre-Releases

Pip follows the rules of [Semantic Versioning](http://semver.org/) and by default prefers released packages over pre-releases. So if a given package has been released as `V0.98` and there is also a release candidate `V1.0-rc1` the default behaviour of `pip install` will be to install `V0.98` - if you wish to install the release candidate, **you are advised to test in a virtual environment first**, you can enable do so with `--pip install --pre` **package-name** or `--pip install --pre --upgrade` **package-name**. In many cases pre-releases or release candidates may not have wheels built for all platform & version combinations so you are more likely to encounter the issues above.

### Note on Installing Development Versions

You can also use pip to install development versions of packages from github and other locations, since such code is in flux it is very unlikely to have wheels built for it, so any impure packages will require the presence of the build tools, and they may be broken at any time so the user is **strongly** encouraged to only install such packages in a virtual environment.

Three options exist for such installations:

1. Download compressed snapshot, most online version control systems have the option to download a compressed snapshot of the code. This can be downloaded manually and then installed with `pip install` **path/to/downloaded/file** note that for most compression formats pip will handle unpacking to a cache area, etc.
1. Let pip handle the download & install for you with: `pip install` **URL/of/package/repository** - you may also need to use the `--trusted-host`, `--client-cert` and/or `--proxy` flags for this to work correctly, especially in a corporate environment. e.g:

```
    > py -3.5-32 -m venv demo-pip
    > demo-pip\Scripts\activate.bat
    > python -m pip install -U pip
    Collecting pip
      Using cached pip-9.0.1-py2.py3-none-any.whl
    Installing collected packages: pip
      Found existing installation: pip 8.1.1
        Uninstalling pip-8.1.1:
          Successfully uninstalled pip-8.1.1
    Successfully installed pip-9.0.1
    > pip install git+https://github.com/sphinx-doc/sphinx/
    Collecting git+https://github.com/sphinx-doc/sphinx/
      Cloning https://github.com/sphinx-doc/sphinx/ to c:\users\steve-~1\appdata\local\temp\pip-04yn9hpp-build
    Collecting six>=1.5 (from Sphinx==1.7.dev20170506)
      Using cached six-1.10.0-py2.py3-none-any.whl
    Collecting Jinja2>=2.3 (from Sphinx==1.7.dev20170506)
      Using cached Jinja2-2.9.6-py2.py3-none-any.whl
    Collecting Pygments>=2.0 (from Sphinx==1.7.dev20170506)
      Using cached Pygments-2.2.0-py2.py3-none-any.whl
    Collecting docutils>=0.11 (from Sphinx==1.7.dev20170506)
      Using cached docutils-0.13.1-py3-none-any.whl
    Collecting snowballstemmer>=1.1 (from Sphinx==1.7.dev20170506)
      Using cached snowballstemmer-1.2.1-py2.py3-none-any.whl
    Collecting babel!=2.0,>=1.3 (from Sphinx==1.7.dev20170506)
      Using cached Babel-2.4.0-py2.py3-none-any.whl
    Collecting alabaster<0.8,>=0.7 (from Sphinx==1.7.dev20170506)
      Using cached alabaster-0.7.10-py2.py3-none-any.whl
    Collecting imagesize (from Sphinx==1.7.dev20170506)
      Using cached imagesize-0.7.1-py2.py3-none-any.whl
    Collecting requests>=2.0.0 (from Sphinx==1.7.dev20170506)
      Using cached requests-2.13.0-py2.py3-none-any.whl
    Collecting typing (from Sphinx==1.7.dev20170506)
      Using cached typing-3.6.1.tar.gz
    Requirement already satisfied: setuptools in f:\toolbuild\temp\demo-pip\lib\site-packages (from Sphinx==1.7.dev20170506)
    Collecting sphinxcontrib-websupport (from Sphinx==1.7.dev20170506)
      Downloading sphinxcontrib_websupport-1.0.0-py2.py3-none-any.whl
    Collecting colorama>=0.3.5 (from Sphinx==1.7.dev20170506)
      Using cached colorama-0.3.9-py2.py3-none-any.whl
    Collecting MarkupSafe>=0.23 (from Jinja2>=2.3->Sphinx==1.7.dev20170506)
      Using cached MarkupSafe-1.0.tar.gz
    Collecting pytz>=0a (from babel!=2.0,>=1.3->Sphinx==1.7.dev20170506)
      Using cached pytz-2017.2-py2.py3-none-any.whl
    Collecting sqlalchemy>=0.9 (from sphinxcontrib-websupport->Sphinx==1.7.dev20170506)
      Downloading SQLAlchemy-1.1.9.tar.gz (5.2MB)
        100% |################################| 5.2MB 220kB/s
    Collecting whoosh>=2.0 (from sphinxcontrib-websupport->Sphinx==1.7.dev20170506)
      Downloading Whoosh-2.7.4-py2.py3-none-any.whl (468kB)
        100% |################################| 471kB 1.1MB/s
    Installing collected packages: six, MarkupSafe, Jinja2, Pygments, docutils, snowballstemmer, pytz, babel, alabaster, imagesize, requests, typing, sqlalchemy, whoosh, sphinxcontrib-websupport, colorama, Sphinx
      Running setup.py install for MarkupSafe ... done
      Running setup.py install for typing ... done
      Running setup.py install for sqlalchemy ... done
      Running setup.py install for Sphinx ... done
    Successfully installed Jinja2-2.9.6 MarkupSafe-1.0 Pygments-2.2.0 Sphinx-1.7.dev20170506 alabaster-0.7.10 babel-2.4.0 colorama-0.3.9 docutils-0.13.1 imagesize-0.7.1 pytz-2017.2 requests-2.13.0 six-1.10.0 snowballstemmer-1.2.1 sphinxcontrib-websupport-1.0.0 sqlalchemy-1.1.9 typing-3.6.1 whoosh-2.7.4

```

**Note the `git+` prefix to the URL.**

1. Clone the repository using `git`, `mercurial` or other acceptable tool, **preferably a DVCS tool,** and use `pip install` **path/to/cloned/repo** - this will **both** process any requires.text file and perform the build and setup steps, **you can manually change directory to your cloned repository and run `pip install -r requires.txt` and then `python setup.py install` to get the same effect.**  The big advantages of this approach is that while the initial clone operation may take longer than the snapshot download you can update to the latest with, in the case of git: `git pull origin master` and if the current version contains errors you can use `pip uninstall` **package-name** then use `git checkout` commands to move back through the repository history to earlier version(s) and re-try.



#### Syntax


<li>pip <command> [options] where <command> is one of:
<ul>
<li>install
<ul>
- Install packages

- Uninstall packages

- Output installed packages in requirements format

- List installed packages

- Show information about installed packages

- Search PyPI for packages

- Build wheels from your requirements

- Zip individual packages (deprecated)

- Unzip individual packages (deprecated)

- Create pybundles (deprecated)

- Show help for commands



#### Remarks


Sometimes, `pip` will perfom a manual compilation of native code. On Linux python will automatically choose an available C compiler on your system. Refer to the table below for the required Visual Studio/Visual C++ version on Windows (newer versions will not work.).

|Python Version|Visual Studio Version|Visual C++ Version
|------
|2.6 - 3.2|Visual Studio 2008|[Visual C++ 9.0](https://www.microsoft.com/en-us/download/details.aspx?id=44266)
|3.3 - 3.4|Visual Studio 2010|Visual C++ 10.0
|3.5|Visual Studio 2015|Visual C++ 14.0

[Source: wiki.python.org](https://wiki.python.org/moin/WindowsCompilers)

