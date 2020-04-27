---
metaTitle: Usage of "pip" module: PyPI Package Manager
description: Example use of commands, Handling ImportError Exception, Force install
---

# Usage of "pip" module: PyPI Package Manager


Sometimes you may need to use pip package manager inside python eg. when some imports may raise `ImportError` and you want to handle the exception. If you unpack on Windows `Python_root/Scripts/pip.exe`inside is stored `__main__.py` file, where `main` class from `pip` package is imported. This means pip package is used whenever you use pip executable. For usage of pip as executable see: [pip: PyPI Package Manager](https://stackoverflow.com/documentation/python/1781/pip-pypi-package-manage)



## Example use of commands


```
import pip

command = 'install'
parameter = 'selenium'
second_param = 'numpy' # You can give as many package names as needed
switch = '--upgrade'

pip.main([command, parameter, second_param, switch])

```

Only needed parameters are obligatory, so both `pip.main(['freeze'])` and `pip.main(['freeze', '', ''])` are aceptable.

**Batch install**

It is possible to pass many package names in one call, but if one install/upgrade fails, whole installation process stops and ends with status '1'.

```
import pip

installed = pip.get_installed_distributions()
list = []
for i in installed:
    list.append(i.key)

pip.main(['install']+list+['--upgrade'])

```

If you don't want to stop when some installs fail, call installation in loop.

```
for i in installed:
        pip.main(['install']+i.key+['--upgrade'])

```



## Handling ImportError Exception


When you use python file as module there is no need always check if package is installed but it is still useful for scripts.

```
if __name__ == '__main__':
    try:
        import requests
    except ImportError:
        print("To use this module you need 'requests' module")
        t = input('Install requests? y/n: ')
        if t == 'y':
            import pip
            pip.main(['install', 'requests'])
            import requests
            import os
            import sys
            pass
        else:
            import os
            import sys
            print('Some functionality can be unavailable.')
else:
    import requests
    import os
    import sys

```



## Force install


Many packages for example on version 3.4 would run on 3.6 just fine, but if there are no distributions for specific platform, they can't be installed, but there is workaround. In .whl files (known as wheels) naming convention decide whether you can install package on specified platform. Eg. `scikit_learn‑0.18.1‑cp36‑cp36m‑win_amd64.whl`[package_name]-[version]-[python interpreter]-[python-interpreter]-[Operating System].whl. If name of wheel file is changed, so platform does match, pip tries to install package even if platform or python version does not match. Removing platform or interpreter from name will rise an error in newest versoin of pip module `kjhfkjdf.whl is not a valid wheel filename.`.

Alternativly .whl file can be unpacked using an archiver as 7-zip. - It usually contains distribution meta folder and folder with source files. These source files can be simply unpacked to `site-packges` directory unless this wheel contain installation script, if so, it has to be run first.



#### Syntax


<li>pip.<function|attribute|class> where function is one of:
<ul>
<li>autocomplete()
<ul>
- Command and option completion for the main option parser (and options) and its subcommands (and options). Enable by sourcing one of the completion shell scripts (bash, zsh or fish).

- param args {list}
- returns {boolean}

- returns {pip.baseparser.ConfigOptionParser object}

- param args {list}
- returns {integer} If not failed than returns 0

- param args {list}

- returns {list}

- Command name auto-correct.
- param name {string}
- returns {boolean}

- Yields sorted (command name, command summary) tuples.

- returns {string}

- Is distribution an editable install?
- param dist {object}
- returns {boolean}

- attribute {dictionary}

