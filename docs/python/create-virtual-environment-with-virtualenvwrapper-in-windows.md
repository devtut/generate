---
metaTitle Create virtual environment with virtualenvwrapper in windows
description Virtual environment with virtualenvwrapper for windows
---

# Create virtual environment with virtualenvwrapper in windows




## Virtual environment with virtualenvwrapper for windows


Suppose you need to work on three different projects project A, project B and project C. project A and project B need python 3 and some required libraries. But for project C you need python 2.7 and dependent libraries.

So best practice for this is to separate those project environments.
For creating separate python virtual environment need to follow below steps:

**Step 1:**
Install pip with this command: `python -m pip install -U pip`

**Step 2:**
Then install "virtualenvwrapper-win" package by using command (command can be executed windows power shell):

`pip install virtualenvwrapper-win`

**Step 3:**
Create a new virtualenv environment by using command:
`mkvirtualenv python_3.5`

**Step 4:**
Activate the environment by using command:

```
workon < environment name>

```

Main commands for virtualenvwrapper:

```
mkvirtualenv <name>
Create a new virtualenv environment named <name>. The environment will be created in WORKON_HOME.

lsvirtualenv
List all of the enviornments stored in WORKON_HOME.

rmvirtualenv <name>
Remove the environment <name>. Uses folder_delete.bat.

workon [<name>]
If <name> is specified, activate the environment named <name> (change the working virtualenv to <name>). If a project directory has been defined, we will change into it. If no argument is specified, list the available environments. One can pass additional option -c after virtualenv name to cd to virtualenv directory if no projectdir is set.

deactivate
Deactivate the working virtualenv and switch back to the default system Python.

add2virtualenv <full or relative path>
If a virtualenv environment is active, appends <path> to virtualenv_path_extensions.pth inside the environment’s site-packages, which effectively adds <path> to the environment’s PYTHONPATH. If a virtualenv environment is not active, appends <path> to virtualenv_path_extensions.pth inside the default Python’s site-packages. If <path> doesn’t exist, it will be created.

```

