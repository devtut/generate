---
metaTitle: "Perl - Easy way to check installed modules on Mac and Ubuntu"
description: "Use perldoc to check the Perl package install path, Check installed perl modules via terminal, How to check Perl corelist modules., How to check the version of a installed module?"
---

# Easy way to check installed modules on Mac and Ubuntu




## Use perldoc to check the Perl package install path


```perl
$ perldoc -l Time::Local

```



## Check installed perl modules via terminal


Type below command:

`instmodsh`

It'll show you the guild as below:

```perl
Available commands are:
   l            - List all installed modules
   m <module>   - Select a module
   q            - Quit the program
cmd?

```

Then type `l` to list all the installed modules, you can also use command `m <module>` to select the module and get its information.

After finish, just type `q` to quit.



## How to check Perl corelist modules.


```perl
$ corelist -v v5.23.1

```



## How to check the version of a installed module?


```perl
$> perl -MFoo::Bar\ 9999
$> Foo::Bar version 9999 required--this is only version 1.1.

```

