---
metaTitle: "Bash - Aliasing"
description: "Bypass an alias, Create an Alias, Remove an alias, List all Aliases, Expand alias, The BASH_ALIASES is an internal bash assoc array"
---

# Aliasing


Shell aliases are a simple way to create new commands or to wrap existing commands with code of your own.  They somewhat overlap with shell [functions](http://stackoverflow.com/documentation/bash/472/functions), which are however more versatile and should therefore often be preferred.



## Bypass an alias


Sometimes you may want to bypass an alias temporarily,
without disabling it.
To work with a concrete example, consider this alias:

```bash
alias ls='ls --color=auto'

```

And let's say you want to use the `ls` command without disabling the alias.
You have several options:

- Use the `command` builtin: `command ls`
- Use the full path of the command: `/bin/ls`
- Add a `\` anywhere in the command name, for example: `\ls`, or `l\s`
- Quote the command: `"ls"` or `'ls'`



## Create an Alias


```bash
alias word='command'

```

Invoking `word` will run `command`. Any arguments supplied to the alias are simply appended to the target of the alias:

```bash
alias myAlias='some command --with --options'
myAlias foo bar baz

```

The shell will then execute:

```bash
some command --with --options foo bar baz

```

To include multiple commands in the same alias, you can string them together with `&&`. For example:

```bash
alias print_things='echo "foo" && echo "bar" && echo "baz"'

```



## Remove an alias


To remove an existing alias, use:

```bash
unalias {alias_name}

```

Example:

```bash
# create an alias    
$ alias now='date'

# preview the alias
$ now
Thu Jul 21 17:11:25 CEST 2016

# remove the alias
$ unalias now

# test if removed
$ now
-bash: now: command not found

```



## List all Aliases


```bash
alias -p

```

will list all the current aliases.



## Expand alias


Assuming that `bar` is an alias for `someCommand -flag1`.

Type `bar` on the command line and then press <kbd>Ctrl</kbd>+<kbd>alt</kbd>+<kbd>e</kbd>

you'll get `someCommand -flag1` where `bar` was standing.



## The BASH_ALIASES is an internal bash assoc array


Aliases are named shortcuts of commands, one can define and use in interactive bash instances. They are held in an associative array named BASH_ALIASES. To use this var in a script, it must be run within an interactive shell

```bash
#!/bin/bash -li
# note the -li above! -l makes this behave like a login shell
# -i makes it behave like an interactive shell
#
# shopt -s expand_aliases will not work in most cases

echo There are ${#BASH_ALIASES[*]} aliases defined.

for ali in "${!BASH_ALIASES[@]}"; do
   printf "alias: %-10s triggers: %s\n" "$ali" "${BASH_ALIASES[$ali]}" 
done

```



#### Remarks


The alias will only be available in the shell where the alias command was issued.

To persist the alias consider putting it into your `.bashrc`

