---
metaTitle: "Bash - Managing PATH environment variable"
description: "Add a path to the PATH environment variable, Remove a path from the PATH environment variable"
---

# Managing PATH environment variable

## Add a path to the PATH environment variable

The PATH environment variable is generally defined in ~/.bashrc or ~/.bash_profile or /etc/profile or ~/.profile or /etc/bash.bashrc (distro specific Bash configuration file)

```bash
$ echo $PATH
/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin:/usr/lib/jvm/jdk1.8.0_92/bin:/usr/lib/jvm/jdk1.8.0_92/db/bin:/usr/lib/jvm/jdk1.8.0_92/jre/bin

```

Now, if we want to add a path (e.g `~/bin`) to the PATH variable:

```bash
PATH=~/bin:$PATH
# or
PATH=$PATH:~/bin

```

But this will modify the PATH only in the current shell (and its subshell). Once you exit the shell, this modification will be gone.

To make it permanent, we need to add that bit of code to the ~/.bashrc (or whatever) file and reload the file.

If you run the following code (in terminal), it will add `~/bin` to the PATH permanently:

```bash
echo 'PATH=~/bin:$PATH' >> ~/.bashrc && source ~/.bashrc

```

Explanation:

- `echo 'PATH=~/bin:$PATH' >> ~/.bashrc` adds the line `PATH=~/bin:$PATH` at the end of ~/.bashrc file (you could do it with a text editor)
- `source ~/.bashrc` reloads the ~/.bashrc file

```bash
path=~/bin            # path to be included
bashrc=~/.bashrc      # bash file to be written and reloaded
# run the following code unmodified
echo $PATH | grep -q "\(^\|:\)$path\(:\|/\{0,1\}$\)" || echo "PATH=\$PATH:$path" >> "$bashrc"; source "$bashrc"

```

## Remove a path from the PATH environment variable

To remove a PATH from a PATH environment variable, you need to edit ~/.bashrc or ~/.bash_profile or /etc/profile or ~/.profile or /etc/bash.bashrc (distro specific) file and remove the assignment for that particular path.

Instead of finding the exact assignment, you could just do a replacement in the `$PATH` in its final stage.

The following will safely remove `$path` from `$PATH`:

```bash
path=~/bin
PATH="$(echo "$PATH" |sed -e "s#\(^\|:\)$(echo "$path" |sed -e 's/[^^]/[&]/g' -e 's/\^/\\^/g')\(:\|/\{0,1\}$\)#\1\2#" -e 's#:\+#:#g' -e 's#^:\|:$##g')"

```

To make it permanent, you will need to add it at the end of your bash configuration file.

```bash
rpath(){
    for path in "$@";do
        PATH="$(echo "$PATH" |sed -e "s#\(^\|:\)$(echo "$path" |sed -e 's/[^^]/[&]/g' -e 's/\^/\\^/g')\(:\|/\{0,1\}$\)#\1\2#" -e 's#:\+#:#g' -e 's#^:\|:$##g')"
    done
    echo "$PATH"
}

PATH="$(rpath ~/bin /usr/local/sbin /usr/local/bin)"
PATH="$(rpath /usr/games)"
# etc ...

```

This will make it easier to handle multiple paths.

**Notes:**

- You will need to add these codes in the Bash configuration file (~/.bashrc or whatever).
- Run `source ~/.bashrc` to reload the Bash configuration (~/.bashrc) file.

#### Syntax

- Add path : PATH=\$PATH:/new/path
- Add path : PATH=/new/path:\$PATH

#### Parameters

| Parameter | Details                   |
| --------- | ------------------------- |
| PATH      | Path environment variable |

#### Remarks

**Bash configuration file:**

This file is sourced whenever a new interactive Bash shell is started.

In GNU/Linux systems it's generally the ~/.bashrc file; in Mac it's ~/.bash_profile or ~/.profile

**Export:**

The PATH variable must be exported once (It's done by default). Once it is exported it will remain exported and any changes made to it will be applied immediately.

**Apply changes:**

To apply changes to a Bash configuration file, you must reload that file in a terminal (`source /path/to/bash_config_file`)
