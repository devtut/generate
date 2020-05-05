---
metaTitle: "Git - Aliases"
description: "List / search existing aliases, Simple aliases, Advanced Aliases, Temporarily ignore tracked files, Show pretty log with branch graph, Updating code while keeping a linear history, See which files are being ignored by your .gitignore configuration, Unstage staged files"
---

# Aliases



## List / search existing aliases


You can [list existing git aliases](http://stackoverflow.com/q/7066325/23649) using `--get-regexp`:

```git
$ git config --get-regexp '^alias\.'

```

### Searching aliases

To [search aliases](http://stackoverflow.com/questions/39466417/how-do-i-search-my-git-aliases/39466418#39466418), add the following to your `.gitconfig` under `[alias]`:

```git
aliases = !git config --list | grep ^alias\\. | cut -c 7- | grep -Ei --color \"$1\" "#"

```

Then you can:

- `git aliases` - show ALL aliases
- `git aliases commit` - only aliases containing "commit"



## Simple aliases


There are two ways of creating aliases in Git:

- with the `~/.gitconfig` file:

```git
[alias]
    ci = commit
    st = status
    co = checkout

```


- with the command line:

```

git config --global alias.ci "commit"
 git config --global alias.st "status"
 git config --global alias.co "checkout"

```

After the alias is created - type:

- `git ci` instead of `git commit`,
- `git st` instead of `git status`,
- `git co` instead of `git checkout`.

As with regular git commands, aliases can be used beside arguments. For example:

```

git ci -m "Commit message..."
 git co -b feature-42

```



## Advanced Aliases


Git lets you use non-git commands and full [`sh`](https://en.wikipedia.org/wiki/Bourne_shell) shell syntax in your aliases if you prefix them with `!`.

In your `~/.gitconfig` file:

```git
[alias]
    temp = !git add -A && git commit -m "Temp"

```

The fact that full shell syntax is available in these prefixed aliases also means you can use shell functions to construct more complex aliases, such as ones which utilize command line arguments:

```git
[alias]
    ignore = "!f() { echo $1 >> .gitignore; }; f"

```

The above alias defines the `f` function, then runs it with any arguments you pass to the alias. So running `git ignore .tmp/` would add `.tmp/` to your `.gitignore` file.

In fact, this pattern is so useful that Git defines `$1`, `$2`, etc. variables for you, so you don't even have to define a special function for it. (But keep in mind that Git will also append the arguments anyway, even if you access it via these variables, so you may want to add a dummy command at the end.)

Note that aliases prefixed with `!` in this way are run from the root directory of your git checkout, even if your current directory is deeper in the tree. This can be a useful way to run a command from the root without having to `cd` there explicitly.

```git
[alias]
    ignore = "! echo $1 >> .gitignore"

```



## Temporarily ignore tracked files


To temporarily mark a file as ignored (pass file as parameter to alias) - type:

```git
unwatch = update-index --assume-unchanged

```

To start tracking file again - type:

```git
watch = update-index --no-assume-unchanged

```

To list all files that has been temporarily ignored - type:

```git
unwatched = "!git ls-files -v | grep '^[[:lower:]]'"

```

To clear the unwatched list - type:

```git
watchall = "!git unwatched | xargs -L 1 -I % sh -c 'git watch `echo % | cut -c 2-`'"

```

Example of using the aliases:

```git
git unwatch my_file.txt
git watch my_file.txt
git unwatched
git watchall

```



## Show pretty log with branch graph


```git
[alias]
  logp=log --pretty=format:'%h %ad | %s%d [%an]' --graph --date=short

  lg =  log --graph --date-order --first-parent \
     --pretty=format:'%C(auto)%h%Creset %C(auto)%d%Creset %s %C(green)(%ad) %C(bold cyan)<%an>%Creset'
  lgb = log --graph --date-order --branches --first-parent \                      
     --pretty=format:'%C(auto)%h%Creset %C(auto)%d%Creset %s %C(green)(%ad) %C(bold cyan)<%an>%Creset'
 lga = log --graph --date-order --all \                                          
   --pretty=format:'%C(auto)%h%Creset %C(auto)%d%Creset %s %C(green)(%ad) %C(bold cyan)<%an>%Creset'

```

Here an explanation of the options and placeholder used in the `--pretty` format (exhaustive list are available with `git help log` )

--graph - draw the commit tree

--date-order - use commit timestamp order when possible

--first-parent - follow only the first parent on merge node.

--branches - show all local branches (by default, only current branch is shown)

--all - show all local and remotes branches

%h - hash value for commit (abbreviated)

%ad - Date stamp (author)

%an - Author username

%an - Commit username

%C(auto) - to use colors defined in [color] section

%Creset - to reset color

%d - --decorate (branch & tag names)

%s - commit message

%ad - author date (will follow --date directive) (and not commiter date)

%an - author name (can be %cn for commiter name)



## Updating code while keeping a linear history


Sometimes you need to keep a linear (non-branching) history of your code commits. If you are working on a branch for a while, this can be tricky if you have to do a regular `git pull` since that will record a merge with upstream.

```git
[alias]
  up = pull --rebase

```

This will update with your upstream source, then reapply any work you have not pushed on top of whatever you pulled down.

To use:

```git
git up

```



## See which files are being ignored by your .gitignore configuration


```git
[ alias ]

    ignored = ! git ls-files --others --ignored --exclude-standard --directory \
            && git ls-files --others -i --exclude-standard

```

Shows one line per file, so you can grep (only directories):

```git
$ git ignored | grep '/$'
.yardoc/
doc/

```

Or count:

```git
~$ git ignored | wc -l
199811                 # oops, my home directory is getting crowded

```



## Unstage staged files


Normally, to remove files that are staged to be committed using the `git reset` commit, `reset` has a lot of functions depending on the arguments provided to it. To completely unstage all files staged, we can make use of git aliases to create a new alias that uses `reset` but now we do not need to remember to provide the correct arguments to `reset`.

`git config --global alias.unstage "reset --"`

Now, any time you want to **unstage** stages files, type `git unstage` and you are good to go.

