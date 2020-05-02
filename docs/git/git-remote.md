---
metaTitle: "Git Remote"
description: "Display Remote Repositories, Change remote url of your Git repository, Remove a Remote Repository, Add a Remote Repository, Show more information about remote repository, Rename a Remote Repository"
---

# Git Remote




## Display Remote Repositories


To list all configured remote repositories, use `git remote`.

It shows the short name (aliases) of each remote handle that you have configured.

```git
$ git remote
premium
premiumPro
origin

```

To show more detailed information, the `--verbose` or `-v` flag can be used. The output will include the URL and the type of the remote (`push` or `pull`):

```git
$ git remote -v
premiumPro    https://github.com/user/CatClickerPro.git (fetch)
premiumPro    https://github.com/user/CatClickerPro.git (push)
premium    https://github.com/user/CatClicker.git (fetch)
premium    https://github.com/user/CatClicker.git (push)
origin    https://github.com/ud/starter.git (fetch)
origin    https://github.com/ud/starter.git (push)

```



## Change remote url of your Git repository


You may want to do this if the remote repository is migrated. The command for changing the remote url is:

```git
git remote set-url

```

It takes 2 arguments: an existing remote name (origin, upstream) and the url.

Check your current remote url:

```git
git remote -v
origin    https://bitbucket.com/develop/myrepo.git (fetch)
origin    https://bitbucket.com/develop/myrepo.git (push)

```

Change your remote url:

```git
git remote set-url origin https://localserver/develop/myrepo.git

```

Check again your remote url:

```git
git remote -v
origin    https://localserver/develop/myrepo.git (fetch)
origin    https://localserver/develop/myrepo.git (push)

```



## Remove a Remote Repository


Remove the remote named `<name>`. All remote-tracking branches and configuration settings for the remote are removed.

To remove a remote repository `dev`:

```git
git remote rm dev

```



## Add a Remote Repository


To add a remote, use `git remote add` in the root of your local repository.

For adding a remote Git repository <url> as an easy short name <name> use

```git
git remote add <name> <url>

```

The command `git fetch <name>` can then be used to create and update remote-tracking branches `<name>/<branch>`.



## Show more information about remote repository


You can view more information about a remote repository by `git remote show <remote repository alias>`

```git
git remote show origin

```

result:

```git
remote origin
Fetch URL:  https://localserver/develop/myrepo.git
Push  URL:  https://localserver/develop/myrepo.git
HEAD branch: master
Remote branches:
  master      tracked
Local branches configured for 'git pull':
  master      merges with remote master
Local refs configured for 'git push':
  master      pushes to master      (up to date)

```



## Rename a Remote Repository


Rename the remote named `<old>` to `<new>`. All remote-tracking branches and configuration settings for the remote are updated.

To rename a remote branch name `dev` to `dev1` :

```git
git remote rename dev dev1

```



#### Syntax


- `git remote [-v | --verbose]`
- `git remote add [-t <branch>] [-m <master>] [-f] [--[no-]tags] [--mirror=<fetch|push>]<name> <url>`
- `git remote rename <old> <new>`
- `git remote remove <name>`
- `git remote set-head <name> (-a | --auto | -d | --delete | <branch>)`
- `git remote set-branches [--add] <name> <branch>...`
- `git remote set-url [--push] <name> <newurl> [<oldurl>]`
- `git remote set-url --add [--push] <name> <newurl>`
- `git remote set-url --delete [--push] <name> <url>`
- `git remote [-v | --verbose] show [-n] <name>...`
- `git remote prune [-n | --dry-run] <name>...`
- `git remote [-v | --verbose] update [-p | --prune] [(<group> | <remote>)...]`
- `git remote show <name>`



#### Parameters


|Parameter|Details
|------
|-v, --verbose|Run verbosely.
|-m <master>|Sets head to remote's <master> branch
|--mirror=fetch|Refs will not be stored in refs/remotes namespace, but instead will be mirrored in the local repo
|--mirror=push|`git push` will behave as if --mirror was passed
|--no-tags|`git fetch <name>` does not import tags from the remote repo
|-t <branch>|Specifies the remote to track **only** <branch>
|-f|`git fetch <name>` is run immediately after remote is set up
|--tags|`git fetch <name>` imports every tag from the remote repo
|-a, --auto|The symbolic-ref's HEAD is set to the same branch as the remote's HEAD
|-d, --delete|All listed refs are deleted from the remote repository
|--add|Adds <name> to list of currently tracked branches (set-branches)
|--add|Instead of changing some URL, new URL is added (set-url)
|--all|Push all branches.
|--delete|All urls matching <url> are deleted. (set-url)
|--push|Push URLS are manipulated instead of fetch URLS
|-n|The remote heads are not queried first with `git ls-remote <name>`, cached information is used instead
|--dry-run|report what branches will be pruned, but do not actually prune them
|--prune|Remove remote branches that don't have a local counterpart

