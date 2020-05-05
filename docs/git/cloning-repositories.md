---
metaTitle: "Git - Cloning Repositories"
description: "Shallow Clone, Regular Clone, Clone a specific branch, Clone recursively, Clone using a proxy"
---

# Cloning Repositories



## Shallow Clone


Cloning a huge repository (like a project with multiple years of history) might take a long time, or fail because of the amount of data to be transferred. In cases where you don't need to have the full history available, you can do a shallow clone:

```git
git clone [repo_url] --depth 1

```

The above command will fetch just the last commit from the remote repository.

Be aware that you may not be able to resolve merges in a shallow repository. It's often a good idea to take at least as many commits are you are going to need to backtrack to resolve merges. For example, to instead get the last 50 commits:

```git
git clone [repo_url] --depth 50

```

Later, if required, you can the fetch the rest of the repository:

```git
git fetch --unshallow     # equivalent of git fetch -–depth=2147483647
                          # fetches the rest of the repository

```

```git
git fetch --depth=1000    # fetch the last 1000 commits

```



## Regular Clone


To download the entire repository including the full history and all branches, type:

```git
git clone <url>

```

The example above will place it in a directory with the same name as the repository name.

To download the repository and save it in a specific directory, type:

```git
git clone <url> [directory]

```

For more details, visit [Clone a repository](http://stackoverflow.com/documentation/git/218/getting-started-with-git/818/clone-a-repository).



## Clone a specific branch


To clone a specific branch of a repository, type `--branch <branch name>` before the repository url:

```git
git clone --branch <branch name> <url> [directory]

```

To use the shorthand option for `--branch`, type `-b`. This command downloads entire repository and checks out `<branch name>`.

To save disk space you can clone history leading only to single branch with:

```git
git clone --branch <branch_name> --single-branch <url> [directory]

```

If `--single-branch` is not added to the command, history of all branches will be cloned into `[directory]`. This can be issue with big repositories.

To later undo `--single-branch` flag and fetch the rest of repository use command:

```git
git config remote.origin.fetch "+refs/heads/*:refs/remotes/origin/*"
git fetch origin

```



## Clone recursively


```git
git clone <url> --recursive

```

Clones the repository and also clones all submodules. If the submodules themselves contain additional submodules, Git will also clone those.



## Clone using a proxy


If you need to download files with git under a proxy, setting proxy server system-wide couldn't be enough. You could also try the following:

```git
git config --global http.proxy http://<proxy-server>:<port>/

```



#### Syntax


- git clone [<options>] [--] <repo> [<dir>]
- git clone [--template=<template_directory>] [-l] [-s] [--no-hardlinks] [-q] [-n] [--bare] [--mirror] [-o <name>] [-b <name>] [-u <upload-pack>] [--reference <repository>] [--dissociate] [--separate-git-dir <git dir>] [--depth <depth>] [--[no-]single-branch] [--recursive | --recurse-submodules] [--[no-]shallow-submodules] [--jobs <n>] [--] <repository> [<directory>]

