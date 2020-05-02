---
metaTitle: "Branching"
description: "Creating and checking out new branches, Listing branches, Delete a remote branch, Quick switch to the previous branch, Check out a new branch tracking a remote branch, Delete a branch locally, Create an orphan branch (i.e. branch with no parent commit), Rename a branch, Push branch to remote, Searching in branches, Move current branch HEAD to an arbitrary commit, Overwrite single file in current working directory with the same from another branch"
---

# Branching




## Creating and checking out new branches


To create a new branch, while staying on the current branch, use:

```git
git branch <name>

```

Generally, the branch name must not contain spaces and is subject to other specifications listed [here](http://stackoverflow.com/questions/3651860/which-characters-are-illegal-within-a-branch-name). To switch to an existing branch :

```git
git checkout <name>

```

To create a new branch and switch to it:

```git
git checkout -b <name>

```

To create a branch at a point other than the last commit of the current branch (also known as HEAD), use either of these commands:

```git
git branch <name> [<start-point>]
git checkout -b <name> [<start-point>]

```

The `<start-point>` can be any [revision](https://git-scm.com/docs/revisions) known to git (e.g. another branch name, commit SHA, or a symbolic reference such as HEAD or a tag name):

```git
git checkout -b <name> some_other_branch
git checkout -b <name> af295
git checkout -b <name> HEAD~5
git checkout -b <name> v1.0.5

```

To create a branch from a [remote branch](http://stackoverflow.com/documentation/git/243/working-with-remotes) (the default `<remote_name>` is origin):

```git
git branch <name> <remote_name>/<branch_name>
git checkout -b <name> <remote_name>/<branch_name>

```

If a given branch name is only found on one remote, you can simply use

```git
git checkout -b <branch_name>

```

which is equivalent to

```git
git checkout -b <branch_name> <remote_name>/<branch_name>

```

Sometimes you may need to move several of your recent commits to a new branch. This can be achieved by branching and "rolling back", like so:

```git
git branch <new_name>
git reset --hard HEAD~2 # Go back 2 commits, you will lose uncommitted work.
git checkout <new_name>

```

Here is an illustrative explanation of this technique:

```

Initial state       After git branch <new_name>    After git reset --hard HEAD~2
                             newBranch                        newBranch
                                 ↓                                ↓
A-B-C-D-E (HEAD)         A-B-C-D-E (HEAD)                 A-B-C-D-E (HEAD)
        ↑                        ↑                            ↑
      master                   master                       master

```



## Listing branches


Git provides multiple commands for listing branches. All commands use the function of `git branch`, which will provide a list of a certain branches, depending on which options are put on the command line. Git will if possible, indicate the currently selected branch with a star next to it.

|Goal|Command
|------
|List local branches|`git branch`
|List local branches verbose|`git branch -v`
|List remote and local branches|`git branch -a` OR `git branch --all`
|List remote and local branches (verbose)|`git branch -av`
|List remote branches|`git branch -r`
|List remote branches with latest commit|`git branch -rv`
|List merged branches|`git branch --merged`
|List unmerged branches|`git branch --no-merged`
|List branches containing commit|`git branch --contains [<commit>]`

**Notes**:

- Adding an additional `v` to `-v` e.g. `$ git branch -avv` or `$ git branch -vv` will print the name of the upstream branch as well.
- Branches shown in red color are remote branches



## Delete a remote branch


To delete a branch on the `origin` remote repository, you can use for Git version 1.5.0 and newer

```git
git push origin :<branchName>

```

and as of Git version 1.7.0, you can delete a remote branch using

```git
git push origin --delete <branchName>

```

To delete a local remote-tracking branch:

```git
git branch --delete --remotes <remote>/<branch>
git branch -dr <remote>/<branch> # Shorter

git fetch <remote> --prune # Delete multiple obsolete tracking branches
git fetch <remote> -p      # Shorter

```

To delete a branch locally. Note that this will not delete the branch if it has any unmerged changes:

```git
git branch -d <branchName>

```

To delete a branch, even if it has unmerged changes:

```git
git branch -D <branchName>

```



## Quick switch to the previous branch


You can quickly switch to the previous branch using

```git
git checkout -

```



## Check out a new branch tracking a remote branch


There are three ways of creating a new branch `feature` which tracks the remote branch `origin/feature`:

- `git checkout --track -b feature origin/feature`,
- `git checkout -t origin/feature`,
- `git checkout feature` - assuming that there is no local `feature` branch and there is only one remote with the `feature` branch.

To set upstream to track the remote branch - type:

- `git branch --set-upstream-to=<remote>/<branch> <branch>`
- `git branch -u <remote>/<branch> <branch>`

where:

- `<remote>` can be: `origin`, `develop` or the one created by user,
- `<branch>` is user's branch to track on remote.

To verify which remote branches your local branches are tracking:

- `git branch -vv`



## Delete a branch locally


```git
$ git branch -d dev

```

Deletes the branch named `dev` **if** its changes are merged with another branch and will not be lost.  If the `dev` branch does contain changes that have not yet been merged that would be lost, `git branch -d` will fail:

```git
$ git branch -d dev
error: The branch 'dev' is not fully merged.
If you are sure you want to delete it, run 'git branch -D dev'.

```

Per the warning message, you can force delete the branch (and lose any unmerged changes in that branch) by using the `-D` flag:

```git
$ git branch -D dev

```



## Create an orphan branch (i.e. branch with no parent commit)


```git
git checkout --orphan new-orphan-branch

```

> 
The first commit made on this new branch will have no parents and it will be the root of a new history totally disconnected from all the other branches and commits.


[source](https://git-scm.com/docs/git-checkout)



## Rename a branch


Rename the branch you have checked out:

```git
git branch -m new_branch_name

```

Rename another branch:

```git
git branch -m branch_you_want_to_rename new_branch_name

```



## Push branch to remote


Use to push commits made on your local branch to a remote repository.

The `git push` command takes two arguments:

- A remote name, for example, `origin`
<li>A branch name, for example,
`master`</li>

For example:

```git
git push  <REMOTENAME> <BRANCHNAME>

```

As an example, you usually run `git push origin master` to push your local changes to your online repository.

Using `-u` (short for `--set-upstream`) will set up the tracking information during the push.

```git
git push -u <REMOTENAME> <BRANCHNAME>

```

By default, `git` pushes the local branch to a remote branch with the same name. For example, if you have a local called `new-feature`, if you push the local branch it will create a remote branch `new-feature` as well. If you want to use a different name for the remote branch, append the remote name after the local branch name, separated by `:`:

```git
git push <REMOTENAME> <LOCALBRANCHNAME>:<REMOTEBRANCHNAME>

```



## Searching in branches


To list local branches that contain a specific commit or tag

```git
git branch --contains <commit>

```

To list local and remote branches that contain a specific commit or tag

```git
git branch -a --contains <commit>

```



## Move current branch HEAD to an arbitrary commit


A branch is just a pointer to a commit, so you can freely move it around. To make it so that the branch is referring to the commit `aabbcc`, issue the command

```git
git reset --hard aabbcc

```

Please note that this will overwrite your branch's current commit, and as so, its entire history. You might loose some work by issuing this command. If that's the case, you can use the [reflog](https://stackoverflow.com/documentation/git/5149/reflog-restoring-commits-not-shown-in-git-log) to recover the lost commits. It can be advised to perform this command on a new branch instead of your current one.

However, this command can be particularly useful when rebasing or doing such other large history modifications.



## Overwrite single file in current working directory with the same from another branch


The checked out file will **overwrite** not yet commited changes you did in this file.

This command will check out the file `file.example` (which is located in the directory `path/to/`) and **overwrite any changes** you might have made to this file.

```git
git checkout some-branch path/to/file

```

**`some-branch` can be anything `tree-ish` known to git (see [Revision Selection](https://git-scm.com/book/en/v2/Git-Tools-Revision-Selection) and [gitrevisions](https://git-scm.com/docs/gitrevisions) for more details)**

You have to add `--` before the path if your file could be mistaken for a file (optional otherwise). No more options can be supplied after the `--`.

```git
git checkout some-branch -- some-file

```

The second `some-file` is a file in this example.



#### Syntax


- `git branch [--set-upstream | --track | --no-track] [-l] [-f] <branchname> [<start-point>]`
- `git branch (--set-upstream-to=<upstream> | -u <upstream>) [<branchname>]`
- `git branch --unset-upstream [<branchname>]`
- `git branch (-m | -M) [<oldbranch>] <newbranch>`
- `git branch (-d | -D) [-r] <branchname>…​`
- `git branch --edit-description [<branchname>]`
- `git branch [--color[=<when>] | --no-color] [-r | -a] [--list] [-v [--abbrev=<length> | --no-abbrev]] [--column[=<options>] | --no-column] [(--merged | --no-merged | --contains) [<commit>]] [--sort=<key>] [--points-at <object>] [<pattern>…​]`



#### Parameters


|Parameter|Details
|------
|-d, --delete|Delete a branch. The branch must be fully merged in its upstream branch, or in `HEAD` if no upstream was set with `--track` or `--set-upstream`
|-D|Shortcut for `--delete --force`
|-m, --move|Move/rename a branch and the corresponding reflog
|-M|Shortcut for `--move --force`
|-r, --remotes|List or delete (if used with -d) the remote-tracking branches
|-a, --all|List both remote-tracking branches and local branches
|--list|Activate the list mode. `git branch <pattern>` would try to create a branch, use `git branch --list <pattern>` to list matching branches
|--set-upstream|If specified branch does not exist yet or if `--force` has been given, acts exactly like `--track`. Otherwise sets up configuration like --track would when creating the branch, except that where branch points to is not changed



#### Remarks


Every git repository has one or more **branches**.  A branch is a named reference to the `HEAD` of a sequence of commits.

A git repo has a **current** branch (indicated by a `*` in the list of branch names printed by the `git branch` command),  Whenever you create a new commit with the `git commit` command, your new commit becomes the `HEAD` of the current branch, and the previous HEAD becomes the parent of the new commit.

A new branch will have the same `HEAD` as the branch from which it was created until something is committed to the new branch.

