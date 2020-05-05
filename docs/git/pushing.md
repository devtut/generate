---
metaTitle: "Pushing"
description: "Push a specific object to a remote branch, Push, Force Pushing, Push tags, Changing the default push behavior"
---

# Pushing

After changing, staging, and committing code with Git, pushing is required to make your changes available to others and transfers your local changes to the repository server. This topic will cover how to properly push code using Git.

## Push a specific object to a remote branch

### General syntax

```git
git push <remotename> <object>:<remotebranchname>

```

### Example

```git
git push origin master:wip-yourname

```

Will push your master branch to the `wip-yourname` branch of origin (most of the time, the repository you cloned from).

### Delete remote branch

Deleting the remote branch is the equivalent of pushing an empty object to it.

```git
git push <remotename> :<remotebranchname>

```

### Example

```git
git push origin :wip-yourname

```

Will delete the remote branch `wip-yourname`

Instead of using the colon, you can also use the --delete flag, which is better readable in some cases.

### Example

```git
git push origin --delete wip-yourname

```

### Push a single commit

If you have a single commit in your branch that you want to push to a remote without pushing anything else, you can use the following

```git
git push <remotename> <commit SHA>:<remotebranchname>

```

### Example

Assuming a git history like this

```git
eeb32bc Commit 1 - already pushed
347d700 Commit 2 - want to push
e539af8 Commit 3 - only local
5d339db Commit 4 - only local

```

to push only commit **347d700** to remote **master** use the following command

```git
git push origin 347d700:master

```

## Push

```git
git push

```

will push your code to your existing upstream. Depending on the push configuration, it will either push code from you current branch (default in Git 2.x) or from all branches (default in Git 1.x).

### Specify remote repository

When working with git, it can be handy to have multiple remote repositories. To specify a remote repository to push to, just append its name to the command.

```git
git push origin

```

### Specify Branch

To push to a specific branch, say `feature_x`:

```git
git push origin feature_x

```

### Set the remote tracking branch

Unless the branch you are working on originally comes from a remote repository, simply using `git push` won't work the first time. You must perform the following command to tell git to push the current branch to a specific remote/branch combination

```git
git push --set-upstream origin master

```

Here, `master` is the branch name on the remote `origin`. You can use `-u` as a shorthand for `--set-upstream`.

### Pushing to a new repository

To push to a repository that you haven't made yet, or is empty:

1. Create the repository on GitHub (if applicable)
1. Copy the url given to you, in the form `https://github.com/USERNAME/REPO_NAME.git`
   <li>Go to your local repository, and execute `git remote add origin URL`
   <ul>
1. To verify it was added, run `git remote -v`
   </ul>
   </li>
1. Run `git push origin master`

Your code should now be on GitHub

For more information view [Adding a remote repository](http://stackoverflow.com/documentation/git/4071/git-remote/16208/add-a-remote-repository#t=20161222004640218776)

### Explanation

Push code means that git will analyze the differences of your local commits and remote and send them to be written on the upstream. When push succeeds, your local repository and remote repository are synchronized and other users can see your commits.

For more details on the concepts of "upstream" and "downstream", see [Remarks](//stackoverflow.com/documentation/git/2600/pushing#remarks).

## Force Pushing

Sometimes, when you have local changes incompatible with remote changes (ie, when you cannot fast-forward the remote branch, or the remote branch is not a direct ancestor of your local branch), the only way to push your changes is a force push.

```git
git push -f

```

or

```git
git push --force

```

### Important notes

This will **overwrite** any remote changes and your remote will match your local.

Attention: Using this command may cause the remote repository to **lose commits**. Moreover, it is strongly advised against doing a force push if you are sharing this remote repository with others, since their history will retain every overwritten commit, thus rending their work out of sync with the remote repository.

As a rule of thumb, only force push when:

- Nobody except you pulled the changes you are trying to overwrite
- You can force everyone to clone a fresh copy after the forced push and make everyone apply their changes to it (people may hate you for this).

## Push tags

```git
git push --tags

```

Pushes all of the `git tags` in the local repository that are not in the remote one.

## Changing the default push behavior

**Current** updates the branch on the remote repository that shares a name with the current working branch.

```git
git config push.default current

```

**Simple** pushes to the upstream branch, but will not work if the upstream branch is called something else.

```git
git config push.default simple

```

**Upstream** pushes to the upstream branch, no matter what it is called.

```git
git config push.default upstream

```

**Matching** pushes all branches that match on the local and the remote
git config push.default upstream

After you've set the preferred style, use

```git
git push

```

to update the remote repository.

#### Syntax

- git push [-f | --force][-v | --verbose] [<remote> [<refspec>...]]

#### Parameters

| Parameter    | Details                                                                                                                   |
| ------------ | ------------------------------------------------------------------------------------------------------------------------- |
| --force      | Overwrites the remote ref to match your local ref. **Can cause the remote repository to lose commits, so use with care**. |
| --verbose    | Run verbosely.                                                                                                            |
| <remote>     | The remote repository that is destination of the push operation.                                                          |
| <refspec>... | Specify what remote ref to update with what local ref or object.                                                          |

#### Remarks

### Upstream & Downstream

>

<p>In terms of source control, you're **"downstream"** when you copy (clone,
checkout, etc) from a repository. Information flowed "downstream" to
you.</p>
<p>When you make changes, you usually want to send them back **"upstream"**
so they make it into that repository so that everyone pulling from the
same source is working with all the same changes. This is mostly a
social issue of how everyone can coordinate their work rather than a
technical requirement of source control. You want to get your changes
into the main project so you're not tracking divergent lines of
development.</p>
<p>Sometimes you'll read about package or release managers (the people,
not the tool) talking about submitting changes to "upstream". That
usually means they had to adjust the original sources so they could
create a package for their system. They don't want to keep making
those changes, so if they send them "upstream" to the original source,
they shouldn't have to deal with the same issue in the next release.</p>

([Source](http://stackoverflow.com/a/2739476/163024))
