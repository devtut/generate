---
metaTitle: "Git - Recovering"
description: "Recovering from a lost commit, Restore a deleted file after a commit, Restore file to a previous version, Recover a deleted branch, Recovering from a reset, Recover from git stash"
---

# Recovering



## Recovering from a lost commit


In case you have reverted back to a past commit and lost a newer commit you can recover the lost commit by running

```git
git reflog

```

Then find your lost commit, and reset back to it by doing

```git
git reset HEAD --hard <sha1-of-commit>

```



## Restore a deleted file after a commit


In case you have accidentally commited a delete on a file and later realized that you need it back.

First find the commit id of the commit that deleted your file.

```git
git log --diff-filter=D --summary

```

Will give you a sorted summary of commits which deleted files.

Then proceed to restore the file by

```git
git checkout 81eeccf~1 <your-lost-file-name>

```

(Replace  81eeccf with your own commit id)



## Restore file to a previous version


To restore a file to a previous version you can use `reset`.

```git
git reset <sha1-of-commit> <file-name>

```

If you have already made local changes to the file (that you do not require!) you can also use the `--hard` option



## Recover a deleted branch


To recover a deleted branch you need to find the commit which was the head of your deleted branch by running

```git
git reflog

```

You can then recreate the branch by running

```git
git checkout -b <branch-name> <sha1-of-commit>

```

You will not be able to recover deleted branches if git's [garbage collector](https://git-scm.com/docs/git-gc) deleted dangling commits - those without refs. Always have a backup of your repository, especially when you work in a small team / proprietary project



## Recovering from a reset


### With Git, you can (almost) always turn the clock back

Don't be afraid to experiment with commands that rewrite history*. Git doesn't delete your commits for 90 days by default, and during that time you can easily recover them from the reflog:

```git
$ git reset @~3   # go back 3 commits
$ git reflog
c4f708b HEAD@{0}: reset: moving to @~3
2c52489 HEAD@{1}: commit: more changes
4a5246d HEAD@{2}: commit: make important changes
e8571e4 HEAD@{3}: commit: make some changes
... earlier commits ...
$ git reset 2c52489
... and you're back where you started

```

* **Watch out for options like `--hard` and `--force` though — they can discard data.**<br />
*  **Also, avoid rewriting history on any branches you're collaborating on.**



## Recover from git stash


To get your most recent stash after running git stash, use

```git
git stash apply

```

To see a list of your stashes, use

```git
git stash list

```

You will get a list that looks something like this

```git
stash@{0}: WIP on master: 67a4e01 Merge tests into develop
stash@{1}: WIP on master: 70f0d95 Add user role to localStorage on user login

```

Choose a different git stash to restore with the number that shows up for the stash you want

```git
git stash apply stash@{2}

```

You can also choose 'git stash pop', it works same as 'git stash apply' like..

```

git stash pop 

```

or

```

git stash pop stash@{2}

```

Difference in git stash apply and git stash pop...

**git stash pop**:- stash data will be remove from stack of stash list.

Ex:-

```git
git stash list

```

You will get a list that looks something like this

```git
stash@{0}: WIP on master: 67a4e01 Merge tests into develop
stash@{1}: WIP on master: 70f0d95 Add user role to localStorage on user login

```

Now pop stash data using command

```git
git stash pop

```

Again Check for stash list

```git
git stash list

```

You will get a list that looks something like this

```

stash@{0}: WIP on master: 70f0d95 Add user role to localStorage on user login

```

You can see one stash data is removed (popped) from stash list and stash@{1} became stash@{0}.

