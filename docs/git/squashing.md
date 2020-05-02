---
metaTitle: "Squashing"
description: "Squash Recent Commits Without Rebasing, Squashing Commits During a Rebase, Squashing Commit During Merge, Autosquash: Committing code you want to squash during a rebase, Autosquashing and fixups"
---

# Squashing




## Squash Recent Commits Without Rebasing


If you want to squash the previous `x` commits into a single one, you can use the following commands:

```git
git reset --soft HEAD~x
git commit

```

Replacing `x` with the number of previous commits you want to be included in the squashed commit.

Mind that this will create a **new** commit, essentially forgetting information about the previous `x` commits including their author, message and date. You probably want to **first** copy-paste an existing commit message.



## Squashing Commits During a Rebase


Commits can be squashed during a `git rebase`.  It is recommended that you understand [rebasing](http://stackoverflow.com/documentation/git/355/rebasing#t=201604292236379558058) before attempting to squash commits in this fashion.

<li>
Determine which commit you would like to rebase from, and note its commit hash.
</li>
<li>
Run `git rebase -i [commit hash]`.
Alternatively, you can type `HEAD~4` instead of a commit hash, to view the latest commit and 4 more commits before the latest one.
</li>
<li>
In the editor that opens when running this command, determine which commits you want to squash.  Replace `pick` at the beginning of those lines with `squash` to squash them into the previous commit.
</li>
<li>
After selecting which commits you would like to squash, you will be prompted to write a commit message.
</li>

Logging Commits to determine where to rebase

```git
> git log --oneline
612f2f7 This commit should not be squashed
d84b05d This commit should be squashed
ac60234 Yet another commit
36d15de Rebase from here
17692d1 Did some more stuff
e647334 Another Commit
2e30df6 Initial commit

> git rebase -i 36d15de

```

At this point your editor of choice pops up where you can describe what you want to do with the commits. Git provides help in the comments. If you leave it as is then nothing will happen because every commit will be kept and their order will be the same as they were before the rebase. In this example we apply the following commands:

```git
pick ac60234 Yet another commit
squash d84b05d This commit should be squashed
pick 612f2f7 This commit should not be squashed

# Rebase 36d15de..612f2f7 onto 36d15de (3 command(s))
#
# Commands:
# p, pick = use commit
# r, reword = use commit, but edit the commit message
# e, edit = use commit, but stop for amending
# s, squash = use commit, but meld into previous commit
# f, fixup = like "squash", but discard this commit's log message
# x, exec = run command (the rest of the line) using shell
#
# These lines can be re-ordered; they are executed from top to bottom.
#
# If you remove a line here THAT COMMIT WILL BE LOST.
#
# However, if you remove everything, the rebase will be aborted.
#
# Note that empty commits are commented out

```

Git log after writing commit message

```git
> git log --oneline
77393eb This commit should not be squashed
e090a8c Yet another commit
36d15de Rebase from here
17692d1 Did some more stuff
e647334 Another Commit
2e30df6 Initial commit

```



## Squashing Commit During Merge


You can use `git merge --squash` to squash changes introduced by a branch into a single commit. No actual commit will be created.

```git
git merge --squash <branch>
git commit

```

This is more or less equivalent to using `git reset`, but is more convenient when changes being incorporated have a symbolic name. Compare:

```git
git checkout <branch>
git reset --soft $(git merge-base master <branch>)
git commit

```



## Autosquash: Committing code you want to squash during a rebase


Given the following history, imagine you make a change that you want to squash into the commit `bbb2222 A second commit`:

```git
$ git log --oneline --decorate
ccc3333 (HEAD -> master) A third commit
bbb2222 A second commit
aaa1111 A first commit
9999999 Initial commit

```

Once you've made your changes, you can add them to the index as usual, then commit them using the `--fixup` argument with a reference to the commit you want to squash into:

```git
$ git add .
$ git commit --fixup bbb2222
[my-feature-branch ddd4444] fixup! A second commit

```

This will create a new commit with a commit message that Git can recognize during an interactive rebase:

```git
$ git log --oneline --decorate
ddd4444 (HEAD -> master) fixup! A second commit
ccc3333 A third commit
bbb2222 A second commit
aaa1111 A first commit
9999999 Initial commit

```

Next, do an interactive rebase with the `--autosquash` argument:

```git
$ git rebase --autosquash --interactive HEAD~4

```

Git will propose you to squash the commit you made with the `commit --fixup` into the correct position:

```git
pick aaa1111 A first commit
pick bbb2222 A second commit
fixup ddd4444 fixup! A second commit
pick ccc3333 A third commit

```

To avoid having to type `--autosquash` on every rebase, you can enable this option by default:

```git
$ git config --global rebase.autosquash true

```



## Autosquashing and fixups


When committing changes it is possible to specify that the commit will in future be squashed to another commit and this can be done like so,

`git commit --squash=[commit hash of commit to which this commit will be squashed to]`

One might also use, `--fixup=[commit hash]` alternatively to fixup.

It is also possible to use words from the commit message instead of the commit hash, like so,

`git commit --squash :/things`

where the most recent commit with the word 'things' would be used.

These commits' message would begin with `'fixup!'` or `'squash!'` followed by the rest of the commit message to which these commits will be squashed to.

When rebasing `--autosquash` flag should be used to use the autosquash/fixup feature.



#### Remarks


### What is squashing?

Squashing is the process of taking multiple commits and combining them into a single commit encapsulating all the changes from the initial commits.

### Squashing and Remote Branches

Pay special attention when squashing commits on a branch that is tracking a remote branch; if you squash a commit that has already been pushed to a remote branch, the two branches will be diverged, and you will have to use `git push -f` to force those changes onto the remote branch.  **Be aware that this can cause issues for others tracking that remote branch**, so caution should be used when force-pushing squashed commits onto public or shared repositories.

If the project is hosted on GitHub, you can enable "force push protection" on some branches, like `master`, by adding it to `Settings` - `Branches` - `Protected Branches`.

