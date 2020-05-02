---
metaTitle: "Reflog - Restoring commits not shown in git log"
description: "Recovering from a bad rebase"
---

# Reflog - Restoring commits not shown in git log



## Recovering from a bad rebase


Suppose that you had started an interactive rebase:

```git
git rebase --interactive HEAD~20

```

and by mistake, you squashed or dropped some commits that you didn't want to lose, but then completed the rebase. To recover, do `git reflog`, and you might see some output like this:

```git
aaaaaaa HEAD@{0} rebase -i (finish): returning to refs/head/master
bbbbbbb HEAD@{1} rebase -i (squash): Fix parse error
...
ccccccc HEAD@{n} rebase -i (start): checkout HEAD~20
ddddddd HEAD@{n+1} ...
...

```

In this case, the last commit, `ddddddd` (or `HEAD@{n+1}`) is the tip of your **pre-rebase** branch. Thus, to recover that commit (and all parent commits, including those accidentally squashed or dropped), do:

```git
$ git checkout HEAD@{n+1}

```

You can then create a new branch at that commit with `git checkout -b [branch]`. See [Branching](https://stackoverflow.com/documentation/git/415/branching/1633/creating-and-checking-out-new-branches#t=201609211725096795131) for more information.



#### Remarks


Git's reflog records the position of HEAD (the ref for the current state of the repository) every time that it is changed.  Generally, every operation that might be destructive involves moving the HEAD pointer (since if anything is changed, including in the past, the tip commit's hash will change), so it is always possible to revert back to an older state, before a dangerous operation, by finding the right line in the reflog.

Objects that are not referenced by any ref are usually garbage collected in ~30 days, however, so the reflog may not always be able to help.

