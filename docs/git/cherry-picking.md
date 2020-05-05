---
metaTitle: "Cherry Picking"
description: "Copying a commit from one branch to another, Copying a range of commits from one branch to another, Checking if a cherry-pick is required, Find commits yet to be applied to upstream"
---

# Cherry Picking

A cherry-pick takes the patch that was introduced in a commit and tries to reapply it on the branch you’re currently on.

[Source: Git SCM Book](https://git-scm.com/book/en/v2/Distributed-Git-Maintaining-a-Project)

## Copying a commit from one branch to another

`git cherry-pick <commit-hash>` will apply the changes made in an existing commit to another branch, while recording a new commit. Essentially, you can copy commits from branch to branch.

Given the following tree [(Source)](https://ariejan.net/2010/06/10/cherry-picking-specific-commits-from-another-branch/)

```git
dd2e86 - 946992 - 9143a9 - a6fd86 - 5a6057 [master]
           \
            76cada - 62ecb3 - b886a0 [feature]

```

Let's say we want to copy `b886a0` to master (on top of `5a6057`).

We can run

```git
git checkout master
git cherry-pick b886a0

```

Now our tree will look something like:

```git
dd2e86 - 946992 - 9143a9 - a6fd86 - 5a6057 - a66b23 [master]
           \
            76cada - 62ecb3 - b886a0 [feature]

```

Where the new commit `a66b23` has the same content (source diff, commit message) as `b886a0` (but a different parent). Note that cherry-picking will only pick up changes on that commit(`b886a0` in this case) not all the changes in feature branch (for this you will have to either use rebasing or merging).

## Copying a range of commits from one branch to another

`git cherry-pick <commit-A>..<commit-B>` will place every commit **after** A and up to and including B on top of the currently checked-out branch.

`git cherry-pick <commit-A>^..<commit-B>` will place commit A and every commit up to and including B on top of the currently checked-out branch.

## Checking if a cherry-pick is required

Before you start the cherry-pick process, you can check if the commit you want to cherry-pick already exists in the target branch, in which case you don't have to do anything.

`git branch --contains <commit>` lists local branches that contain the specified commit.

`git branch -r --contains <commit>` also includes remote tracking branches in the list.

## Find commits yet to be applied to upstream

Command `git cherry` shows the changes which haven't yet been cherry-picked.

Example:

```git
git checkout master
git cherry development

```

... and see output a bit like this:

```git
+ 492508acab7b454eee8b805f8ba906056eede0ff
- 5ceb5a9077ddb9e78b1e8f24bfc70e674c627949
+ b4459544c000f4d51d1ec23f279d9cdb19c1d32b
+ b6ce3b78e938644a293b2dd2a15b2fecb1b54cd9

```

The commits that being with `+` will be the ones that haven't yet cherry-picked into `development`.

**Syntax:**

`git cherry [-v] [<upstream> [<head> [<limit>]]]`

Options:

**-v** Show the commit subjects next to the SHA1s.

**< upstream >** Upstream branch to search for equivalent commits. Defaults to the upstream branch of HEAD.

**< head >** Working branch; defaults to HEAD.

**< limit >** Do not report commits up to (and including) limit.

Check [git-cherry documentation](https://git-scm.com/docs/git-cherry) for more info.

#### Syntax

<li>git cherry-pick [--edit] [-n] [-m parent-number] [-s] [-x] [--ff]
[-S[key-id]] commit...</li>
- git cherry-pick --continue
- git cherry-pick --quit
- git cherry-pick --abort

#### Parameters

| Parameters | Details                                                                                                                                                                                                                                        |
| ---------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| -e, --edit | With this option, `git cherry-pick` will let you edit the commit message prior to committing.                                                                                                                                                  |
| -x         | When recording the commit, append a line that says "(cherry picked from commit …​)" to the original commit message in order to indicate which commit this change was cherry-picked from. This is done only for cherry picks without conflicts. |
| --ff       | If the current HEAD is the same as the parent of the cherry-pick’ed commit, then a fast forward to this commit will be performed.                                                                                                              |
| --continue | Continue the operation in progress using the information in .git/sequencer. Can be used to continue after resolving conflicts in a failed cherry-pick or revert.                                                                               |
| --quit     | Forget about the current operation in progress. Can be used to clear the sequencer state after a failed cherry-pick or revert.                                                                                                                 |
| --abort    | Cancel the operation and return to the pre-sequence state.                                                                                                                                                                                     |
