---
metaTitle: "Git - Merging"
description: "Merge one branch into another, Automatic Merging, Aborting a merge, Keep changes from only one side of a merge, Merge with a commit, Finding all branches with no merged changes"
---

# Merging

## Merge one branch into another

```git
git merge incomingBranch

```

This merges the branch `incomingBranch` into the branch you are currently in. For example, if you are currently in `master`, then `incomingBranch` will be merged into `master`.

Merging can create conflicts in some cases. If this happens, you will see the message `Automatic merge failed; fix conflicts and then commit the result.` You will need to manually edit the conflicted files, or to undo your merge attempt, run:

```git
git merge --abort

```

## Automatic Merging

When the commits on two branches don't conflict, Git can automatically merge them:

```git
~/Stack Overflow(branch:master) » git merge another_branch
Auto-merging file_a
Merge made by the 'recursive' strategy.
 file_a | 2 +-
 1 file changed, 1 insertion(+), 1 deletion(-)

```

## Aborting a merge

After starting a merge, you might want to stop the merge and return everything to its pre-merge state. Use `--abort`:

```git
git merge --abort

```

## Keep changes from only one side of a merge

During a merge, you can pass `--ours` or `--theirs` to `git checkout` to take all changes for a file from one side or the other of a merge.

```git
$ git checkout --ours   -- file1.txt # Use our version of file1, delete all their changes
$ git checkout --theirs -- file2.txt # Use their version of file2, delete all our changes

```

## Merge with a commit

Default behaviour is when the merge resolves as a fast-forward, only update the branch pointer, without creating a merge commit. Use `--no-ff` to resolve.

`git merge <branch_name> --no-ff -m "<commit message>"`

## Finding all branches with no merged changes

Sometimes you might have branches lying around that have already had their changes merged into master. This finds all branches that are not `master` that have no unique commits as compared to `master`. This is very useful for finding branches that were not deleted after the PR was merged into master.

```

for branch in $(git branch -r) ; do
   [ "${branch}" != "origin/master" ] && [ $(git diff master...${branch} | wc -l) -eq 0 ] && echo -e `git show --pretty=format:"%ci %cr" $branch | head -n 1`\\t$branch
 done | sort -r

```

#### Syntax

- git merge **another_branch** [options]
- git merge **--abort**

#### Parameters

| Parameter        | Details                                                                  |
| ---------------- | ------------------------------------------------------------------------ |
| `-m`             | Message to be included in the merge commit                               |
| `-v`             | Show verbose output                                                      |
| `--abort`        | Attempt to revert all files back to their state                          |
| `--ff-only`      | Aborts instantly when a merge-commit would be required                   |
| `--no-ff`        | Forces creation of a merge-commit, even if it wasn't mandatory           |
| `--no-commit`    | Pretends the merge failed to allow inspection and tweaking of the result |
| `--stat`         | Show a diffstat after merge completion                                   |
| `-n`/`--no-stat` | Don't show the diffstat                                                  |
| `--squash`       | Allows for a single commit on the current branch with the merged changes |
