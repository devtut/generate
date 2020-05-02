---
metaTitle: "Staging"
description: "Staging All Changes to Files, Unstage a file that contains changes, Add changes by hunk, Interactive add, Show Staged Changes, Staging A Single File, Stage deleted files"
---

# Staging



## Staging All Changes to Files


```git
git add -A

```

```git
git add .

```

In version 2.x, `git add .` will stage all changes to files in the current directory and all its subdirectories. However, in 1.x it will only stage [new and modified files, not deleted files](http://stackoverflow.com/a/26039014/3345375).

Use `git add -A`, or its equivalent command `git add --all`, to stage all changes to files in any version of git.



## Unstage a file that contains changes


```git
git reset <filePath>

```



## Add changes by hunk


You can see what "hunks" of work would be staged for commit using the patch flag:

```git
git add -p

```

or

```git
git add --patch

```

This opens an interactive prompt that allows you to look at the diffs and let you decide whether you want to include them or not.

```git
Stage this hunk [y,n,q,a,d,/,s,e,?]?

```


- <kbd>y</kbd> stage this hunk for the next commit
- <kbd>n</kbd> do not stage this hunk for the next commit
- <kbd>q</kbd> quit; do not stage this hunk or any of the remaining hunks
- <kbd>a</kbd> stage this hunk and all later hunks in the file
- <kbd>d</kbd> do not stage this hunk or any of the later hunks in the file
- <kbd>g</kbd> select a hunk to go to
- <kbd>/</kbd> search for a hunk matching the given regex
- <kbd>j</kbd> leave this hunk undecided, see next undecided hunk
- <kbd>J</kbd> leave this hunk undecided, see next hunk
- <kbd>k</kbd> leave this hunk undecided, see previous undecided hunk
- <kbd>K</kbd> leave this hunk undecided, see previous hunk
- <kbd>s</kbd> split the current hunk into smaller hunks
- <kbd>e</kbd> manually edit the current hunk
- <kbd>?</kbd> print hunk help

**This makes it easy to catch changes which you do not want to commit.**

You can also open this via `git add --interactive` and selecting `p`.



## Interactive add


`git add -i` (or `--interactive`) will give you an interactive interface where you can edit the index, to prepare what you want to have in the next commit. You can add and remove changes to whole files, add untracked files and remove files from being tracked, but also select subsection of changes to put in the index, by selecting chunks of changes to be added, splitting those chunks, or even editing the diff. Many graphical commit tools for Git (like e.g. `git gui`) include such feature; this might be easier to use than the command line version.

It is very useful (1) if you have entangled changes in the working directory that you want to put in separate commits, and not all in one single commit (2) if you are in the middle of an interactive rebase and want to split too large commit.

```git
$ git add -i
           staged     unstaged path
  1:    unchanged        +4/-4 index.js
  2:        +1/-0      nothing package.json

*** Commands ***
  1: status       2: update       3: revert       4: add untracked
  5: patch        6: diff         7: quit         8: help
What now>

```

The top half of this output shows the current state of the index broken up into staged and unstaged columns:

1. `index.js` has had 4 lines added and 4 lines removed. It is currently not staged, as the current status reports "unchanged." When this file becomes staged, the `+4/-4` bit will be transferred to the staged column and the unstaged column will read "nothing."
1. `package.json` has had one line added and has been staged. There are no further changes since it has been staged as indicated by the "nothing" line under the unstaged column.

The bottom half shows what you can do. Either enter a number (1-8) or a letter (`s`, `u`, `r`, `a`, `p`, `d`, `q`, `h`).

`status` shows output identical to the top part of the output above.

`update` allows you to make further changes to the staged commits with additional syntax.

`revert` will revert the staged commit information back to HEAD.

`add untracked` allows you to add filepaths previously untracked by version control.

`patch` allows for one path to be selected out of an output similar to `status` for further analysis.

`diff` displays what will be committed.

`quit` exits the command.

`help` presents further help on using this command.



## Show Staged Changes


To display the hunks that are staged for commit:

```git
git diff --cached

```



## Staging A Single File


To stage a file for committing, run

```git
git add <filename>

```



## Stage deleted files


```git
git rm filename

```

To delete the file from git without removing it from disk, use the `--cached` flag

```git
git rm --cached filename

```



#### Remarks


It's worth noting that staging has little to do with 'files' themselves and everything to do with the changes within each given file. We stage files that contain changes, and git tracks the changes as commits (even when the changes in a commit are made across several files).

The distinction between files and commits may seem minor, but understanding this difference is fundamental to understanding essential functions like cherry-pick and diff. (See the frustration in [comments regarding the complexity of an accepted answer that proposes cherry-pick as a file management tool](http://stackoverflow.com/questions/449541/how-do-you-merge-selective-files-with-git-merge).)

What's a good place for explaining concepts? Is it in remarks?

Key concepts:

A files is the more common metaphor of the two in information technology. Best practice dictates that a filename not change as its contents change (with a few recognized exceptions).

A commit is a metaphor that is unique to source code management. Commits are changes related to a specific effort, like a bug fix. Commits often involve several files. A single, minor bug fix may involve tweaks to templates and css in unique files. As the change is described, developed, documented, reviewed and deployed, the changes across the separate files can be annotated and handled as a single unit. The single unit in this case is the commit. Equally important, focusing just on the commit during a review allows the unchanged lines of code in the various affected files to be ignored safely.

