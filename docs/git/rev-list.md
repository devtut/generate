---
metaTitle: "Git - Rev-List"
description: "List Commits in master but not in origin/master"
---

# Rev-List

## List Commits in master but not in origin/master

```git
git rev-list --oneline master ^origin/master

```

Git rev-list will list commits in one branch that are not in another branch. It is a great tool when you're trying to figure out if code has been merged into a branch or not.

- Using the `--oneline` option will display the title of each commit.
- The `^` operator excludes commits in the specified branch from the list.
- You can pass more than two branches if you want. For example, `git rev-list foo bar ^baz` lists commits in foo and bar, but not baz.

#### Syntax

- git rev-list [options] <commit> ...

#### Parameters

| Parameter | Details                                            |
| --------- | -------------------------------------------------- |
| --oneline | Display commits as a single line with their title. |
