---
metaTitle: "Renaming"
description: "Rename Folders, Renaming a local branch, rename a local and the remote branch"
---

# Renaming

## Rename Folders

To rename a folder from `oldName` to `newName`

```git
git mv directoryToFolder/oldName directoryToFolder/newName

```

Followed by `git commit` and/or `git push`

If this error occurs:

> fatal: renaming 'directoryToFolder/oldName' failed: Invalid argument

Use the following command:

```git
git mv directoryToFolder/oldName temp && git mv temp directoryToFolder/newName

```

## Renaming a local branch

You can rename branch in local repository using this command:

```git
git branch -m old_name new_name

```

## rename a local and the remote branch

the easiest way is to have the local branch checked out:

```git
git checkout old_branch

```

then rename the local branch, delete the old remote and set the new renamed branch as upstream:

```git
git branch -m new_branch
git push origin :old_branch
git push --set-upstream origin new_branch

```

#### Syntax

- `git mv <source> <destination>`
- `git mv -f <source> <destination>`

#### Parameters

| Parameter         | Details                                                      |
| ----------------- | ------------------------------------------------------------ |
| `-f` or `--force` | Force renaming or moving of a file even if the target exists |
