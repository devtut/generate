---
metaTitle: "diff-tree"
description: "See the files changed in a specific commit, Usage, Common diff options"
---

# diff-tree

Compares the content and mode of blobs found via two tree objects.

## See the files changed in a specific commit

## Usage

```git
git diff-tree [--stdin] [-m] [-c] [--cc] [-s] [-v] [--pretty] [-t] [-r] [--root] [<common-diff-options>] <tree-ish> [<tree-ish>] [<path>...]

```

| **Option** | **Explanation**                                      |
| ---------- | ---------------------------------------------------- |
| -r         | diff recursively                                     |
| --root     | include the initial commit as diff against /dev/null |

## Common diff options

| **Option**           | **Explanation**                                           |
| -------------------- | --------------------------------------------------------- |
| -z                   | output diff-raw with lines terminated with NUL.           |
| -p                   | output patch format.                                      |
| -u                   | synonym for -p.                                           |
| --patch-with-raw     | output both a patch and the diff-raw format.              |
| --stat               | show diffstat instead of patch.                           |
| --numstat            | show numeric diffstat instead of patch.                   |
| --patch-with-stat    | output a patch and prepend its diffstat.                  |
| --name-only          | show only names of changed files.                         |
| --name-status        | show names and status of changed files.                   |
| --full-index         | show full object name on index lines.                     |
| --abbrev=<n>         | abbreviate object names in diff-tree header and diff-raw. |
| -R                   | swap input file pairs.                                    |
| -B                   | detect complete rewrites.                                 |
| -M                   | detect renames.                                           |
| -C                   | detect copies.                                            |
| --find-copies-harder | try unchanged files as candidate for copy detection.      |
| -l<n>                | limit rename attempts up to paths.                        |
| -O                   | reorder diffs according to the .                          |
| -S                   | find filepair whose only one side contains the string.    |
| --pickaxe-all        | show all files diff when -S is used and hit is found.     |
| -a --text            | treat all files as text.                                  |
