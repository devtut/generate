---
metaTitle: "Git statistics"
description: "Lines of code per developer, Listing each branch and its last revision's date, Commits per developer, Commits per date, Total number of commits in a  branch , List all commits in pretty format, Find All Local Git Repositories on Computer, Show the total number of commits per author"
---

# Git statistics

## Lines of code per developer

```git
git ls-tree -r HEAD | sed -Ee 's/^.{53}//' | \
while read filename; do file "$filename"; done | \
grep -E ': .*text' | sed -E -e 's/: .*//' | \
while read filename; do git blame --line-porcelain "$filename"; done | \
sed -n 's/^author //p' | \
sort | uniq -c | sort -rn

```

## Listing each branch and its last revision's date

```git
for k in `git branch -a | sed s/^..//`; do echo -e `git log -1 --pretty=format:"%Cgreen%ci %Cblue%cr%Creset" $k --`\\t"$k";done | sort

```

## Commits per developer

Git `shortlog` is used to summarize the git log outputs and group the commits by author.

By default, all commit messages are shown but argument `--summary` or `-s` skips the messages and gives a list of authors with their total number of commits.

`--numbered` or `-n` changes the ordering from alphabetical (by author ascending) to number of commits descending.

```git
git shortlog -sn        #Names and Number of commits

git shortlog -sne       #Names along with their email ids and the Number of commits

```

or

```git
git log --pretty=format:%ae \
| gawk -- '{ ++c[$0]; } END { for(cc in c) printf "%5d %s\n",c[cc],cc; }'

```

**Note:** Commits by the same person may not be grouped together where their name and/or email address has been spelled differently. For example `John Doe` and `Johnny Doe` will appear separately in the list. To resolve this, refer to the `.mailmap` feature.

## Commits per date

```git
git log --pretty=format:"%ai" | awk '{print " : "$1}' | sort -r | uniq -c

```

## Total number of commits in a branch

```git
git log  --pretty=oneline |wc -l

```

## List all commits in pretty format

```git
git log --pretty=format:"%Cgreen%ci %Cblue%cn  %Cgreen%cr%Creset %s"

```

This will give a nice overview of all commits (1 per line) with date, user and commit message.

The `--pretty` option has many placeholders, each starting with `%`. All options can be found [here ](https://git-scm.com/docs/pretty-formats)

## Find All Local Git Repositories on Computer

To list all the git repository locations on your you can run the following

```git
find $HOME -type d -name ".git"

```

Assuming you have `locate`, this should be much faster:

```git
locate .git |grep git$

```

If you have `gnu locate` or `mlocate`, this will select only the git dirs:

```git
locate -ber \\.git$

```

## Show the total number of commits per author

In order to get the total number of commits that each developer or contributor has made on a repository, you can simply use the `git shortlog`:

```git
git shortlog -s

```

which provides the author names and number of commits by each one.

Additionally, if you want to have the results calculated on all branches, add `--all` flag to the command:

```git
git shortlog -s --all

```

#### Syntax

- git log [<options>][<revision range>] [[--] <path>]
- git log --pretty=short | git shortlog [<options>]
- git shortlog [<options>][<revision range>] [[--] <path>]

#### Parameters

| Parameter                             | Details                                                                                                                                                                              |
| ------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `-n`, `--numbered`                    | Sort output according to the number of commits per author instead of alphabetic order                                                                                                |
| `-s`, `--summary`                     | Only provide a commit count summary                                                                                                                                                  |
| `-e`, `--email`                       | Show the email address of each author                                                                                                                                                |
| `--format`[=<format>]                 | Instead of the commit subject, use some other information to describe each commit. <format> can be any string accepted by the `--format` option of `git log`.                        |
| `-w`[<width>[,<indent1>[,<indent2>]]] | Linewrap the output by wrapping each line at `width`. The first line of each entry is indented by `indent1` number of spaces, and subsequent lines are indented by `indent2` spaces. |
| <revision range>                      | Show only commits in the specified revision range. Default to the whole history until the current commit.                                                                            |
| [`--`] <path>                         | Show only commits that explain how the files matching `path` came to be. Paths may need to be prefixed with "-- " to separate them from options or the revision range.               |
