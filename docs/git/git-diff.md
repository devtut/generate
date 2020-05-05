---
metaTitle: "Git Diff"
description: "Show differences in working branch, Show changes between two commits, Show differences for staged files, Comparing branches, Show both staged and unstaged changes, Show differences for a specific file or directory, Viewing a word-diff for long lines, Using meld to see all modifications in the working directory, Show differences between current version and last version, Produce a patch-compatible diff, difference between two commit or branch, Diff UTF-16 encoded text and binary plist files, Viewing a three-way merge including the common ancestor, Show changes between two branches"
---

# Git Diff

## Show differences in working branch

```git
git diff

```

This will show the [**unstaged**](http://stackoverflow.com/documentation/git/244/staging) changes on the current branch from the commit before it. It will only show changes relative to the index, meaning it shows what you **could** add to the next commit, but haven't. To add (stage) these changes, you can use `git add`.

If a file is staged, but was modified after it was staged, `git diff` will show the differences between the current file and the staged version.

## Show changes between two commits

```git
git diff 1234abc..6789def    # old   new

```

E.g.: Show the changes made in the last 3 commits:

```git
git diff @~3..@    # HEAD -3   HEAD

```

Note: the two dots (..) is optional, but adds clarity.

This will show the textual difference between the commits, regardless of where they are in the tree.

## Show differences for staged files

```git
git diff --staged

```

This will show the changes between the previous commit and the currently staged files.

**NOTE:** You can also use the following commands to accomplish the same thing:

```git
git diff --cached

```

Which is just a synonym for `--staged` or

```git
git status -v

```

Which will trigger the verbose settings of the `status` command.

## Comparing branches

Show the changes between the tip of **`new`** and the tip of **`original`**:

```git
git diff original new     # equivalent to original..new

```

Show all changes on **`new`** since it branched from **`original`**:

```git
git diff original...new     # equivalent to $(git merge-base original new)..new

```

Using only one parameter such as

git diff original

is equivalent to

git diff original..HEAD

## Show both staged and unstaged changes

To show all staged **and** unstaged changes, use:

```git
git diff HEAD

```

**NOTE:** You can also use the following command:

```git
git status -vv

```

The difference being that the output of the latter will actually tell you which changes are staged for commit and which are not.

## Show differences for a specific file or directory

```git
git diff myfile.txt

```

Shows the changes between the previous commit of the specified file (`myfile.txt`) and the locally-modified version that has not yet been staged.

This also works for directories:

```git
git diff documentation

```

The above shows the changes between the previous commit of all files in the specified directory (`documentation/`) and the locally-modified versions of these files, that have not yet been staged.

To show the difference between some version of a file in a given commit and the local `HEAD` version you can specify the commit you want to compare against:

```git
git diff 27fa75e myfile.txt

```

Or if you want to see the version between two separate commits:

```git
git diff 27fa75e ada9b57 myfile.txt

```

To show the difference between the version specified by the hash `ada9b57` and the latest commit on the branch `my_branchname` for only the relative directory called `my_changed_directory/` you can do this:

```git
git diff ada9b57 my_branchname my_changed_directory/

```

## Viewing a word-diff for long lines

```git
git diff [HEAD|--staged...] --word-diff

```

Rather than displaying lines changed, this will display differences within lines. For example, rather than:

```git
-Hello world
+Hello world!

```

Where the whole line is marked as changed, `word-diff` alters the output to:

```git
Hello [-world-]{+world!+}

```

You can omit the markers `[-`, `-]`, `{+`, `+}` by specifying `--word-diff=color` or `--color-words`. This will only use color coding to mark the difference:

[<img src="http://i.stack.imgur.com/1vsUP.png" alt="Example of using git diff --color-words" />](http://i.stack.imgur.com/1vsUP.png)

## Using meld to see all modifications in the working directory

```git
git difftool -t meld --dir-diff

```

will show the working directory changes. Alternatively,

```git
git difftool -t meld --dir-diff  [COMMIT_A] [COMMIT_B]

```

will show the differences between 2 specific commits.

## Show differences between current version and last version

```git
git diff HEAD^ HEAD

```

This will show the changes between the previous commit and the current commit.

## Produce a patch-compatible diff

Sometimes you just need a diff to apply using patch. The regular `git --diff` does not work. Try this instead:

```git
git diff --no-prefix > some_file.patch

```

Then somewhere else you can reverse it:

```git
patch -p0 < some_file.patch

```

## difference between two commit or branch

To view difference between two branch

```git
git diff <branch1>..<branch2>

```

To view difference between two branch

```git
git diff <commitId1>..<commitId2>

```

To view diff with current branch

```git
git diff <branch/commitId>

```

To view summary of changes

```git
git diff --stat <branch/commitId>

```

To view files that changed after a certain commit

```git
git diff --name-only <commitId>

```

To view files that are different than a branch

```git
git diff --name-only <branchName>

```

To view files that changed in a folder after a certain commit

```git
git diff --name-only <commitId> <folder_path>

```

## Diff UTF-16 encoded text and binary plist files

You can diff UTF-16 encoded files (localization strings file os iOS and macOS are examples) by specifying how git should diff these files.

Add the following to your `~/.gitconfig` file.

```git
[diff "utf16"]
textconv = "iconv -f utf-16 -t utf-8"

```

`iconv` is a program to [convert different encodings](http://linux.die.net/man/1/iconv).

Then edit or create a `.gitattributes` file in the root of the repository where you want to use it. Or just edit `~/.gitattributes`.

```git
*.strings diff=utf16

```

This will convert all files ending in `.strings` before git diffs.

You can do similar things for other files, that can be converted to text.

For binary plist files you edit `.gitconfig`

```git
[diff "plist"]
textconv = plutil -convert xml1 -o -

```

and `.gitattributes`

```git
*.plist diff=plist

```

## Viewing a three-way merge including the common ancestor

```git
git config --global merge.conflictstyle diff3

```

Sets the `diff3` style as default: instead of the usual format in conflicted sections, showing the two files:

```git
<<<<<<< HEAD
left
=======
right
>>>>>>> master

```

it will include an additional section containing the original text (coming form the common ancestor):

```git
<<<<<<< HEAD
first
second
|||||||
first
=======
last
>>>>>>> master

```

This format makes it easier to understand merge-conflict, ie. in this case locally `second` has been added, while remote changed `first` to `last`, resolving to:

```git
last
second

```

The same resolution would have been much harder using the default:

```git
<<<<<<< HEAD
first
second
=======
last
>>>>>>> master

```

## Show changes between two branches

```git
git diff branch1..branch2

```

#### Syntax

- `git diff [options] [<commit>] [--] [<path>…​]`
- `git diff [options] --cached [<commit>] [--] [<path>…​]`
- `git diff [options] <commit> <commit> [--] [<path>…​]`
- `git diff [options] <blob> <blob>`
- `git diff [options] [--no-index] [--] <path> <path>`

#### Parameters

| Parameter         | Details                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| ----------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| -p, -u, --patch   | Generate patch                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| -s, --no-patch    | Suppress diff output. Useful for commands like `git show` that show the patch by default, or to cancel the effect of `--patch`                                                                                                                                                                                                                                                                                                                                                 |
| --raw             | Generate the diff in raw format                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| --diff-algorithm= | Choose a diff algorithm. The variants are as follows: `myers`, `minimal`, `patience`, `histogram`                                                                                                                                                                                                                                                                                                                                                                              |
| --summary         | Output a condensed summary of extended header information such as creations, renames and mode changes                                                                                                                                                                                                                                                                                                                                                                          |
| --name-only       | Show only names of changed files                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| --name-status     | Show names and statuses of changed files The most common statuses are M (Modified), A (Added), and D (Deleted)                                                                                                                                                                                                                                                                                                                                                                 |
| --check           | Warn if changes introduce conflict markers or whitespace errors. What are considered whitespace errors is controlled by `core.whitespace` configuration. By default, trailing whitespaces (including lines that solely consist of whitespaces) and a space character that is immediately followed by a tab character inside the initial indent of the line are considered whitespace errors. Exits with non-zero status if problems are found. Not compatible with --exit-code |
| --full-index      | Instead of the first handful of characters, show the full pre- and post-image blob object names on the "index" line when generating patch format output                                                                                                                                                                                                                                                                                                                        |
| --binary          | In addition to `--full-index`, output a binary diff that can be applied with `git apply`                                                                                                                                                                                                                                                                                                                                                                                       |
| -a, --text        | Treat all files as text.                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
| --color           | Set the color mode; i.e. use `--color=always` if you would like to pipe a diff to less and keep git's coloring                                                                                                                                                                                                                                                                                                                                                                 |
