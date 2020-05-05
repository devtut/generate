---
metaTitle: "Git - Git Patch"
description: "Creating a patch, Applying patches"
---

# Git Patch

## Creating a patch

To create a patch, there are two steps.

1. Make your changes and commit them.
1. Run `git format-patch <commit-reference>` to convert all commits since the commit <commit-reference> (not including it) into patch files.

For example, if patches should be generated from the latest two commits:

```git
git format-patch HEAD~~

```

This will create 2 files, one for each commit since `HEAD~~`, like this:

```git
0001-hello_world.patch
0002-beginning.patch

```

## Applying patches

We can use `git apply some.patch` to have the changes from the `.patch` file applied to your current working directory. They will be unstaged and need to be committed.

To apply a patch as a commit (with its commit message), use

```git
git am some.patch

```

To apply all patch files to the tree:

```git
git am *.patch

```

#### Syntax

<li>git am [--signoff] [--keep] [--[no-]keep-cr] [--[no-]utf8]
[--3way] [--interactive] [--committer-date-is-author-date]
[--ignore-date] [--ignore-space-change | --ignore-whitespace]
[--whitespace=<option>] [-C<n>] [-p<n>] [--directory=<dir>]
[--exclude=<path>] [--include=<path>] [--reject] [-q | --quiet]
[--[no-]scissors] [-S[<keyid>]] [--patch-format=<format>]
[(<mbox> | <Maildir>)...]</li>
- git am (--continue | --skip | --abort)

#### Parameters

| Parameter                                                                                                                                                       | Details                                                                                                                                                                                                                                                                                    |
| --------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| (<mbox>                                                                                                                                                         | <Maildir>)...                                                                                                                                                                                                                                                                              | The list of mailbox files to read patches from. If you do not supply this argument, the command reads from the standard input. If you supply directories, they will be treated as Maildirs. |
| -s, --signoff                                                                                                                                                   | Add a Signed-off-by: line to the commit message, using the committer identity of yourself.                                                                                                                                                                                                 |
| -q, --quiet                                                                                                                                                     | Be quiet. Only print error messages.                                                                                                                                                                                                                                                       |
| -u, --utf8                                                                                                                                                      | Pass `-u` flag to `git mailinfo`. The proposed commit log message taken from the e-mail is re-coded into UTF-8 encoding (configuration variable `i18n.commitencoding` can be used to specify project’s preferred encoding if it is not UTF-8). You can use `--no-utf8` to override this.   |
| --no-utf8                                                                                                                                                       | Pass -n flag to git mailinfo.                                                                                                                                                                                                                                                              |
| -3, --3way                                                                                                                                                      | When the patch does not apply cleanly, fall back on 3-way merge if the patch records the identity of blobs it is supposed to apply to and we have those blobs available locally.                                                                                                           |
| --ignore-date, --ignore-space-change, --ignore-whitespace, --whitespace=<option>, -C<n>, -p<n>, --directory=<dir>, --exclude=<path>, --include=<path>, --reject | These flags are passed to the git apply program that applies the patch.                                                                                                                                                                                                                    |
| --patch-format                                                                                                                                                  | By default the command will try to detect the patch format automatically. This option allows the user to bypass the automatic detection and specify the patch format that the patch(es) should be interpreted as. Valid formats are `mbox`, `stgit`, `stgit-series`, and `hg`.             |
| -i, --interactive                                                                                                                                               | Run interactively.                                                                                                                                                                                                                                                                         |
| --committer-date-is-author-date                                                                                                                                 | By default the command records the date from the e-mail message as the commit author date, and uses the time of commit creation as the committer date. This allows the user to lie about the committer date by using the same value as the author date.                                    |
| --ignore-date                                                                                                                                                   | By default the command records the date from the e-mail message as the commit author date, and uses the time of commit creation as the committer date. This allows the user to lie about the author date by using the same value as the committer date.                                    |
| --skip                                                                                                                                                          | Skip the current patch. This is only meaningful when restarting an aborted patch.                                                                                                                                                                                                          |
| -S[<keyid>], --gpg-sign[=<keyid>]                                                                                                                               | GPG-sign commits.                                                                                                                                                                                                                                                                          |
| --continue, -r, --resolved                                                                                                                                      | After a patch failure (e.g. attempting to apply conflicting patch), the user has applied it by hand and the index file stores the result of the application. Make a commit using the authorship and commit log extracted from the e-mail message and the current index file, and continue. |
| --resolvemsg=<msg>                                                                                                                                              | When a patch failure occurs, `<msg>` will be printed to the screen before exiting. This overrides the standard message informing you to use `--continue` or `--skip` to handle the failure. This is solely for internal use between `git rebase` and `git am`.                             |
| --abort                                                                                                                                                         | Restore the original branch and abort the patching operation.                                                                                                                                                                                                                              |
