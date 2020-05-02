---
metaTitle: "Archive"
description: "Create an archive of git repository, Create an archive of git repository with directory prefix, Create archive of git repository based on specific branch, revision, tag or directory"
---

# Archive



## Create an archive of git repository


With `git archive` it is possible to create compressed archives of a repository, for example for distributing releases.

Create a tar archive of current `HEAD` revision:

```git
git archive --format tar HEAD | cat > archive-HEAD.tar

```

Create a tar archive of current `HEAD` revision with gzip compression:

```git
git archive --format tar HEAD | gzip > archive-HEAD.tar.gz

```

This can also be done with (which will use the in-built tar.gz handling):

```git
git archive --format tar.gz HEAD > archive-HEAD.tar.gz

```

Create a zip archive of current `HEAD` revision:

```git
git archive --format zip HEAD > archive-HEAD.zip

```

Alternatively it is possible to just specify an output file with valid extension and the format and compression type will be inferred from it:

```git
git archive --output=archive-HEAD.tar.gz HEAD

```



## Create an archive of git repository with directory prefix


It is considered good practice to use a prefix when creating git archives, so that extraction will place all files inside a directory. To create an archive of `HEAD` with a directory prefix:

```git
git archive --output=archive-HEAD.zip --prefix=src-directory-name HEAD

```

When extracted all the files will be extracted inside a directory named `src-directory-name` in the current directory.



## Create archive of git repository based on specific branch, revision, tag or directory


It is also possible to create archives of other items than `HEAD`, such as branches, commits, tags, and directories.

To create an archive of a local branch `dev`:

```git
git archive --output=archive-dev.zip --prefix=src-directory-name dev

```

To create an archive of a remote branch `origin/dev`:

```git
git archive --output=archive-dev.zip --prefix=src-directory-name origin/dev

```

To create an archive of a tag `v.01`:

```git
git archive --output=archive-v.01.zip --prefix=src-directory-name v.01

```

Create an archive of files inside a specific sub directory (`sub-dir`) of revision `HEAD`:

```git
git archive zip --output=archive-sub-dir.zip --prefix=src-directory-name HEAD:sub-dir/

```



#### Syntax


- git archive [--format=<fmt>] [--list] [--prefix=<prefix>/] [<extra>] [-o <file> | --output=<file>] [--worktree-attributes] [--remote=<repo> [ --exec=<git-upload-archive>]] <tree-ish> [<path>...]



#### Parameters


|Parameter|Details
|------
|--format=<fmt>|Format of the resulting archive: `tar` or `zip`. If this options is not given and the output file is specified, the format is inferred from the filename if possible. Otherwise, defaults to `tar`.
|-l, --list|Show all available formats.
|-v, --verbose|Report progress to stderr.
|--prefix=<prefix>/|Prepend <prefix>/ to each filename in the archive.
|-o <file>, --output=<file>|Write the archive to <file> instead of stdout.
|--worktree-attributes|Look for attributes in `.gitattributes` files in the working tree.
|<extra>|This can be any options that the archiver backend understands. For `zip` backend, using `-0` will store the files without deflating them, while `-1` through `-9` can be used to adjust compression speed and ratio.
|--remote=<repo>|Retrieve a tar archive from a remote repository `<repo>` rather than the local repository.
|--exec=<git-upload-archive>|Used with `--remote` to specify the path to the `<git-upload-archive` on the remote.
|<tree-ish>|The tree or commit to produce an archive for.
|<path>|Without an optional parameter, all files and directories in the current working directory are included in the archive. If one or more paths are specified, only these are included.

