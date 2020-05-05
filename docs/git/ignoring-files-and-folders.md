---
metaTitle: "Git - Ignoring Files and Folders"
description: "Ignoring files and directories with a .gitignore file, Checking if a file is ignored, Exceptions in a .gitignore file, A global .gitignore file, Ignore files that have already been committed to a Git repository, Ignore files locally without committing ignore rules, Ignoring a file in any directory, Ignoring subsequent changes to a file (without removing it), Ignoring files in subfolders (Multiple gitignore files), Prefilled .gitignore Templates, Create an Empty Folder, Finding files ignored by .gitignore, Ignoring only part of a file [stub], Ignoring changes in tracked files. [stub], Clear already committed files, but included in .gitignore"
---

# Ignoring Files and Folders


This topic illustrates how to avoid adding unwanted files (or file changes) in a Git repo. There are several ways (global or local `.gitignore`, `.git/exclude`, `git update-index --assume-unchanged`, and `git update-index --skip-tree`), but keep in mind Git is managing **content**, which means: ignoring actually ignores a folder **content** (i.e. files). An empty folder would be ignored by default, since it cannot be added anyway.



## Ignoring files and directories with a .gitignore file


You can make Git ignore certain files and directories — that is, exclude them from being tracked by Git — by creating one or more [`.gitignore`](https://git-scm.com/docs/gitignore) files in your repository.

In software projects, `.gitignore` typically contains a listing of files and/or directories that are generated during the build process or at runtime.  Entries in the `.gitignore` file may include names or paths pointing to:

1. temporary resources e.g. caches, log files, compiled code, etc.
1. local configuration files that should not be shared with other developers
1. files containing secret information, such as login passwords, keys and credentials

When created in the top level directory, the rules will apply recursively to all files and sub-directories throughout the entire repository. When created in a sub-directory, the rules will apply to that specific directory and its sub-directories.

When a file or directory is ignored, it will not be:

1. tracked by Git
1. reported by commands such as `git status` or `git diff`
1. staged with commands such as `git add -A`

In the unusual case that you need to ignore tracked files, special care should be taken. See: [Ignore files that have already been committed to a Git repository](http://stackoverflow.com/documentation/git/245/using-a-gitignore-file/1777/ignore-files-that-have-already-been-committed-to-a-git-repository#t=201607211343212958337).

### Examples

Here are some generic examples of rules in a `.gitignore` file, based on [glob file patterns](https://en.wikipedia.org/wiki/Glob_(programming)):

```git
# Lines starting with `#` are comments.

# Ignore files called 'file.ext'
file.ext

# Comments can't be on the same line as rules!
# The following line ignores files called 'file.ext # not a comment'
file.ext # not a comment 

# Ignoring files with full path.
# This matches files in the root directory and subdirectories too.
# i.e. otherfile.ext will be ignored anywhere on the tree.
dir/otherdir/file.ext
otherfile.ext

# Ignoring directories
# Both the directory itself and its contents will be ignored.
bin/
gen/

# Glob pattern can also be used here to ignore paths with certain characters.
# For example, the below rule will match both build/ and Build/
[bB]uild/

# Without the trailing slash, the rule will match a file and/or
# a directory, so the following would ignore both a file named `gen`
# and a directory named `gen`, as well as any contents of that directory
bin
gen

# Ignoring files by extension
# All files with these extensions will be ignored in
# this directory and all its sub-directories.
*.apk
*.class

# It's possible to combine both forms to ignore files with certain
# extensions in certain directories. The following rules would be
# redundant with generic rules defined above.
java/*.apk
gen/*.class

# To ignore files only at the top level directory, but not in its
# subdirectories, prefix the rule with a `/`
/*.apk
/*.class

# To ignore any directories named DirectoryA 
# in any depth use ** before DirectoryA
# Do not forget the last /, 
# Otherwise it will ignore all files named DirectoryA, rather than directories
**/DirectoryA/
# This would ignore 
# DirectoryA/
# DirectoryB/DirectoryA/ 
# DirectoryC/DirectoryB/DirectoryA/
# It would not ignore a file named DirectoryA, at any level

# To ignore any directory named DirectoryB within a 
# directory named DirectoryA with any number of 
# directories in between, use ** between the directories
DirectoryA/**/DirectoryB/
# This would ignore 
# DirectoryA/DirectoryB/ 
# DirectoryA/DirectoryQ/DirectoryB/ 
# DirectoryA/DirectoryQ/DirectoryW/DirectoryB/

# To ignore a set of files, wildcards can be used, as can be seen above.
# A sole '*' will ignore everything in your folder, including your .gitignore file.
# To exclude specific files when using wildcards, negate them.
# So they are excluded from the ignore list:
!.gitignore 

# Use the backslash as escape character to ignore files with a hash (#)
# (supported since 1.6.2.1)
\#*#

```

Most `.gitignore` files are standard across various languages, so to get started, here is set of [sample `.gitignore` files](https://github.com/github/gitignore) listed by language from which to clone or copy/modify into your project.  Alternatively, for a fresh project you may consider auto-generating a starter file using an [online tool](https://www.gitignore.io/).

### Other forms of .gitignore

`.gitignore` files are intended to be committed as part of the repository. If you want to ignore certain files without committing the ignore rules, here are some options:

- Edit the `.git/info/exclude` file (using the same syntax as `.gitignore`). The rules will be global in the scope of the repository;
- Set up [a global gitignore file](http://stackoverflow.com/documentation/git/245/ignoring-files-and-folders/1222/a-global-gitignore-file) that will apply ignore rules to all your local repositories:

Furthermore, you can ignore local changes to tracked files without changing the global git configuration with:

- `git update-index --skip-worktree [<file>...]`:  for minor local modifications
- `git update-index --assume-unchanged [<file>...]`: for production ready, non-changing files upstream

See [more details on differences between the latter flags](http://stackoverflow.com/a/13631525/4531270) and the [`git update-index` documentation](https://git-scm.com/docs/git-update-index) for further options.

### Cleaning up ignored files

You can use `git clean -X` to cleanup ignored files:

```git
git clean -Xn #display a list of ignored files
git clean -Xf #remove the previously displayed files

```

Note: `-X` (caps) cleans up **only** ignored files. Use `-x` (no caps) to also remove untracked files.

See [the `git clean` documentation](https://stackoverflow.com/documentation/git/1254/git-clean) for more details.

See [the Git manual](https://git-scm.com/docs/gitignore) for more details.



## Checking if a file is ignored


The [`git check-ignore`](https://git-scm.com/docs/git-check-ignore) command reports on files ignored by Git.

You can pass filenames on the command line, and `git check-ignore` will list the filenames that are ignored. For example:

```git
$ cat .gitignore
*.o
$ git check-ignore example.o Readme.md
example.o

```

Here, only *.o files are defined in .gitignore, so Readme.md is not listed in the output of `git check-ignore`.

If you want to see line of which .gitignore is responsible for ignoring a file, add -v to the git check-ignore command:

```git
$ git check-ignore -v example.o Readme.md
.gitignore:1:*.o        example.o

```

From Git 1.7.6 onwards you can also use `git status --ignored` in order to see ignored files. You can find more info on this in the [official documentation](https://git-scm.com/docs/git-status) or in [Finding files ignored by .gitignore](http://stackoverflow.com/documentation/git/245/ignoring-files-and-folders/19445/finding-files-ignored-by-gitignore#t=201611211142389191756).



## Exceptions in a .gitignore file


If you ignore files by using a pattern but have exceptions, prefix an exclamation mark(!) to the exception. For example:

```git
*.txt
!important.txt

```

The above example instructs Git to ignore all files with the `.txt` extension except for files named `important.txt`.

If the file is in an ignored folder, you can **NOT** re-include it so easily:

```git
folder/
!folder/*.txt

```

In this example all .txt files in the folder would remain ignored.

The right way is re-include the folder itself on a separate line, then ignore all files in `folder` by `*`, finally re-include the `*.txt` in `folder`, as the following:

```git
!folder/
folder/*
!folder/*.txt

```

**Note**: For file names beginning with an exclamation mark, add two exclamation marks or escape with the `\` character:

```git
!!includethis
\!excludethis

```



## A global .gitignore file


To have Git ignore certain files across all repositories you can [create a global .gitignore](https://help.github.com/articles/ignoring-files/#create-a-global-gitignore) with the following command in your terminal or command prompt:

```git
$ git config --global core.excludesfile <Path_To_Global_gitignore_file>

```

Git will now use this in addition to each repository's own [.gitignore](https://git-scm.com/docs/gitignore) file. Rules for this are:

- If the local `.gitignore` file explicitly includes a file while the global `.gitignore` ignores it, the local `.gitignore` takes priority (the file will be included)
- If the repository is cloned on multiple machines, then the global `.gigignore` must be loaded on all machines or at least include it, as the ignored files will be pushed up to the repo while the PC with the global `.gitignore` wouldn't update it. This is why a repo specific `.gitignore` is a better idea than a global one if the project is worked on by a team

This file is a good place to keep platform, machine or user specific ignores, e.g. OSX `.DS_Store`, Windows `Thumbs.db` or Vim `*.ext~` and `*.ext.swp` ignores if you don't want to keep those in the repository. So one team member working on OS X can add all `.DS_STORE` and `_MACOSX` (which is actually useless), while another team member on Windows can ignore all `thumbs.bd`



## Ignore files that have already been committed to a Git repository


If you have already added a file to your Git repository and now want to **stop tracking it** (so that it won't be present in future commits), you can remove it from the index:

```git
git rm --cached <file>

```

This will remove the file from the repository and prevent further changes from being tracked by Git. The `--cached` option will make sure that the file is not physically deleted.

Note that previously added contents of the file will still be visible via the Git history.

Keep in mind that if anyone else pulls from the repository after you removed the file from the index, **their copy will be physically deleted**.

You can make Git pretend that the working directory version of the file is up to date and read the index version instead (thus ignoring changes in it) with "[skip worktree](https://www.kernel.org/pub/software/scm/git/docs/git-update-index.html#_skip_worktree_bit)" bit:

```git
git update-index --skip-worktree <file>

```

Writing is not affected by this bit, content safety is still first priority. You will never lose your precious ignored changes; on the other hand this bit conflicts with stashing: to remove this bit, use

```git
git update-index --no-skip-worktree <file>

```

It is sometimes ****wrongly**** recommended to lie to Git and have it assume that file is unchanged without examining it.  It looks at first glance as ignoring any further changes to the file, without removing it from its index:

```git
git update-index --assume-unchanged <file>

```

This will force git to ignore any change made in the file (keep in mind that if you pull any changes to this file, or you stash it, **your ignored changes will be lost**)

If you want git to "care" about this file again, run the following command:

```git
git update-index --no-assume-unchanged <file>

```



## Ignore files locally without committing ignore rules


`.gitignore` ignores files locally, but it is intended to be committed to the repository and shared with other contributors and users. You can set a global `.gitignore`, but then all your repositories would share those settings.

If you want to ignore certain files in a repository locally and not make the file part of any repository, edit `.git/info/exclude` inside your repository.

For example:

```git
# these files are only ignored on this repo
# these rules are not shared with anyone
# as they are personal                                              
gtk_tests.py
gui/gtk/tests/*
localhost
pushReports.py
server/

```



## Ignoring a file in any directory


To ignore a file `foo.txt` in **any** directory you should just write its name:

```git
foo.txt # matches all files 'foo.txt' in any directory

```

If you want to ignore the file only in part of the tree, you can specify the subdirectories of a specific directory with `**` pattern:

```git
bar/**/foo.txt # matches all files 'foo.txt' in 'bar' and all subdirectories

```

Or you can create a `.gitignore` file in the `bar/` directory. Equivalent to the previous example would be creating file `bar/.gitignore` with these contents:

```git
foo.txt # matches all files 'foo.txt' in any directory under bar/

```



## Ignoring subsequent changes to a file (without removing it)


Sometimes you want to have a file held in Git but ignore subsequent changes.

Tell Git to ignore changes to a file or directory using `update-index`:

The above command instructs Git to assume `my-file.txt` hasn't been changed, and not to check or report changes. The file is still present in the repository.

This can be useful for providing defaults and allowing local environment overrides, e.g.:



## Ignoring files in subfolders (Multiple gitignore files)


Suppose you have a repository structure like this:

```git
examples/
    output.log
src/
    <files not shown>
    output.log
README.md

```

`output.log` in the examples directory is valid and required for the project to gather an understanding while the one beneath `src/` is created while debugging and should not be in the history or part of the repository.

There are two ways to ignore this file. You can place an absolute path into the `.gitignore` file at the root of the working directory:

```git
# /.gitignore
src/output.log

```

Alternatively, you can create a `.gitignore` file in the `src/` directory and ignore the file that is relative to this `.gitignore`:

```git
# /src/.gitignore
output.log

```



## Prefilled .gitignore Templates


If you are unsure which rules to list in your `.gitignore` file, or you just want to add generally accepted exceptions to your project, you can choose or generate a `.gitignore` file:

- [https://www.gitignore.io/](https://www.gitignore.io/)
- [https://github.com/github/gitignore](https://github.com/github/gitignore)

Many hosting services such as GitHub and BitBucket offer the ability to generate `.gitignore` files based upon the programming languages and IDEs you may be using:

[<img src="http://i.stack.imgur.com/WfT5z.png" alt="GitHub .gitignore dropdown" />](http://i.stack.imgur.com/WfT5z.png)



## Create an Empty Folder


It is not possible to add and commit an empty folder in Git due to the fact that Git manages *files* and attaches their directory to them, which slims down commits and improves speed. To get around this, there are two methods:

Method one: `.gitkeep`

One hack to get around this is to use a `.gitkeep` file to register the folder for Git. To do this, just create the required directory and add a `.gitkeep` file to the folder. This file is blank and doesn't serve any purpose other than to just register the folder. To do this in Windows (which has awkward file naming conventions) just open git bash in the directory and run the command:

> 
$ touch .gitkeep


This command just makes a blank `.gitkeep` file in the current directory

Method two: `dummy.txt`

Another hack for this is very similar to the above and the same steps can be followed, but instead of a `.gitkeep`, just use a `dummy.txt` instead. This has the added bonus of being able to easily create it in Windows using the context menu. And you get to leave funny messages in them too.You can also use `.gitkeep` file to track the empty directory. `.gitkeep` normally is an empty file that is added to track the empty directoy.



## Finding files ignored by .gitignore


You can list all files ignored by git in current directory with command:

```git
git status --ignored

```

So if we have repository structure like this:

```git
.git
.gitignore
./example_1
./dir/example_2
./example_2

```

...and .gitignore file containing:

```git
example_2

```

...than result of the command will be:

```git
$ git status --ignored

On branch master

Initial commit

Untracked files:
  (use "git add <file>..." to include in what will be committed)

.gitignore
.example_1

Ignored files:
  (use "git add -f <file>..." to include in what will be committed)

dir/
example_2

```

If you want to list recursively ignored files in directories, you have to use additional parameter - `--untracked-files=all`

Result will look like this:

```git
$ git status --ignored --untracked-files=all
On branch master

Initial commit

Untracked files:
  (use "git add <file>..." to include in what will be committed)

.gitignore
example_1

Ignored files:
  (use "git add -f <file>..." to include in what will be committed)

dir/example_2
example_2

```



## Ignoring only part of a file [stub]


Sometimes you may want to have local changes in a file you don't want to commit or publish. Ideally local settings should be concentrated in a separate file that can be placed into `.gitignore`, but sometimes as a short-term solution it can be helpful to have something local in a checked-in file.

You can make Git "unsee" those lines using clean filter. They won't even show up in diffs.

Suppose here is snippet from file `file1.c`:

```git
struct settings s;
s.host = "localhost";
s.port = 5653;
s.auth = 1;
s.port = 15653; // NOCOMMIT
s.debug = 1; // NOCOMMIT
s.auth = 0; // NOCOMMIT

```

You don't want to publish `NOCOMMIT` lines anywhere.

Create "nocommit" filter by adding this to Git config file like `.git/config`:

```git
[filter "nocommit"]
    clean=grep -v NOCOMMIT

```

Add (or create) this to `.git/info/attributes` or `.gitmodules`:

```git
file1.c filter=nocommit

```

And your NOCOMMIT lines are hidden from Git.

Caveats:

- Using clean filter slows down processing of files, especially on Windows.
- The ignored line may disappear from file when Git updates it. It can be counteracted with a smudge filter, but it is trickier.
- Not tested on Windows



## Ignoring changes in tracked files. [stub]


[.gitignore](https://git-scm.com/docs/gitignore) and `.git/info/exclude` work only for untracked files.

To set ignore flag on a tracked file, use the command [update-index](https://git-scm.com/docs/git-update-index):

```git
git update-index --skip-worktree myfile.c

```

To revert this, use:

```git
git update-index --no-skip-worktree myfile.c

```

You can add this snippet to your global [git config](https://git-scm.com/docs/git-config) to have more convenient `git hide`, `git unhide` and `git hidden` commands:

```git
[alias]
    hide   = update-index --skip-worktree
    unhide = update-index --no-skip-worktree
    hidden  = "!git ls-files -v | grep ^[hsS] | cut -c 3-"

```

You can also use the option --assume-unchanged with the update-index function

```git
git update-index --assume-unchanged <file>

```

If you want to watch this file again for the changes, use

```git
git update-index --no-assume-unchanged <file>

```

When --assume-unchanged flag is specified, the user promises not to change the file and allows Git to assume that the working tree file matches what is recorded in the index.Git will fail in case it needs to modify this file in the index e.g. when merging in a commit; thus, in case the assumed-untracked file is changed upstream, you will need to handle the situation manually.The focus lies on performance in this case.

While --skip-worktree flag is useful when you instruct git not to touch a specific file ever because the file is going to be changed locally and you don't want to accidentally commit the changes (i.e configuration/properties file configured for a particular environment). Skip-worktree takes precedence over assume-unchanged when both are set.



## Clear already committed files, but included in .gitignore


Sometimes it happens that a file was being tracked by git, but in a later point in time was added to .gitignore, in order to stop tracking it. It's a very common scenario to forget to clean up such files before its addition to .gitignore. In this case, the old file will still be hanging around in the repository.

To fix this problem, one could perform a "dry-run" removal of everything in the repository, followed by re-adding all the files back. As long as you don't have pending changes and the `--cached` parameter is passed, this command is fairly safe to run:

```git
# Remove everything from the index (the files will stay in the file system) 
$ git rm -r --cached .

# Re-add everything (they'll be added in the current state, changes included)
$ git add .

# Commit, if anything changed. You should see only deletions
$ git commit -m 'Remove all files that are in the .gitignore'

# Update the remote
$ git push origin master

```

