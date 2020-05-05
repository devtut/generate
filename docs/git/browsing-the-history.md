---
metaTitle: "Browsing the history"
description: "Prettier log, Regular Git Log, Oneline log, Colorize Logs, Log search, List all contributions grouped by author name, Searching commit string in git log, Filter logs, Log for a range of lines within a file, Log with changes inline, Git Log Between Two Branches, Log showing commited files, Show the contents of a single commit, One line showing commiter name and time since commit"
---

# Browsing the history

## Prettier log

To see the log in a prettier graph-like structure use:

```git
git log --decorate --oneline --graph

```

sample output :

```git
* e0c1cea (HEAD -> maint, tag: v2.9.3, origin/maint) Git 2.9.3
*   9b601ea Merge branch 'jk/difftool-in-subdir' into maint
|\
| * 32b8c58 difftool: use Git::* functions instead of passing around state
| * 98f917e difftool: avoid $GIT_DIR and $GIT_WORK_TREE
| * 9ec26e7 difftool: fix argument handling in subdirs
* |   f4fd627 Merge branch 'jk/reset-ident-time-per-commit' into maint
...

```

Since it's a pretty big command, you can assign an alias:

```git
git config --global alias.lol "log --decorate --oneline --graph"

```

To use the alias version:

```git
# history of current branch :
git lol

# combined history of active branch (HEAD), develop and origin/master branches :
git lol HEAD develop origin/master

# combined history of everything in your repo :
git lol --all

```

## "Regular" Git Log

```git
git log

```

will display all your commits with the author and hash. This will be shown over multiple lines per commit. (If you wish to show a single line per commit, look at [onelineing](http://stackoverflow.com/documentation/git/240/git-logs/871/oneline#t=201604090859548590171)). Use the `q` key to exit the log.

>

<p>By default, with no arguments, git log lists the commits made in that
repository in reverse chronological order – that is, the most recent
commits show up first. As you can see, this command lists each commit
with its SHA-1 checksum, the author’s name and email, the date
written, and the commit message. - [**source**](https://git-scm.com/book/en/v2/Git-Basics-Viewing-the-Commit-History)</p>

Example (from [**Free Code Camp**](https://github.com/FreeCodeCamp/FreeCodeCamp) repository):

```git
commit 87ef97f59e2a2f4dc425982f76f14a57d0900bcf
Merge: e50ff0d eb8b729
Author: Brian <sludge256@users.noreply.github.com>
Date:   Thu Mar 24 15:52:07 2016 -0700

    Merge pull request #7724 from BKinahan/fix/where-art-thou

    Fix 'its' typo in Where Art Thou description

commit eb8b7298d516ea20a4aadb9797c7b6fd5af27ea5
Author: BKinahan <b.kinahan@gmail.com>
Date:   Thu Mar 24 21:11:36 2016 +0000

    Fix 'its' typo in Where Art Thou description

commit e50ff0d249705f41f55cd435f317dcfd02590ee7
Merge: 6b01875 2652d04
Author: Mrugesh Mohapatra <raisedadead@users.noreply.github.com>
Date:   Thu Mar 24 14:26:04 2016 +0530

    Merge pull request #7718 from deathsythe47/fix/unnecessary-comma

    Remove unnecessary comma from CONTRIBUTING.md

```

If you wish to limit your command to last `n` commits log you can simply pass a parameter. For example, if you wish to list last 2 commits logs

```git
git log -2

```

## Oneline log

```git
git log --oneline

```

will show all of your commits with only the first part of the hash and the commit message. Each commit will be in a single line, as the `oneline` flag suggests.

> The oneline option prints each commit on a single line, which is useful if you’re looking at a lot of commits. - [**source**](https://git-scm.com/book/en/v2/Git-Basics-Viewing-the-Commit-History)

Example (from [**Free Code Camp**](https://github.com/FreeCodeCamp/FreeCodeCamp) repository, with the same section of code from the other example):

```git
87ef97f Merge pull request #7724 from BKinahan/fix/where-art-thou
eb8b729 Fix 'its' typo in Where Art Thou description
e50ff0d Merge pull request #7718 from deathsythe47/fix/unnecessary-comma
2652d04 Remove unnecessary comma from CONTRIBUTING.md
6b01875 Merge pull request #7667 from zerkms/patch-1
766f088 Fixed assignment operator terminology
d1e2468 Merge pull request #7690 from BKinahan/fix/unsubscribe-crash
bed9de2 Merge pull request #7657 from Rafase282/fix/

```

If you wish to limit you command to last `n` commits log you can simply pass a parameter. For example, if you wish to list last 2 commits logs

```git
git log -2 --oneline

```

## Colorize Logs

```git
git log --graph --pretty=format:'%C(red)%h%Creset -%C(yellow)%d%Creset %s %C(green)(%cr) %C(yellow)<%an>%Creset'

```

The `format` option allows you to specify your own log output format:

| Parameter        | Details                                            |
| ---------------- | -------------------------------------------------- |
| `%C(color_name)` | option colors the output that comes after it       |
| `%h` or %H       | abbreviates commit hash (use %H for complete hash) |
| `%Creset`        | resets color to default terminal color             |
| `%d`             | ref names                                          |
| `%s`             | subject [commit message]                           |
| `%cr`            | committer date, relative to current date           |
| `%an`            | author name                                        |

## Log search

```git
git log -S"#define SAMPLES"

```

Searches for **addition** or **removal** of specific string or the string **matching** provided REGEXP. In this case we're looking for addition/removal of the string `#define SAMPLES`. For example:

```git
+#define SAMPLES  100000

```

or

```git
-#define SAMPLES  100000

```

```git
git log -G"#define SAMPLES"

```

Searches for **changes** in **lines** **containing** specific string or the string **matching** provided REGEXP. For example:

```git
-#define SAMPLES  100000
+#define SAMPLES  100000000

```

## List all contributions grouped by author name

`git shortlog` summarizes `git log` and groups by author

If no parameters are given, a list of all commits made per committer will be shown in chronological order.

```git
$ git shortlog
Committer 1 (<number_of_commits>):
    Commit Message 1
    Commit Message 2
    ...
Committer 2 (<number_of_commits>):
    Commit Message 1
    Commit Message 2
    ...

```

To simply see the number of commits and suppress the commit description, pass in the summary option:

`-s`

`--summary`

```git
$ git shortlog -s
<number_of_commits> Committer 1
<number_of_commits> Committer 2

```

To sort the output by number of commits instead of alphabetically by committer name, pass in the numbered option:

`-n`

`--numbered`

To add the email of a committer, add the email option:

`-e`

`--email`

A custom format option can also be provided if you want to display information other than the commit subject:

`--format`

This can be any string accepted by the `--format` option of `git log`.

See **[Colorizing Logs](http://stackoverflow.com/documentation/git/240/browsing-the-history/13880/colorize-logs)** above for more information on this.

## Searching commit string in git log

Searching git log using some string in log:

```git
git log [options] --grep "search_string"

```

Example:

```git
git log --all --grep "removed file"

```

Will search for `removed file` string in **all logs** in **all branches**.

Starting from git 2.4+, the search can be inverted using the `--invert-grep` option.

Example:

```git
git log --grep="add file" --invert-grep

```

Will show all commits that do not contain `add file`.

## Filter logs

```git
git log --after '3 days ago'

```

Specific dates work too:

```git
git log --after 2016-05-01

```

As with other commands and flags that accept a date parameter,
the allowed date format is as supported by GNU date (highly flexible).

An alias to `--after` is `--since`.

Flags exist for the converse too: `--before` and `--until`.

You can also filter logs by `author`. e.g.

```git
git log --author=author

```

## Log for a range of lines within a file

```git
$ git log -L 1,20:index.html
commit 6a57fde739de66293231f6204cbd8b2feca3a869
Author: John Doe <john@doe.com>
Date:   Tue Mar 22 16:33:42 2016 -0500

    commit message

diff --git a/index.html b/index.html
--- a/index.html
+++ b/index.html
@@ -1,17 +1,20 @@
 <!DOCTYPE HTML>
 <html>
-       <head>
-        <meta charset="utf-8">
+
+<head>
+    <meta charset="utf-8">
     <meta http-equiv="X-UA-Compatible" content="IE=edge">
     <meta name="viewport" content="width=device-width, initial-scale=1">

```

## Log with changes inline

To see the log with changes inline, use the `-p` or `--patch` options.

```git
git log --patch

```

Example (from [Trello Scientist](https://github.com/trello/scientist) repository)

```git
ommit 8ea1452aca481a837d9504f1b2c77ad013367d25
Author: Raymond Chou <info@raychou.io>
Date:   Wed Mar 2 10:35:25 2016 -0800

    fix readme error link

diff --git a/README.md b/README.md
index 1120a00..9bef0ce 100644
--- a/README.md
+++ b/README.md
@@ -134,7 +134,7 @@ the control function threw, but *after* testing the other functions and readying
 the logging. The criteria for matching errors is based on the constructor and
 message.

-You can find this full example at [examples/errors.js](examples/error.js).
+You can find this full example at [examples/errors.js](examples/errors.js).

 ## Asynchronous behaviors


commit d3178a22716cc35b6a2bdd679a7ec24bc8c63ffa
:

```

## Git Log Between Two Branches

`git log master..foo` will show the commits that are on `foo` and not on `master`. Helpful for seeing what commits you've added since branching!

## Log showing commited files

```git
git log --stat

```

Example:

```git
commit 4ded994d7fc501451fa6e233361887a2365b91d1
Author: Manassés Souza <manasses.inatel@gmail.com>
Date:   Mon Jun 6 21:32:30 2016 -0300

    MercadoLibre java-sdk dependency

 mltracking-poc/.gitignore |  1 +
 mltracking-poc/pom.xml    | 14 ++++++++++++--
 2 files changed, 13 insertions(+), 2 deletions(-)

commit 506fff56190f75bc051248770fb0bcd976e3f9a5
Author: Manassés Souza <manasses.inatel@gmail.com>
Date:   Sat Jun 4 12:35:16 2016 -0300

    [manasses] generated by SpringBoot initializr

 .gitignore                                                                            |  42 ++++++++++++
 mltracking-poc/mvnw                                                                   | 233 +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 mltracking-poc/mvnw.cmd                                                               | 145 +++++++++++++++++++++++++++++++++++++++
 mltracking-poc/pom.xml                                                                |  74 ++++++++++++++++++++
 mltracking-poc/src/main/java/br/com/mls/mltracking/MltrackingPocApplication.java      |  12 ++++
 mltracking-poc/src/main/resources/application.properties                              |   0
 mltracking-poc/src/test/java/br/com/mls/mltracking/MltrackingPocApplicationTests.java |  18 +++++
 7 files changed, 524 insertions(+)

```

## Show the contents of a single commit

Using [`git show`](https://git-scm.com/docs/git-show) we can view a single commit

```git
git show 48c83b3
git show 48c83b3690dfc7b0e622fd220f8f37c26a77c934

```

Example

```git
commit 48c83b3690dfc7b0e622fd220f8f37c26a77c934
Author: Matt Clark <mrclark32493@gmail.com>
Date:   Wed May 4 18:26:40 2016 -0400

    The commit message will be shown here.

diff --git a/src/main/java/org/jdm/api/jenkins/BuildStatus.java b/src/main/java/org/jdm/api/jenkins/BuildStatus.java
index 0b57e4a..fa8e6a5 100755
--- a/src/main/java/org/jdm/api/jenkins/BuildStatus.java
+++ b/src/main/java/org/jdm/api/jenkins/BuildStatus.java
@@ -50,7 +50,7 @@ public enum BuildStatus {

                        colorMap.put(BuildStatus.UNSTABLE, Color.decode( "#FFFF55" ));
-                       colorMap.put(BuildStatus.SUCCESS,  Color.decode( "#55FF55" ));
+                       colorMap.put(BuildStatus.SUCCESS,  Color.decode( "#33CC33" ));
                        colorMap.put(BuildStatus.BUILDING, Color.decode( "#5555FF" ));

```

## One line showing commiter name and time since commit

```git
tree = log --oneline --decorate --source --pretty=format:'"%Cblue %h %Cgreen %ar %Cblue %an %C(yellow) %d %Creset %s"'  --all --graph

```

example

```git
*    40554ac  3 months ago  Alexander Zolotov    Merge pull request #95 from gmandnepr/external_plugins
|\
| *  e509f61  3 months ago  Ievgen Degtiarenko    Documenting new property
| *  46d4cb6  3 months ago  Ievgen Degtiarenko    Running idea with external plugins
| *  6253da4  3 months ago  Ievgen Degtiarenko    Resolve external plugin classes
| *  9fdb4e7  3 months ago  Ievgen Degtiarenko    Keep original artifact name as this may be important for intellij
| *  22e82e4  3 months ago  Ievgen Degtiarenko    Declaring external plugin in intellij section
|/
*  bc3d2cb  3 months ago  Alexander Zolotov    Ignore DTD in plugin.xml

```

#### Syntax

- git log [options][revision range] [[--] path...]

#### Parameters

| Parameter                | Explanation                                                                                                           |
| ------------------------ | --------------------------------------------------------------------------------------------------------------------- |
| -q, --quiet              | Quiet, suppresses diff output                                                                                         |
| --source                 | Shows source of commit                                                                                                |
| --use-mailmap            | Use mail map file (changes user info for committing user)                                                             |
| --decorate[=...]         | Decorate options                                                                                                      |
| --L <n,m:file>           | Show log for specific range of lines in a file, counting from 1. Starts from line n, goes to line m. Also shows diff. |
| --show-signature         | Display signatures of signed commits                                                                                  |
| -i, --regexp-ignore-case | Match the regular expression limiting patterns without regard to letter case                                          |

#### Remarks

References and up-to-date **documentation** : [git-log official documentation](https://git-scm.com/docs/git-log)
