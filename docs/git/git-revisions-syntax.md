---
metaTitle: "Git - Git revisions syntax"
description: "Specifying revision by object name, Symbolic ref names: branches, tags, remote-tracking branches, The default revision: HEAD, Reflog references: <refname>@{<n>}, Reflog references: <refname>@{<date>}, Tracked / upstream branch: <branchname>@{upstream}, Commit ancestry chain: <rev>^, <rev>~<n>, etc., Dereferencing branches and tags: <rev>^0, <rev>^{<type>}, Youngest matching commit: <rev>^{/<text>}, :/<text>"
---

# Git revisions syntax



## Specifying revision by object name


```git
$ git show dae86e1950b1277e545cee180551750029cfe735
$ git show dae86e19

```

You can specify revision (or in truth any object: tag, tree i.e. directory contents, blob i.e. file contents) using SHA-1 object name, either full 40-byte hexadecimal string, or a substring that is unique to the repository.



## Symbolic ref names: branches, tags, remote-tracking branches


```git
$ git log master    # specify branch
$ git show v1.0     # specify tag
$ git show HEAD     # specify current branch
$ git show origin   # specify default remote-tracking branch for remote 'origin'

```

You can specify revision using a symbolic ref name, which includes branches (for example 'master', 'next', 'maint'), tags (for example 'v1.0', 'v0.6.3-rc2'), remote-tracking branches (for example 'origin', 'origin/master'), and special refs such as 'HEAD' for current branch.

If the symbolic ref name is ambiguous, for example if you have both branch and tag named 'fix' (having branch and tag with the same name is not recommended), you need to specify the kind of ref you want to use:

```git
$ git show heads/fix      # or 'refs/heads/fix', to specify branch
$ git show tags/fix       # or 'refs/tags/fix', to specify tag

```



## The default revision: HEAD


```git
$ git show         # equivalent to 'git show HEAD'

```

'HEAD' names the commit on which you based the changes in the working tree, and is usually the symbolic name for the current branch.  Many (but not all) commands that take revision parameter defaults to 'HEAD' if it is missing.



## Reflog references: <refname>@{<n>}


```git
$ git show @{1}            # uses reflog for current branch
$ git show master@{1}      # uses reflog for branch 'master'
$ git show HEAD@{1}        # uses 'HEAD' reflog

```

A ref, usually a branch or HEAD, followed by the suffix `@` with an ordinal specification enclosed in a brace pair (e.g. `{1}`, `{15}`) specifies the n-th prior value of that ref **in your **local** repository**.  You can check recent reflog entries with [`git reflog`](https://www.kernel.org/pub/software/scm/git/docs/git-reflog.html) command, or `--walk-reflogs` / `-g` option to `git log`.

```git
$ git reflog
08bb350 HEAD@{0}: reset: moving to HEAD^
4ebf58d HEAD@{1}: commit: gitweb(1): Document query parameters
08bb350 HEAD@{2}: pull: Fast-forward
f34be46 HEAD@{3}: checkout: moving from af40944bda352190f05d22b7cb8fe88beb17f3a7 to master
af40944 HEAD@{4}: checkout: moving from master to v2.6.3

$ git reflog gitweb-docs
4ebf58d gitweb-docs@{0}: branch: Created from master

```

**Note**: using reflogs practically replaced older mechanism of utilizing `ORIG_HEAD` ref (roughly equivalent to `HEAD@{1}`).



## Reflog references: <refname>@{<date>}


```git
$ git show master@{yesterday}
$ git show HEAD@{5 minutes ago}   # or HEAD@{5.minutes.ago}

```

A ref followed by the suffix `@` with a date specification enclosed in a brace pair (e.g. `{yesterday}`, `{1 month 2 weeks 3 days 1 hour 1 second ago}` or `{1979-02-26 18:30:00}`) specifies the value of the ref at a prior point in time (or closest point to it). Note that this looks up the state of your **local** ref at a given time; e.g., what was in your local **'master'** branch last week.

You can use [`git reflog`](https://www.kernel.org/pub/software/scm/git/docs/git-reflog.html) with a date specifier to look up exact time where you did something to given ref in the local repository.

```git
$ git reflog HEAD@{now}
08bb350 HEAD@{Sat Jul 23 19:48:13 2016 +0200}: reset: moving to HEAD^
4ebf58d HEAD@{Sat Jul 23 19:39:20 2016 +0200}: commit: gitweb(1): Document query parameters
08bb350 HEAD@{Sat Jul 23 19:26:43 2016 +0200}: pull: Fast-forward

```



## Tracked / upstream branch: <branchname>@{upstream}


```git
$ git log @{upstream}..       # what was done locally and not yet published, current branch
$ git show master@{upstream}  # show upstream of branch 'master'

```

The suffix `@{upstream}` appended to a branchname (short form `<branchname>@{u}`) refers to the branch that the branch specified by branchname is set to build on top of (configured with `branch.<name>.remote` and `branch.<name>.merge`, or with `git branch --set-upstream-to=<branch>`). A missing branchname defaults to the current one.

Together with syntax for revision ranges it is very useful to see the commits your branch is ahead of upstream (commits in your local repository not yet present upstream), and what commits you are behind (commits in upstream not merged into local branch), or both:

```git
$ git log --oneline @{u}..
$ git log --oneline ..@{u}
$ git log --oneline --left-right @{u}...  # same as ...@{u}

```



## Commit ancestry chain: <rev>^, <rev>~<n>, etc.


```git
$ git reset --hard HEAD^             # discard last commit
$ git rebase --interactive HEAD~5    # rebase last 4 commits

```

A suffix `^` to a revision parameter means the first parent of that commit object. `^<n>` means the <n>-th parent (i.e. `<rev>^` is equivalent to `<rev>^1`).

A suffix `~<n>` to a revision parameter means the commit object that is the <n>-th generation ancestor of the named commit object, following only the first parents. This means that for example `<rev>~3` is equivalent to `<rev>^^^`.  As a shortcut, `<rev>~` means `<rev>~1`, and is equivalent to `<rev>^1`, or `<rev>^` in short.

This syntax is composable.

To find such symbolic names you can use the [`git name-rev`](https://www.kernel.org/pub/software/scm/git/docs/git-name-rev.html) command:

```git
$ git name-rev 33db5f4d9027a10e477ccf054b2c1ab94f74c85a
33db5f4d9027a10e477ccf054b2c1ab94f74c85a tags/v0.99~940

```

Note that `--pretty=oneline` and not `--oneline` must be used in the following example

```git
$ git log --pretty=oneline | git name-rev --stdin --name-only
master Sixth batch of topics for 2.10
master~1 Merge branch 'ls/p4-tmp-refs'
master~2 Merge branch 'js/am-call-theirs-theirs-in-fallback-3way'
[...]
master~14^2 sideband.c: small optimization of strbuf usage
master~16^2 connect: read $GIT_SSH_COMMAND from config file
[...]
master~22^2~1 t7810-grep.sh: fix a whitespace inconsistency
master~22^2~2 t7810-grep.sh: fix duplicated test name

```



## Dereferencing branches and tags: <rev>^0, <rev>^{<type>}


In some cases the behavior of a command depends on whether it is given branch name, tag name, or an arbitrary revision. You can use "de-referencing" syntax if you need the latter.

A suffix `^` followed by an object type name (`tag`, `commit`, `tree`, `blob`) enclosed in brace pair (for example `v0.99.8^{commit}`) means dereference the object at `<rev>` recursively until an object of type `<type>` is found or the object cannot be dereferenced anymore.  `<rev>^0` is a short-hand for `<rev>^{commit}`.

```git
$ git checkout HEAD^0    # equivalent to 'git checkout --detach' in modern Git

```

A suffix `^` followed by an empty brace pair (for example `v0.99.8^{}`) means to dereference the tag recursively until a non-tag object is found.

Compare

```git
$ git show v1.0
$ git cat-file -p v1.0
$ git replace --edit v1.0

```

with

```git
$ git show v1.0^{}
$ git cat-file -p v1.0^{}
$ git replace --edit v1.0^{}

```



## Youngest matching commit: <rev>^{/<text>}, :/<text>


```git
$ git show HEAD^{/fix nasty bug}   # find starting from HEAD
$ git show ':/fix nasty bug'       # find starting from any branch

```

A colon ('`:`'), followed by a slash ('`/`'), followed by a text, names a commit whose commit message matches the specified regular expression. This name returns the youngest matching commit which is reachable from **any** ref. The regular expression can match any part of the commit message. To match messages starting with a string, one can use e.g. `:/^foo`. The special sequence `:/!` is reserved for modifiers to what is matched. `:/!-foo` performs a negative match, while `:/!!foo` matches a literal ! character, followed by `foo`.

A suffix `^` to a revision parameter, followed by a brace pair that contains a text led by a slash, is the same as the `:/<text>` syntax below that it returns the youngest matching commit which is reachable from the `<rev>` before `^`.



#### Remarks


Many Git commands take revision parameters as arguments. Depending on the command, they denote a specific commit or, for commands which walk the revision graph (such as [**git-log(1)**](https://www.kernel.org/pub/software/scm/git/docs/git-log.html)), all commits which can be reached from that commit. They are usually denoted as `<commit>`, or `<rev>`, or `<revision>` in the syntax description.

The reference documentation for Git revisions syntax is the [**gitrevisions(7)**](https://www.kernel.org/pub/software/scm/git/docs/gitrevisions.html) manpage.

Still missing from this page:

- [_] Output from `git describe`, e.g. `v1.7.4.2-679-g3bee7fb`
- [_] `@` alone as a shortcut for `HEAD`
- [_] `@{-<n>}`, e.g. `@{-1}`, and `-` meaning `@{-1}`
- [_] `<branchname>@{push}`
- [_] `<rev>^@`, for all parents of `<rev>`

Needs separate documentation:

- [_] Referring to blobs and trees in the repository and in the index: `<rev>:<path>` and `:<n>:<path>` syntax
- [_] Revision ranges like `A..B`, `A...B`, `B ^A`, `A^1`, and revision limiting like `-<n>`, `--since`

