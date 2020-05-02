---
metaTitle: "Bisecting/Finding faulty commits"
description: "Binary search (git bisect), Semi-automatically find a faulty commit"
---

# Bisecting/Finding faulty commits




## Binary search (git bisect)


[`git bisect`](https://git-scm.com/docs/git-bisect) allows you to find which commit introduced a bug using a binary search.

Start by bisecting a session by providing two commit references: a good commit before the bug, and a bad commit after the bug. Generally, the bad commit is `HEAD`.

```git
# start the git bisect session
$ git bisect start

# give a commit where the bug doesn't exist
$ git bisect good 49c747d

# give a commit where the bug exist
$ git bisect bad HEAD

```

`git` starts a binary search: It splits the revision in half and switches the repository to the intermediate revision. Inspect the code to determine if the revision is good or bad:

```git
# tell git the revision is good,
# which means it doesn't contain the bug
$ git bisect good

# if the revision contains the bug,
# then tell git it's bad
$ git bisect bad

```

`git` will continue to run the binary search on each remaining subset of bad revisions depending on your instructions. `git` will present a single revision that, unless your flags were incorrect, will represent exactly the revision where the bug was introduced.

Afterwards remember to run `git bisect reset` to end the bisect session and return to HEAD.

```git
$ git bisect reset

```

If you have a script that can check for the bug, you can automate the process with:

```git
$ git bisect run [script] [arguments]

```

Where `[script]` is the path to your script and `[arguments]` is any arguments that should be passed to your script.

Running this command will automatically run through the binary search, executing `git bisect good` or `git bisect bad` at each step depending on the exit code of your script.  Exiting with 0 indicates `good`, while exiting with 1-124, 126, or 127 indicates bad.  125 indicates that the script cannot test that revision (which will trigger a `git bisect skip`).



## Semi-automatically find a faulty commit


Imagine you are on the `master` branch and something is not working as expected (a regression was introduced), but you don't know where. All you know is, that is was working in the last release (which was e.g., tagged or you know the commit hash, lets take `old-rel` here).

Git has help for you, finding the faulty commit which introduced the regression with a very low number of steps (binary search).

First of all start bisecting:

```git
git bisect start master old-rel

```

This will tell git that `master` is a broken revision (or the first broken version) and `old-rel` is the last known version.

Git will now check out a detached head in the middle of both commits. Now, you can do your testing. Depending on whether it works or not issue

```git
git bisect good

```

or

```git
git bisect bad

```

. In case this commit cannot be tested, you can easily `git reset` and test that one, git willl take care of this.

After a few steps git will output the faulty commit hash.

In order to abort the bisect process just issue

```git
git bisect reset

```

and git will restore the previous state.



#### Syntax


<li>
`git bisect <subcommand> <options>`
</li>
<li>
`git bisect start <bad> [<good>...]`
</li>
<li>
`git bisect reset`
</li>
<li>
`git bisect good`
</li>
<li>
`git bisect bad`
</li>

