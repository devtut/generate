---
metaTitle: "Rebasing"
description: "Local Branch Rebasing, Rebase: ours and theirs, local and remote, Interactive Rebase, Rebase down to the initial commit, Rebasing before a code review, Testing all commits during rebase, Configuring autostash, Aborting an Interactive Rebase, Setup git-pull for automatically perform a rebase instead of a merge, Pushing after a rebase"
---

# Rebasing




## Local Branch Rebasing


**[Rebasing](https://git-scm.com/docs/git-rebase)** reapplies a series of commits on top of another commit.

To `rebase` a branch, checkout the branch and then `rebase` it on top of another branch.

```git
git checkout topic
git rebase master  # rebase current branch onto master branch

```

This would cause:

```

     A---B---C topic
     /
D---E---F---G master

```

To turn into:

```

             A'--B'--C' topic
             /
D---E---F---G master

```

These operations can be combined into a single command that checks out the branch and immediately rebases it:

```git
git rebase master topic   # rebase topic branch onto master branch

```

**Important:** After the rebase, the applied commits will have a different hash. You should not rebase commits you have already pushed to a remote host.  A consequence may be an inability to `git push` your local rebased branch to a remote host, leaving your only option to `git push --force`.



## Rebase: ours and theirs, local and remote


A rebase switches the meaning of "ours" and "theirs":

```git
git checkout topic
git rebase   master    # rebase topic branch on top of master branch

```

**Whatever HEAD's pointing to is "ours"**

The first thing a rebase does is resetting the `HEAD` to `master`; before cherry-picking commits from the old branch `topic` to a new one (every commit in the former `topic` branch will be rewritten and will be identified by a different hash).

With respect to terminologies used by merge tools (not to be confused with [local ref or remote ref](https://git-scm.com/docs/gitglossary#gitglossary-aiddefrefspecarefspec))

```git
=> local is master ("ours"),
=> remote is topic ("theirs")

```

That means a merge/diff tool will present the upstream branch as `local` (`master`: the branch on top of which you are rebasing), and the working branch as `remote` (`topic`: the branch being rebased)

```git
+-----------------------------------------+
| LOCAL:master |    BASE   | REMOTE:topic |
+-----------------------------------------+
|             MERGED                      |
+-----------------------------------------+

```

### Inversion illustrated

### On a merge:

```git
c--c--x--x--x(*) <- current branch topic ('*'=HEAD)
    \
     \
      \--y--y--y <- other branch to merge

```

We don't change the current branch `topic`, so what we have is still what we were working on (and we merge from another branch)

```git
c--c--x--x--x---------o(*)  MERGE, still on branch topic
    \       ^        /
     \     ours     /
      \            /
       --y--y--y--/  
               ^
              theirs

```

### On a rebase:

But **on a rebase** we switch sides because the first thing a rebase does is to checkout the upstream branch to replay the current commits on top of it!

```git
c--c--x--x--x(*) <- current branch topic ('*'=HEAD)
    \
     \
      \--y--y--y <- upstream branch

```

A **`git rebase upstream`** will first set `HEAD` to the upstream branch, hence the switch of 'ours' and 'theirs' compared to the previous "current" working branch.

```git
c--c--x--x--x <- former "current" branch, new "theirs"
    \
     \
      \--y--y--y(*) <- set HEAD to this commit, to replay x's on it
               ^       this will be the new "ours"
               |
            upstream      

```

The rebase will then replay 'their' commits on the new 'our' `topic` branch:

```git
c--c..x..x..x <- old "theirs" commits, now "ghosts", available through "reflogs"
    \
     \
      \--y--y--y--x'--x'--x'(*) <- topic  once all x's are replayed,
               ^                      point branch topic to this commit
               |
        upstream branch

```



## Interactive Rebase


This example aims to describe how one can utilize `git rebase` in interactive mode. It is expected that one has a basic understanding of what `git rebase` is and what it does.

Interactive rebase is initiated using following command:

```git
git rebase -i

```

The `-i` option refers to **interactive mode**. Using interactive rebase, the user can change commit messages, as well as reorder, split, and/or squash (combine to one) commits.

Say you want to rearrange your last three commits. To do this you can run:

```git
git rebase -i HEAD~3

```

After executing the above instruction, a file will be opened in your text editor where you will be able to select how your commits will be rebased. For the purpose of this example, just change the order of your commits, save the file, and close the editor. This will initiate a rebase with the order you've applied. If you check `git log` you will see your commits in the new order you specified.

### Rewording commit messages

Now, you've decided that one of the commit messages is vague and you want it to be more descriptive. Let's examine the last three commits using the same command.

```git
git rebase -i HEAD~3

```

Instead of rearranging the order the commits will be rebased, this time we will change `pick`, the default, to `reword` on a commit where you would like to change the message.

When you close the editor, the rebase will initiate and it will stop at the specific commit message that you wanted to reword. This will let you change the commit message to whichever you desire. After you've changed the message, simply close the editor to proceed.

### Changing the content of a commit

Besides changing the commit message you can also adapt the changes done by the commit. To do so just change `pick` to `edit` for one commit. Git will stop when it arrives at that commit and provide the original changes of the commit in the staging area. You can now adapt those changes by unstaging them or adding new changes.

As soon as the staging area contains all changes you want in that commit, commit the changes. The old commit message will be shown and can be adapted to reflect the new commit.

### Splitting a single commit into multiple

Say you've made a commit but decided at a later point this commit could be split into two or more commits instead. Using the same command as before, replace `pick` with `edit` instead and hit enter.

Now, git will stop at the commit you have marked for editing and place all of its content into the staging area. From that point you can run `git reset HEAD^` to place the commit into your working directory. Then, you can add and commit your files in a different sequence - ultimately splitting a single commit into **n** commits instead.

### Squashing multiple commits into one

Say you have done some work and have multiple commits which you think could be a single commit instead. For that you can carry out `git rebase -i HEAD~3`, replacing `3` with an appropriate amount of commits.

This time replace `pick` with `squash` instead. During the rebase, the commit which you've instructed to be squashed will be squashed on top of the previous commit; turning them into a single commit instead.



## Rebase down to the initial commit


Since Git [1.7.12](https://github.com/git/git/blob/1d1bdafd64266e5ee3bd46c6965228f32e4022ea/Documentation/RelNotes/1.7.12.txt#L59-L60) it is possible to rebase down to the root commit. The root commit is the first commit ever made in a repository, and normally cannot be edited. Use the following command:

```git
git rebase -i --root

```



## Rebasing before a code review


### Summary

This goal is to reorganize all of your scattered commits into more meaningful commits for easier code reviews. If there are too many layers of changes across too many files at once, it is harder to do a code review. If you can reorganize your chronologically created commits into topical commits, then the code review process is easier (and possibly less bugs slip through the code review process).

This overly-simplified example is not the only strategy for using git to do better code reviews. It is the way I do it, and it's something to inspire others to consider how to make code reviews and git history easier/better.

This also pedagogically demonstrates the power of rebase in general.

This example assumes you know about interactive rebasing.

### Assuming:

- you're working on a feature branch off of master
- your feature has three main layers: front-end, back-end, DB
- you have made a lot of commits while working on a feature branch. Each commit touches multiple layers at once
<li>you want (in the end) only three commits in your branch
<ul>
- one containing all front end changes
- one containing all back end changes
- one containing all DB changes

### Strategy:

- we are going to change our chronological commits into "topical" commits.
- first, split all commits into multiple, smaller commits -- each containing only one topic at a time (in our example, the topics are front end, back end, DB changes)
- Then reorder our topical commits together and 'squash' them into single topical commits

### Example:

```git
$ git log --oneline master..
975430b db adding works: db.sql logic.rb
3702650 trying to allow adding todo items: page.html logic.rb
43b075a first draft: page.html and db.sql
$ git rebase -i master

```

This will be shown in text editor:

```git
pick 43b075a first draft: page.html and db.sql
pick 3702650 trying to allow adding todo items: page.html logic.rb
pick 975430b db adding works: db.sql logic.rb

```

Change it to this:

```git
e 43b075a first draft: page.html and db.sql
e 3702650 trying to allow adding todo items: page.html logic.rb
e 975430b db adding works: db.sql logic.rb

```

Then git will apply one commit at a time. After each commit, it will show a prompt, and then you can do the following:

```git
Stopped at 43b075a92a952faf999e76c4e4d7fa0f44576579... first draft: page.html and db.sql
You can amend the commit now, with

        git commit --amend

Once you are satisfied with your changes, run

        git rebase --continue

$ git status
rebase in progress; onto 4975ae9
You are currently editing a commit while rebasing branch 'feature' on '4975ae9'.
  (use "git commit --amend" to amend the current commit)
  (use "git rebase --continue" once you are satisfied with your changes)

nothing to commit, working directory clean
$ git reset HEAD^ #This 'uncommits' all the changes in this commit.
$ git status -s
 M db.sql
 M page.html
$ git add db.sql  #now we will create the smaller topical commits
$ git commit -m "first draft: db.sql"
$ git add page.html
$ git commit -m "first draft: page.html"
$ git rebase --continue

```

Then you will repeat those steps for every commit. In the end, you have this:

```git
$ git log --oneline
0309336 db adding works: logic.rb
06f81c9 db adding works: db.sql
3264de2 adding todo items: page.html
675a02b adding todo items: logic.rb
272c674 first draft: page.html
08c275d first draft: db.sql

```

Now we run rebase one more time to reorder and squash:

```git
$ git rebase -i master

```

This will be shown in text editor:

```git
pick 08c275d first draft: db.sql
pick 272c674 first draft: page.html
pick 675a02b adding todo items: logic.rb
pick 3264de2 adding todo items: page.html
pick 06f81c9 db adding works: db.sql
pick 0309336 db adding works: logic.rb

```

Change it to this:

```git
pick 08c275d first draft: db.sql
s 06f81c9 db adding works: db.sql
pick 675a02b adding todo items: logic.rb
s 0309336 db adding works: logic.rb
pick 272c674 first draft: page.html
s  3264de2 adding todo items: page.html

```

NOTICE: make sure that you tell git rebase to apply/squash the smaller topical commits **in the order they were chronologically commited**. Otherwise you might have false, needless merge conflicts to deal with.

When that interactive rebase is all said and done, you get this:

```git
$ git log --oneline master..
74bdd5f adding todos: GUI layer
e8d8f7e adding todos: business logic layer
121c578 adding todos: DB layer

```

### Recap

You have now rebased your chronological commits into topical commits. In real life, you may not need to do this every single time, but when you do want or need to do this, now you can. Plus, hopefully you learned more about git rebase.



## Testing all commits during rebase


Before making a pull request, it is useful to make sure that compile is successful and tests are passing for each commit in the branch. We can do that automatically using `-x` parameter.

For example:

`git rebase -i -x make`

will perform the interactive rebase and stop after each commit to execute `make`. In case `make` fails, git will stop to give you an opportunity to fix the issues and amend the commit before proceeding with picking the next one.



## Configuring autostash


Autostash is a very useful configuration option when using rebase for local changes. Oftentimes, you may need to bring in commits from the upstream branch, but are not ready to commit just yet.

However, Git does not allow a rebase to start if the working directory is not clean. Autostash to the rescue:

```git
git config --global rebase.autostash    # one time configuration
git rebase @{u}                         # example rebase on upstream branch

```

The autostash will be applied whenever the rebase is finished. It does not matter whether the rebase finishes successfully, or if it is aborted. Either way, the autostash will be applied. If the rebase was successful, and the base commit therefore changed, then there may be a conflict between the autostash and the new commits. In this case, you will have to resolve the conflicts before committing. This is no different than if you would have manually stashed, and then applied, so there is no downside to doing it automatically.



## Aborting an Interactive Rebase


You have started an interactive rebase. In the editor where you pick your commits, you decide that something is going wrong (for example a commit is missing, or you chose the wrong rebase destination), and you want to abort the rebase.

To do this, simply delete all commits and actions (i.e. all lines not starting with the `#` sign) and the rebase will be aborted!

The help text in the editor actually provides this hint:

```git
# Rebase 36d15de..612f2f7 onto 36d15de (3 command(s))
#
# Commands:
# p, pick = use commit
# r, reword = use commit, but edit the commit message
# e, edit = use commit, but stop for amending
# s, squash = use commit, but meld into previous commit
# f, fixup = like "squash", but discard this commit's log message
# x, exec = run command (the rest of the line) using shell
#
# These lines can be re-ordered; they are executed from top to bottom.
#
# If you remove a line here THAT COMMIT WILL BE LOST.
#
# However, if you remove everything, the rebase will be aborted.
#          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Note that empty commits are commented out

```



## Setup git-pull for automatically perform a rebase instead of a merge


If your team is following a rebase-based workflow, it may be a advantageous to setup git so that each newly created branch will perform a rebase operation, instead of a merge operation, during a `git pull`.

To setup every **new** branch to automatically rebase, add the following to your `.gitconfig` or `.git/config`:

```git
[branch]
autosetuprebase = always

```

**Command line: `git config [--global] branch.autosetuprebase always`**

Alternatively, you can setup the `git pull` command to always behave as if the option `--rebase` was passed:

```git
[pull]
rebase = true

```

**Command line: `git config [--global] pull.rebase true`**



## Pushing after a rebase


Sometimes you need rewrite history with a rebase, but `git push` complains about doing so because you rewrote history.

This can be solved with a `git push --force`, but consider `git push --force-with-lease`, indicating that you want the push to fail if the local remote-tracking branch differs from the branch on the remote, e.g.,  someone else pushed to the remote after the last fetch. This avoids inadvertently overwriting someone else's recent push.

**Note**: `git push --force` - and even `--force-with-lease` for that matter - can be a dangerous command because it rewrites the history of the branch. If another person had pulled the branch before the forced push, his/her `git pull` or `git fetch` will have errors because the local history and the remote history are diverged. This may cause the person to have unexpected errors. With enough looking at the reflogs the other user's work can be recovered, but it can lead to a lot of wasted time. If you must do a forced push to a branch with other contributors, try to coordinate with them so that they do not have to deal with errors.



#### Syntax


- `git rebase [-i | --interactive] [options] [--exec <cmd>] [--onto <newbase>] [<upstream>] [<branch>]`
- `git rebase [-i | --interactive] [options] [--exec <cmd>] [--onto <newbase>] --root [<branch>]`
- `git rebase --continue | --skip | --abort | --edit-todo`



#### Parameters


|Parameter|Details
|------
|--continue|Restart the rebasing process after having resolved a merge conflict.
|--abort|Abort the rebase operation and reset HEAD to the original branch. If branch was provided when the rebase operation was started, then HEAD will be reset to branch. Otherwise HEAD will be reset to where it was when the rebase operation was started.
|--keep-empty|Keep the commits that do not change anything from its parents in the result.
|--skip|Restart the rebasing process by skipping the current patch.
|-m, --merge|Use merging strategies to rebase. When the recursive (default) merge strategy is used, this allows rebase to be aware of renames on the upstream side. Note that a rebase merge works by replaying each commit from the working branch on top of the upstream branch. Because of this, when a merge conflict happens, the side reported as ours is the so-far rebased series, starting with upstream, and theirs is the working branch. In other words, the sides are swapped.
|--stat|Show a diffstat of what changed upstream since the last rebase. The diffstat is also controlled by the configuration option rebase.stat.
|-x, --exec `command`|Perform interactive rebase, stopping between each commit and executing `command`



#### Remarks


Please keep in mind that rebase effectively rewrites the repository history.

Rebasing commits that exists in the remote repository could rewrite repository nodes used by other developers as base node for their developments. Unless you really know what you are doing, it is a best practice to rebase before pushing your changes.

