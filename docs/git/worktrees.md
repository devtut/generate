---
metaTitle: "Worktrees"
description: "Using a worktree, Moving a worktree"
---

# Worktrees



## Using a worktree


You are right in the middle of working on a new feature, and your boss comes in demanding that you fix something immediately. You may typically want use `git stash` to store your changes away temporarily. However, at this point your working tree is in a state of disarray (with new, moved, and removed files, and other bits and pieces strewn around) and you don't want to disturb your progress.

By adding a worktree, you create a temporary linked working tree to make the emergency fix, remove it when done, and then resume your earlier coding session:

```git
$ git worktree add -b emergency-fix ../temp master
$ pushd ../temp
# ... work work work ...
$ git commit -a -m 'emergency fix for boss'
$ popd
$ rm -rf ../temp
$ git worktree prune

```

NOTE: In this example, the fix still is in the emergency-fix branch. At this point you probably want to `git merge` or `git format-patch` and afterwards remove the emergency-fix branch.



## Moving a worktree


Currently (as of version 2.11.0) there is no built-in functionality to move an already existing worktree. This is listed as an official bug (see [https://git-scm.com/docs/git-worktree#_bugs)](https://git-scm.com/docs/git-worktree#_bugs)).

To get around this limitation it is possible to perform manual operations directly in the `.git` reference files.

In this example, the main copy of the repo is living at `/home/user/project-main` and the secondary worktree is located at `/home/user/project-1` and we want to move it to `/home/user/project-2`.

Don't perform any git command in between these steps, otherwise the garbage collector might be triggered and the references to the secondary tree can be lost. Perform these steps from the start until the end without interruption:

<li>
<p>Change the worktree's `.git` file to point to the new location inside the main
tree. The file `/home/user/project-1/.git` should now contain the following:</p>

```git
gitdir: /home/user/project-main/.git/worktrees/project-2

```


</li>
<li>
Rename the worktree inside the `.git` directory of the main project by moving the worktree's directory that exists in there:

```git
$ mv /home/user/project-main/.git/worktrees/project-1 /home/user/project-main/.git/worktrees/project-2

```


</li>
<li>
Change the reference inside `/home/user/project-main/.git/worktrees/project-2/gitdir` to point to the new location. In this example, the file would have the following contents:

```git
/home/user/project-2/.git

```


</li>
<li>
Finally, move your worktree to the new location:

```git
$ mv /home/user/project-1 /home/user/project-2

```


</li>

If you have done everything correctly, listing the existing worktrees should refer to the new location:

```git
$ git worktree list
/home/user/project-main  23f78ad [master]
/home/user/project-2     78ac3f3 [branch-name]

```

It should now also be safe to run `git worktree prune`.



#### Syntax


- git worktree add [-f] [--detach] [--checkout] [-b <new-branch>] <path> [<branch>]
- git worktree prune [-n] [-v] [--expire <expire>]
- git worktree list [--porcelain]



#### Parameters


|Parameter|Details
|---|---|---
|-f --force|By default, add refuses to create a new working tree when `<branch>` is already checked out by another working tree. This option overrides that safeguard.
|-b `<new-branch>` -B `<new-branch>`|With add, create a new branch named `<new-branch>` starting at `<branch>`, and check out `<new-branch>` into the new working tree. If `<branch>` is omitted, it defaults to `HEAD`. By default, `-b` refuses to create a new branch if it already exists. `-B` overrides this safeguard, resetting `<new-branch>` to `<branch>`.
|--detach|With add, detach `HEAD` in the new working tree.
|--[no-] checkout|By default, add checks out `<branch>`, however, `--no-checkout` can be used to suppress checkout in order to make customizations, such as configuring sparse-checkout.
|-n --dry-run|With prune, do not remove anything; just report what it would remove.
|--porcelain|With list, output in an easy-to-parse format for scripts. This format will remain stable across Git versions and regardless of user configuration.
|-v --verbose|With prune, report all removals.
|--expire `<time>`|With prune, only expire unused working trees older than `<time>`.



#### Remarks


See the official documentation for more information: [https://git-scm.com/docs/git-worktree](https://git-scm.com/docs/git-worktree).

