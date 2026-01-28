---
metaTitle: "Git - git-svn"
description: "Cloning the SVN repository, Pushing local changes to SVN, Working locally, Getting the latest changes from SVN, Handling empty folders"
---

# git-svn



## Cloning the SVN repository


You need to create a new local copy of the repository with the command

`git svn clone SVN_REPO_ROOT_URL [DEST_FOLDER_PATH] -T TRUNK_REPO_PATH -t TAGS_REPO_PATH -b BRANCHES_REPO_PATH`

If your SVN repository follows the standard layout (trunk, branches, tags folders) you can save some typing:

`git svn clone -s SVN_REPO_ROOT_URL [DEST_FOLDER_PATH]`

`git svn clone` checks out each SVN revision, one by one, and makes a git commit in your local repository in order to recreate the history. If the SVN repository has a lot of commits this will take a while.

When the command is finished you will have a full fledged git repository with a local branch called master that tracks the trunk branch in the SVN repository.



## Pushing local changes to SVN


The command

```git
git svn dcommit

```

will create a SVN revision for each of your local git commits. As with SVN, your local git history must be in sync with the latest changes in the SVN repository, so if the command fails, try performing a `git svn rebase` first.



## Working locally


Just use your local git repository as a normal git repo, with the normal git commands:

- `git add FILE` and `git checkout -- FILE` To stage/unstage a file
- `git commit` To save your changes. Those commits will be local and will not be "pushed" to the SVN repo, just like in a normal git repository
- `git stash` and `git stash pop` Allows using stashes
- `git reset HEAD --hard` Revert all your local changes
- `git log` Access all the history in the repository
- `git rebase -i` so you can rewrite your local history freely
- `git branch` and `git checkout` to create local branches

As the git-svn documentation states "Subversion is a system that is far less sophisticated than Git" so you can't use all the full power of git without messing up the history in the Subversion server. Fortunately the rules are very simple: **Keep the history linear**

This means you can make almost any git operation: creating branches, removing/reordering/squashing commits, move the history around, delete commits, etc. Anything **but merges**.  If you need to reintegrate the history of local branches use `git rebase` instead.

When you perform a merge, a merge commit is created. The particular thing about merge commits is that they have two parents, and that makes the history non-linear. Non-linear history will confuse SVN in the case you "push" a merge commit to the repository.

However do not worry: **you won't break anything if you "push" a git merge commit to SVN**. If you do so, when the git merge commit is sent to the svn server it will contain all the changes of all commits for that merge, so you will lose the history of those commits, but not the changes in your code.



## Getting the latest changes from SVN


The equivalent to `git pull` is the command

```git
git svn rebase

```

This retrieves all the changes from the SVN repository and applies them **on top** of your local commits in your current branch.

You can also use the command

```git
git svn fetch

```

to retrieve the changes from the SVN repository and bring them to your local machine but without applying them to your local branch.



## Handling empty folders


git does not recognice the concept of folders, it just works with files and their filepaths. This means git does not track empty folders. SVN, however, does. Using git-svn means that, by default,  **any change you do involving empty folders with git will not be propagated to SVN**.

Using the `--rmdir` flag when issuing a comment corrects this issue, and removes an empty folder in SVN if you locally delete the last file inside it:

`git svn dcommit --rmdir`

Unfortunately **it does not removes existing empty folders**: you need to do it manually.

To avoid adding the flag each time you do a dcommit, or to play it safe if you are using a git GUI tool (like SourceTree) you can set this behaviour as default with the command:

`git config --global svn.rmdir true`

This changes your .gitconfig file and adds these lines:

```git
[svn]
rmdir = true

```

To remove all untracked files and folders that should be kept empty for SVN use the git command:

`git clean -fd`

Please note: the previous command will remove all untracked files and empty folders, even the ones that should be tracked by SVN!
If you need to generate againg the empty folders tracked by SVN use the command

`git svn mkdirs`

In practices this means that if you want to cleanup your workspace from untracked files and folders you should always use both commands to recreate the empty folders tracked by SVN:

`git clean -fd && git svn mkdirs`



#### Remarks


**Cloning really big SVN repositories**

If you SVN repo history is really really big this operation could take hours, as git-svn needs to rebuild the complete history of the SVN repo.
Fortunately you only need to clone the SVN  repo once; as with any other git repository you can just copy the repo folder to other collaborators. Copying the folder to multiple computers will be quicker that just cloning big SVN repos from scratch.

**About commits and SHA1**

Your local git commits will be **rewritten** when using the command `git svn dcommit`. This command will add a text to the git commit's message referencing the SVN revision created in the SVN server, which is very useful. However, adding a new text requires modifying an existing commit's message which can't actually be done: git commits are inmutable. The solution is create a new commit with the same contents and the new message, but it is technically a new commit anyway (i.e. the git commit's SHA1 will change)

As git commits created for git-svn are local, the SHA1 ids for git commits are different between each git repository! This means that you can't use a SHA1 to reference a commit from another person because the same commit will have a diferent SHA1 in each local git repository.
You need to rely in svn revision number appended to the commit message when you push to the SVN server if you want to reference a commit between different copies of the repository.

**You can** use the SHA1 for local operations though (show/diff an specific commit, cherry-picks and resets, etc)

### Troubleshooting

**git svn rebase command issues a checksum mismatch error**

The command git svn rebase throws an error similar to this:

```

 Checksum mismatch: <path_to_file> <some_kind_of_sha1>
  expected: <checksum_number_1>
    got: <checksum_number_2>

```

The solution to this problem is reset svn to the revision when the troubled file got modified for the last time, and do a git svn fetch so the SVN history is restored. The commands to perform the SVN reset are:

- git log -1 -- `<path_to_file>` (copy the SVN revision number that appear in the commit message)
- git svn reset `<revision_number>`
- git svn fetch

You should be able to push/pull data from SVN again

**File was not found in commit**
When you try to fetch or pull from SVN you get an error similar to this

```git
<file_path> was not found in commit <hash>

```

This means that a revision in SVN is trying to modify a file that for some reason doesn't exists in your local copy.  The best way to get rid of this error is force a fetch ignoring the path of that file and it will updated to its status in the latest SVN revision:

- `git svn fetch --ignore-paths <file_path>`

