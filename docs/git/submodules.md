---
metaTitle: "Submodules"
description: "Cloning a Git repository having submodules, Updating a Submodule, Adding a submodule, Setting a submodule to follow a branch, Moving a submodule, Removing a submodule"
---

# Submodules




## Cloning a Git repository having submodules


When you clone a repository that uses submodules, you'll need to initialize and update them.

```git
$ git clone --recursive https://github.com/username/repo.git

```

This will clone the referenced submodules and place them in the appropriate folders (including submodules within submodules). This is equivalent to running `git submodule update --init --recursive` immediately after the clone is finished.



## Updating a Submodule


A submodule references a specific commit in another repository. To check out the exact state that is referenced for all submodules, run

```git
git submodule update --recursive

```

Sometimes instead of using the state that is referenced you want to update to your local checkout to the latest state of that submodule on a remote.
To check out all submodules to the latest state on the remote with a single command, you can use

```git
git submodule foreach git pull <remote> <branch>

```

or use the default `git pull` arguments

```git
git submodule foreach git pull

```

Note that this will just update your local working copy. Running `git status` will list the submodule directory as dirty if it changed because of this command. To update your repository to reference the new state instead, you have to commit the changes:

```git
git add <submodule_directory>
git commit

```

There might be some changes you have that can have merge conflict if you use `git pull` so you can use `git pull --rebase` to rewind your changes to top, most of the time it decreases the chances of conflict. Also it pulls all the branches to local.

```git
git submodule foreach git pull --rebase

```

To checkout the latest state of a specific submodule, you can use :

```git
git submodule update --remote <submodule_directory>

```



## Adding a submodule


You can include another Git repository as a folder within your project, tracked by Git:

```git
$ git submodule add https://github.com/jquery/jquery.git

```

You should add and commit the new `.gitmodules` file; this tells Git what submodules should be cloned when `git submodule update` is run.



## Setting a submodule to follow a branch


A submodule is always checked out at a specific commit SHA1 (the "gitlink", special entry in the index of the parent repo)

But one can request to update that submodule to the latest commit of a branch of the submodule remote repo.

Rather than going in each submodule, doing a `git checkout abranch --track origin/abranch, git pull`, you can simply do (from the parent repo) a:

```git
git submodule update --remote --recursive

```

Since the SHA1 of the submodule would change, you would still need to follow that with:

```git
git add .
git commit -m "update submodules"

```

That supposes the submodules were:

<li>
either added with a branch to follow:

```git
  git submodule -b abranch -- /url/of/submodule/repo

```


</li>
<li>
or configured (for an existing submodule) to follow a branch:

```git
  cd /path/to/parent/repo
  git config -f .gitmodules submodule.asubmodule.branch abranch

```


</li>



## Moving a submodule


Run:

```git
$ git mv **old/path/to/module** **new/path/to/module**</pre></code>

1.8
<ol>
<li>
Edit `.gitmodules` and change the path of the submodule appropriately, and put it in the index with `git add .gitmodules`.
</li>
<li>
If needed, create the parent directory of the new location of the submodule (`mkdir -p **new/path/to**`).
</li>
<li>
Move all content from the old to the new directory (`mv -vi **old/path/to/module** **new/path/to/submodule**`).
</li>
<li>
Make sure Git tracks this directory (`git add **new/path**/to`).
</li>
<li>
Remove the old directory with `git rm --cached **old/path/to/module**`.
</li>
<li>
Move the directory `.git/modules/**old/path/to/module**` with all its content to `.git/modules/**new/path/to/module**`.
</li>
<li>
<p>Edit the `.git/modules/**new/path/to**/config` file, make sure that worktree item points to the new locations, so in this example it should be `worktree = ../../../../../**old/path/to/module**`. Typically there should be two more `..` then directories in the direct path in that place.
. Edit the file `**new/path/to/module**/.git`, make sure that the path in it points to the correct new location inside the main project `.git` folder, so in this example `gitdir: ../../../.git/modules/**new/path/to/module**`.</p>
`git status` output looks like this afterwards:

```git
 # On branch master
 # Changes to be committed:
 #   (use "git reset HEAD <file>..." to unstage)
 #
 #       modified:   .gitmodules
 #       renamed:    old/path/to/submodule -> new/path/to/submodule
 #

```


<li>
Edit `.gitmodules` and change the path of the submodule appropriately, and put it in the index with `git add .gitmodules`.
</li>
<li>
If needed, create the parent directory of the new location of the submodule (`mkdir -p **new/path/to**`).
</li>
<li>
Move all content from the old to the new directory (`mv -vi **old/path/to/module** **new/path/to/submodule**`).
</li>
<li>
Make sure Git tracks this directory (`git add **new/path**/to`).
</li>
<li>
Remove the old directory with `git rm --cached **old/path/to/module**`.
</li>
<li>
Move the directory `.git/modules/**old/path/to/module**` with all its content to `.git/modules/**new/path/to/module**`.
</li>
<li>
<p>Edit the `.git/modules/**new/path/to**/config` file, make sure that worktree item points to the new locations, so in this example it should be `worktree = ../../../../../**old/path/to/module**`. Typically there should be two more `..` then directories in the direct path in that place.
. Edit the file `**new/path/to/module**/.git`, make sure that the path in it points to the correct new location inside the main project `.git` folder, so in this example `gitdir: ../../../.git/modules/**new/path/to/module**`.</p>
`git status` output looks like this afterwards:

```git
 # On branch master
 # Changes to be committed:
 #   (use "git reset HEAD <file>..." to unstage)
 #
 #       modified:   .gitmodules
 #       renamed:    old/path/to/submodule -> new/path/to/submodule
 #

```


</li>
<li>
Finally, commit the changes.
</li>

This example from [Stack Overflow](http://stackoverflow.com/a/6310246), by [Axel Beckert](http://stackoverflow.com/users/793172)



## Removing a submodule


You can remove a submodule (e.g. `the_submodule`) by calling:

```git
$ git submodule deinit the_submodule
$ git rm the_submodule 

```


<li>
`git submodule deinit the_submodule` deletes `the_submodule`s' entry from .git/config. This excludes the_submodule from `git submodule update`, `git submodule sync` and `git submodule foreach` calls and deletes its local content [(source)](https://git-scm.com/docs/git-submodule#git-submodule-deinit). Also, this will not be shown as change in your parent repository. `git submodule init` and `git submodule update` will restore the submodule, again without commitable changes in your parent repository.
</li>
<li>
`git rm the_submodule` will remove the submodule from the work tree. The files will be gone as well as the submodules' entry in the `.gitmodules` file [(source)](https://git-scm.com/docs/git-rm#_submodules). If only `git rm the_submodule` (without prior `git submodule deinit the_submodule` is run, however, the submodules' entry in your .git/config file will remain.
</li>

Taken from [here](http://stackoverflow.com/a/1260982/7598462):

1. Delete the relevant section from the `.gitmodules` file.
1. Stage the `.gitmodules` changes `git add .gitmodules`
1. Delete the relevant section from `.git/config`.
1. Run `git rm --cached path_to_submodule` (no trailing slash).
1. Run `rm -rf .git/modules/path_to_submodule`
1. Commit `git commit -m "Removed submodule <name>"`
1. Delete the now untracked submodule files
1. `rm -rf path_to_submodule`

