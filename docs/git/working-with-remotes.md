---
metaTitle: "Git - Working with Remotes"
description: "Deleting a Remote Branch, Changing Git Remote URL, Updating from Upstream Repository, ls-remote, Removing Local Copies of Deleted Remote Branches, List Existing Remotes, Adding a New Remote Repository, Getting Started, Set Upstream on a New Branch, Show information about a Specific Remote, Changing a Remote Repository, Renaming a Remote, Set the URL for a Specific Remote, Get the URL for a Specific Remote"
---

# Working with Remotes



## Deleting a Remote Branch


To delete a remote branch in Git:

```git
git push [remote-name] --delete [branch-name]

```

or

```git
git push [remote-name] :[branch-name]

```



## Changing Git Remote URL


Check existing remote

```git
git remote -v 
# origin https://github.com/username/repo.git (fetch)
# origin https://github.com/usernam/repo.git (push)

```

Changing repository URL

```git
git remote set-url origin https://github.com/username/repo2.git
# Change the 'origin' remote's URL

```

Verify new remote URL

```git
git remote -v
# origin  https://github.com/username/repo2.git (fetch)
# origin  https://github.com/username/repo2.git (push)

```



## Updating from Upstream Repository


Assuming you set the upstream (as in the "setting an upstream repository")

```git
git fetch remote-name
git merge remote-name/branch-name

```

The `pull` command combines a `fetch` and a `merge`.

```git
git pull

```

The `pull` with `--rebase` flag command combines a `fetch` and a `rebase` instead of `merge`.

```git
git pull --rebase remote-name branch-name

```



## ls-remote


[`git ls-remote`](https://git-scm.com/docs/git-ls-remote) is one unique command allowing you to query a remote repo **without having to clone/fetch it first**.

It will list refs/heads and refs/tags of said remote repo.

You will see sometimes `refs/tags/v0.1.6` **and** `refs/tags/v0.1.6^{}`: the `^{}` to list the dereferenced annotated tag (ie the commit that tag is pointing to)

Since git 2.8 (March 2016), you can avoid that double entry for a tag, and list directly those dereferenced tags with:

```git
git ls-remote --ref

```

It can also help resolve the actual url used by a remote repo when you have "`url.<base>.insteadOf`" config setting.<br />
If `git remote --get-url <aremotename>` returns [https://server.com/user/repo](https://server.com/user/repo), and you have set `git config url.ssh://git@server.com:.insteadOf https://server.com/`:

```git
git ls-remote --get-url <aremotename>
ssh://git@server.com:user/repo

```



## Removing Local Copies of Deleted Remote Branches


If a remote branch has been deleted, your local repository has to be told to prune the reference to it.

To prune deleted branches from a specific remote:

```git
git fetch [remote-name] --prune

```

To prune deleted branches from **all** remotes:

```git
git fetch --all --prune

```



## List Existing Remotes


List all the existing remotes associated with this repository:

```git
git remote

```

List all the existing remotes associated with this repository in detail including the `fetch` and `push` URLs:

```git
git remote --verbose

```

or simply

```git
git remote -v

```



## Adding a New Remote Repository


```git
git remote add upstream git-repository-url

```

Adds remote git repository represented by `git-repository-url` as new remote named `upstream` to the git repository



## Getting Started


### Syntax for pushing to a remote branch

`git push <remote_name> <branch_name>`

### Example

`git push origin master`



## Set Upstream on a New Branch


You can create a new branch and switch to it using

```git
git checkout -b AP-57

```

After you use git checkout to create a new branch, you will need to set that upstream origin to push to using

```git
git push --set-upstream origin AP-57

```

After that, you can use git push while you are on that branch.



## Show information about a Specific Remote


Output some information about a known remote: `origin`

```git
git remote show origin

```

Print just the remote's URL:

```git
git config --get remote.origin.url

```

With 2.7+, it is also possible to do, which is arguably better than the above one that uses the `config` command.

```git
git remote get-url origin

```



## Changing a Remote Repository


To change the URL of the repository you want your remote to point to, you can use the `set-url` option, like so:

```git
git remote set-url <remote_name> <remote_repository_url>

```

Example:

```git
git remote set-url heroku https://git.heroku.com/fictional-remote-repository.git

```



## Renaming a Remote


To rename remote, use command `git remote rename`

The `git remote rename` command takes two arguments:

- An existing remote name, for example : **origin**
- A new name for the remote, for example : **destination**

Get existing remote name

```git
git remote
# origin

```

Check existing remote with URL

```git
git remote -v 
# origin https://github.com/username/repo.git (fetch)
# origin https://github.com/usernam/repo.git (push)

```

Rename remote

```

git remote rename origin destination
 # Change remote name from 'origin' to 'destination'

```

Verify new name

```git
git remote -v 
# destination https://github.com/username/repo.git (fetch)
# destination https://github.com/usernam/repo.git (push)

```

**=== Posible Errors ===**

<li>
Could not rename config section 'remote.[old name]' to 'remote.[new name]'
This error means that the remote you tried the old remote name (**origin**) doesn't exist.
</li>

<li>
Remote [new name] already exists.
Error message is self explanatory.
</li>



## Set the URL for a Specific Remote


You can change the url of an existing remote by the command

```git
git remote set-url remote-name url 

```



## Get the URL for a Specific Remote


You can obtain the url for an existing remote by using the command

`git remote get-url <name>`

By default, this will be

`git remote get-url origin`



#### Syntax


- `git remote [-v | --verbose]`
- `git remote add [-t <branch>] [-m <master>] [-f] [--[no-]tags] [--mirror=<fetch|push>] <name> <url>`
- `git remote rename <old> <new>`
- `git remote remove <name>`
- `git remote set-head <name> (-a | --auto | -d | --delete | <branch>)`
- `git remote set-branches [--add] <name> <branch>…​`
- `git remote get-url [--push] [--all] <name>`
- `git remote set-url [--push] <name> <newurl> [<oldurl>]`
- `git remote set-url --add [--push] <name> <newurl>`
- `git remote set-url --delete [--push] <name> <url>`
- `git remote [-v | --verbose] show [-n] <name>…​`
- `git remote prune [-n | --dry-run] <name>…​`
- `git remote [-v | --verbose] update [-p | --prune] [(<group> | <remote>)…​]`

