---
metaTitle: "Subtrees"
description: "Create, Pull, and Backport Subtree"
---

# Subtrees



## Create, Pull, and Backport Subtree


### Create Subtree

Add a new remote called `plugin` pointing to the plugin's repository:

```git
git remote add plugin https://path.to/remotes/plugin.git

```

Then Create a subtree specifying the new folder prefix `plugins/demo`. `plugin` is the remote name, and `master` refers to the master branch on the subtree's repository:

```git
git subtree add --prefix=plugins/demo plugin master

```

### Pull Subtree Updates

Pull normal commits made in plugin:

```git
git subtree pull --prefix=plugins/demo plugin master

```

### Backport Subtree Updates

<li>
Specify commits made in superproject to be backported:

```git
git commit -am "new changes to be backported"

```


</li>
<li>
Checkout new branch for merging, set to track subtree repository:

```git
git checkout -b backport plugin/master

```


</li>
<li>
Cherry-pick backports:

```git
git cherry-pick -x --strategy=subtree master

```


</li>
<li>
Push changes back to plugin source:

```git
git push plugin backport:master

```


</li>



#### Syntax


- `git subtree add -P <prefix> <commit>`
- `git subtree add -P <prefix> <repository> <ref>`
- `git subtree pull -P <prefix> <repository> <ref>`
- `git subtree push -P <prefix> <repository> <ref>`
- `git subtree merge -P <prefix> <commit>`
- `git subtree split -P <prefix> [OPTIONS] [<commit>]`



#### Remarks


This is an alternative to using a [`submodule`](https://git-scm.com/docs/git-submodule)

