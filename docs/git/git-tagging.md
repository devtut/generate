---
metaTitle: "Git Tagging"
description: "Listing all available tags, Create and push tag(s) in GIT"
---

# Git Tagging


Like most Version Control Systems (VCSs), `Git` has the ability to `tag` specific points in history as being important. Typically people use this functionality to mark release points (`v1.0`, and so on).



## Listing all available tags


Using the command `git tag` lists out all available tags:

```git
$ git tag
<output follows>
v0.1
v1.3

```

> 
**Note**: the `tags` are output in an **alphabetical** order.


One may also `search` for available `tags`:

```git
$ git tag -l "v1.8.5*"
<output follows>
v1.8.5
v1.8.5-rc0
v1.8.5-rc1
v1.8.5-rc2
v1.8.5-rc3
v1.8.5.1
v1.8.5.2
v1.8.5.3
v1.8.5.4
v1.8.5.5

```



## Create and push tag(s) in GIT


**Create a tag:**

<li>
To create a tag on your current branch:

```git
git tag < tagname >

```


This will create a local `tag` with the current state of the branch you are on.
</li>
<li>
To create a tag with some commit:

```git
git tag tag-name commit-identifier

```


This will create a local `tag` with the commit-identifier of the branch you are on.
</li>

**Push a commit in GIT:**

<li>
Push an individual tag:

```git
git push origin tag-name

```


</li>
<li>
Push all the tags at once

```git
git push origin --tags

```


</li>



#### Syntax


<li>
<p>git tag [-a | -s | -u < keyid >] [-f] [-m < msg > | -F < file >]
< tagname > [< commit > | < object >]</p>
</li>
<li>
git tag -d <  tagname​  >
</li>
<li>
<p>git tag [-n[< num >]] -l [--contains < commit >] [--contains < commit >]
[--points-at < object >] [--column[=< options >] | --no-column]
[--create-reflog] [--sort=< key >] [--format=< format >]
[--[no-]merged [< commit >]] [< pattern >…​]</p>
</li>
<li>
git tag -v [--format=< format >] < tagname >…​
</li>

