---
metaTitle: "Bundles"
description: "Creating a git bundle on the local machine and using it on another"
---

# Bundles



## Creating a git bundle on the local machine and using it on another


Sometimes you may want maintain versions of a git repository on machines that  have no network connection. Bundles allow you to package git objects and references in a repository on one machine and import those into a repository on another.

```git
git tag 2016_07_24
git bundle create changes_between_tags.bundle [some_previous_tag]..2016_07_24

```

Somehow transfer the **changes_between_tags.bundle** file to the remote machine; e.g., via thumb drive. Once you have it there:

```git
git bundle verify changes_between_tags.bundle  # make sure bundle arrived intact
git checkout [some branch]       # in the repo on the remote machine
git bundle list-heads changes_between_tags.bundle # list the references in the bundle
git pull changes_between_tags.bundle [reference from the bundle, e.g. last field from the previous output]

```

The reverse is also possible. Once you've made changes on the remote repository you can bundle up the deltas; put the changes on, e.g., a thumb drive, and merge them back into the local repository so the two can stay in sync without requiring direct `git`, `ssh`, `rsync`, or `http` protocol access between the machines.



#### Remarks


The key to making this work is to begin by cloning a bundle that starts from the beginning of the repo history:

```

git bundle create initial.bundle master
 git tag -f some_previous_tag master  # so the whole repo does not have to go each time

```

getting that initial bundle to the remote machine; and

```

git clone -b master initial.bundle remote_repo_name

```

