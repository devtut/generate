---
metaTitle: "Git Large File Storage (LFS)"
description: "Declare certain file types to store externally, Install LFS, Set LFS config for all clones"
---

# Git Large File Storage (LFS)



## Declare certain file types to store externally


A common workflow for using Git LFS is to declare which files are intercepted through a rules-based system, just like `.gitignore` files.

Much of time, wildcards are used to pick certain file-types to blanket track.

e.g. `git lfs track "*.psd"`

When a file matching the above pattern is added them committed, when it is then pushed to the remote, it will be uploaded separately, with a pointer replacing the file in the remote repository.

After a file has been tracked with lfs, your `.gitattributes` file will be updated accordingly. Github recommends committing your local `.gitattributes` file, rather than working with a global `.gitattributes` file, to help ensure you don't have any issues when working with different projects.



## Install LFS


Download and install, either via Homebrew, or from [website](https://git-lfs.github.com).

For Brew,<br />
`brew install git-lfs`<br />
`git lfs install`

Often you will also need to do some setup on the service that hosts your remote to allow it to work with lfs. This will be different for each host, but will likely just be checking a box saying you want to use git lfs.



## Set LFS config for all clones


To set LFS options that apply to all clones, create and commit a file named `.lfsconfig` at the repository root. This file can specify LFS options the same way as allowed in `.git/config`.

For example, to exclude a certain file from LFS fetches be default, create and commit `.lfsconfig` with the following contents:

```git
[lfs]
    fetchexclude = ReallyBigFile.wav

```



#### Remarks


[Git Large File Storage](https://git-lfs.github.com) (LFS) aims to avoid a limitation of the Git version control system, that it performs poorly when versioning large files, especially binaries. LFS solves this problem by storing the contents of such files on an external server, then instead committing just a text pointer to the path of those assets in the git object database.

Common file types that are stored via LFS tend to be compiled source; graphical assets, like PSDs and JPEGs; or 3D assets. This way resources used by projects can be managed in the same repository, rather than having to maintain a separate management system externally.

LFS was originally developed by GitHub ([https://github.com/blog/1986-announcing-git-large-file-storage-lfs)](https://github.com/blog/1986-announcing-git-large-file-storage-lfs)); however, Atlasssian had been working on a similar project at nearly the exact same time, called [git-lob](https://github.com/atlassian/git-lob). Soon these efforts were merged to avoid fragmentation in the industry.

