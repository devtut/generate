---
metaTitle: "Git - git-tfs"
description: "git-tfs clone, git-tfs clone from bare git repository, git-tfs install via Chocolatey, git-tfs Check In, git-tfs push"
---

# git-tfs



## git-tfs clone


This will create a folder with the same name as the project, i.e. /My.Project.Name

```git
$ git tfs clone http://tfs:8080/tfs/DefaultCollection/ $/My.Project.Name

```



## git-tfs clone from bare git repository


Cloning from a git repository is ten times faster than cloning directly from TFVS and works well in a team environment. At least one team member will have to create the bare git repository by doing the regular git-tfs clone first. Then the new repository can be bootstrapped to work with TFVS.

```git
$ git clone x:/fileshare/git/My.Project.Name.git 
$ cd My.Project.Name 
$ git tfs bootstrap 
$ git tfs pull

```



## git-tfs install via Chocolatey


The following assumes you will use kdiff3 for file diffing and although not essential it is a good idea.

```git
C:\> choco install kdiff3

```

Git can be installed first so you can state any parameters you wish. Here all the Unix tools are also installed and 'NoAutoCrlf' means checkout as is, commit as is.

```git
C:\> choco install git -params '"/GitAndUnixToolsOnPath /NoAutoCrlf"'

```

This is all you really need to be able to install git-tfs via chocolatey.

```git
C:\> choco install git-tfs

```



## git-tfs Check In


Launch the Check In dialog for TFVS.

```git
$ git tfs checkintool

```

This will take all of your local commits and create a single check-in.



## git-tfs push


Push all local commits to the TFVS remote.

```git
$ git tfs rcheckin

```

Note: this will fail if Check-in Notes are required. These can be bypassed by adding `git-tfs-force: rcheckin` to the commit message.



#### Remarks


[Git-tfs](http://git-tfs.com/) is a third party tool to connect a Git repository to a Team Foundation Server (“TFS”) repository.

Most remote TFVS instances will request your credentials on every interaction and installing Git-Credential-Manager-for-Windows may not help. It can be overcome by adding your **name** and **password** to your `.git/config`

```git
[tfs-remote "default"]
  url = http://tfs.mycompany.co.uk:8080/tfs/DefaultCollection/
  repository = $/My.Project.Name/
  username = me.name
  password = My733TPwd

```

