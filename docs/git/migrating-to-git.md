---
metaTitle: "Git - Migrating to Git"
description: "SubGit, Migrate from SVN to Git using Atlassian conversion utility, Migrate from Team Foundation Version Control (TFVC) to Git, Migrating Mercurial to Git, Migrate from SVN to Git using svn2git"
---

# Migrating to Git



## SubGit


[SubGit](http://www.subgit.com/remote-book.html#7) may be used to perform a one-time import of an SVN repository to git.

```git
$ subgit import --non-interactive --svn-url http://svn.my.co/repos/myproject myproject.git

```



## Migrate from SVN to Git using Atlassian conversion utility


Download the Atlassian conversion utility [here](https://bitbucket.org/atlassian/svn-migration-scripts/downloads/svn-migration-scripts.jar). This utility requires Java, so please ensure that you have the Java Runtime Environment [JRE](https://www.java.com/en/download/installed.jsp) installed on the machine you plan to do the conversion.

Use the command `java -jar svn-migration-scripts.jar verify` to check if your machine is missing any of the programs necessary to complete the conversion. Specifically, this command checks for the Git, subversion, and `git-svn` utilities. It also verifies that you are performing the migration on a case-sensitive file system. Migration to Git should be done on a case-sensitive file system to avoid corrupting the repository.

Next, you need to generate an authors file. Subversion tracks changes by the committer's username only. Git, however, uses two pieces of information to distinguish a user: a real name and an email address. The following command will generate a text file mapping the subversion usernames to their Git equivalents:

```git
java -jar svn-migration-scripts.jar authors <svn-repo> authors.txt

```

where `<svn-repo>` is the URL of the subversion repository you wish to convert. After running this command, the contributors' identification information will be mapped in `authors.txt`. The email addresses will be of the form `<username>@mycompany.com`. In the authors file, you will need to manually change each person's default name (which by default has become their username) to their actual names. Make sure to also check all of the email addresses for correctness before proceeding.

The following command will clone an svn repo as a Git one:

```git
git svn clone --stdlayout --authors-file=authors.txt <svn-repo> <git-repo-name>

```

where `<svn-repo>` is the same repository URL used above and `<git-repo-name>` is the folder name in the current directory to clone the repository into. There are a few considerations before using this command:

- The `--stdlayout` flag from above tells Git that you're using a standard layout with `trunk`, `branches`, and `tags` folders. Subversion repositories with non-standard layouts require you to specify the locations of the `trunk` folder, any/all `branch` folders, and the `tags` folder. This can be done by following this example: `git svn clone --trunk=/trunk --branches=/branches --branches=/bugfixes --tags=/tags --authors-file=authors.txt <svn-repo> <git-repo-name>`.
- This command could take many hours to complete depending on the size of your repo.
- To cut down the conversion time for large repositories, the conversion can be run directly on the server hosting the subversion repository in order to eliminate network overhead.

`git svn clone` imports the subversion branches (and trunk) as remote branches including subversion tags (remote branches prefixed with `tags/`). To convert these to actual branches and tags, run the following commands on a Linux machine in the order they are provided. After running them, `git branch -a` should show the correct branch names, and `git tag -l` should show the repository tags.

```git
git for-each-ref refs/remotes/origin/tags | cut -d / -f 5- | grep -v @ | while read tagname; do git tag $tagname origin/tags/$tagname; git branch -r -d origin/tags/$tagname; done
git for-each-ref refs/remotes | cut -d / -f 4- | grep -v @ | while read branchname; do git branch "$branchname" "refs/remotes/origin/$branchname"; git branch -r -d "origin/$branchname"; done

```

The conversion from svn to Git is now complete! Simply `push` your local repo to a server and you can continue to contribute using Git as well as having a completely preserved version history from svn.



## Migrate from Team Foundation Version Control (TFVC) to Git


You could migrate from team foundation version control to git by using an open source tool called Git-TF. Migration will also transfer your existing history by converting tfs checkins to git commits.

To put your solution into Git by using Git-TF follow these steps:

**Download Git-TF**

You can download (and install) Git-TF from Codeplex: [Git-TF @ Codeplex](https://gittf.codeplex.com/)

**Clone your TFVC solution**

Launch powershell (win) and type the command

```git
git-tf clone http://my.tfs.server.address:port/tfs/mycollection '$/myproject/mybranch/mysolution' --deep

```

The --deep switch is the keeyword to note as this tells Git-Tf to copy your checkin-history. You now have a local git repository in the folder from which you called your cloe command from.

**Cleanup**

- Add a .gitignore file. If you are using Visual Studio the editor can do this for you, otherwise you could do this manually by downloading a complete file from [github/gitignore](https://github.com/github/gitignore).
- RemoveTFS source control bindings from solution (remove all *.vssscc files). You could also modify your solution file by removing the GlobalSection(TeamFoundationVersionControl)......EndClobalSection

**Commit & Push**

Complete your conversion by committing and pushing your local repository to your remote.

```git
git add .
git commit -a -m "Coverted solution source control from TFVC to Git"

git remote add origin https://my.remote/project/repo.git

git push origin master

```



## Migrating Mercurial to Git


One can use the following methods in order to import a `Mercurial` Repo into `Git`:

1. Using [fast export](https://github.com/frej/fast-export):

```git
cd
git clone git://repo.or.cz/fast-export.git
git init git_repo
cd git_repo
~/fast-export/hg-fast-export.sh -r /path/to/old/mercurial_repo
git checkout HEAD

```


<li>
<p>Using [Hg-Git](http://hg-git.github.io/):
A very detailed answer here: [https://stackoverflow.com/a/31827990/5283213](https://stackoverflow.com/a/31827990/5283213)</p>
</li>
<li>
<p>Using [GitHub's Importer](https://help.github.com/articles/about-github-importer/):
Follow the (detailed) instructions at [GitHub](https://github.com/new/import).</p>
</li>



## Migrate from SVN to Git using svn2git


[svn2git](https://github.com/nirvdrum/svn2git) is a Ruby wrapper around git's native SVN support through [git-svn](https://git-scm.com/docs/git-svn), helping you with migrating projects from Subversion to Git, keeping history (incl. trunk, tags and branches history).

**Examples**

To migrate a svn repository with the standard layout (ie. branches, tags and trunk at the root level of the repository):

```git
$ svn2git http://svn.example.com/path/to/repo

```

To migrate a svn repository which is not in standard layout:

```git
$ svn2git http://svn.example.com/path/to/repo --trunk trunk-dir --tags tags-dir --branches branches-dir

```

In case you do not want to migrate (or do not have) branches, tags or trunk you can use options `--notrunk`, `--nobranches`, and `--notags`.

For example, `$ svn2git http://svn.example.com/path/to/repo --trunk trunk-dir --notags --nobranches` will migrate only trunk history.

To reduce the space required by your new repository you may want to exclude any directories or files you once added while you should not have (eg. build directory or archives):

```git
$ svn2git http://svn.example.com/path/to/repo --exclude build --exclude '.*\.zip$'

```

**Post-migration optimization**

If you already have a few thousand of commits (or more) in your newly created git repository, you may want to reduce space used before pushing your repository on a remote. This can be done using the following command:

```git
$ git gc --aggressive

```

**Note:** The previous command can take up to several hours on large repositories (tens of thousand of commits and/or hundreds of megabytes of history).

