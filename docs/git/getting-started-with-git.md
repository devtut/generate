---
metaTitle: "Git - Getting started with Git"
description: "Create your first repository, then add and commit files, Clone a repository, Sharing code, Setting your user name and email, Setting up the upstream remote, Learning about a command, Set up SSH for Git, Git Installation"
---

# Getting started with Git



## Create your first repository, then add and commit files


At the command line, first verify that you have Git installed:

On all operating systems:

```git
git --version

```

On UNIX-like operating systems:

```git
which git

```

If nothing is returned, or the command is not recognized, you may have to install Git on your system by downloading and running the installer. See the [Git homepage](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git) for exceptionally clear and easy installation instructions.

After installing Git, [configure your username and email address](http://stackoverflow.com/documentation/git/218/getting-started-with-git/6939/setting-your-user-name-and-email). Do this **before** making a commit.

Once Git is installed, navigate to the directory you want to place under version control and create an empty Git repository:

```git
git init

```

This creates a hidden folder, `.git`, which contains the plumbing needed for Git to work.

Next, check what files Git will add to your new repository; this step is worth special care:

```git
git status

```

Review the resulting list of files; you can tell Git which of the files to place into version control (avoid adding files with confidential information such as passwords, or files that just clutter the repo):

```git
git add <file/directory name #1> <file/directory name #2> < ... >

```

If all files in the list should be shared with everyone who has access to the repository, a single command will add everything in your current directory and its subdirectories:

```git
git add .

```

This will ["stage"](http://stackoverflow.com/documentation/git/244/staging) all files to be added to version control, preparing them to be committed in your first commit.

For files that you want never under version control, [create and populate a file named `.gitignore`](http://stackoverflow.com/documentation/git/245/ignoring-files-and-folders#t=201607281145105871327) before running the `add` command.

Commit all the files that have been added, along with a commit message:

```git
git commit -m "Initial commit"

```

This creates a new [commit](http://stackoverflow.com/documentation/git/323/committing) with the given message. A commit is like a save or snapshot of your entire project. You can now [push](http://stackoverflow.com/documentation/git/2600/pushing), or upload, it to a remote repository, and later you can jump back to it if necessary.<br />
If you omit the `-m` parameter, your default editor will open and you can edit and save the commit message there.

Adding a remote

To add a new remote, use the `git remote add` command on the terminal, in the directory your repository is stored at.

The `git remote add` command takes two arguments:

1. A remote name, for example, `origin`
1. A remote URL, for example, `https://<your-git-service-address>/user/repo.git`

```

   git remote add origin https://<your-git-service-address>/owner/repository.git

```

NOTE: Before adding the remote you have to create the required repository in your git service,
You'll be able to push/pull commits after adding your remote.



## Clone a repository


The `git clone` command is used to copy an existing Git repository from a server to the local machine.

For example, to clone a GitHub project:

```git
cd <path where you'd like the clone to create a directory>
git clone https://github.com/username/projectname.git

```

To clone a BitBucket project:

```git
cd <path where you'd like the clone to create a directory>
git clone https://yourusername@bitbucket.org/username/projectname.git

```

This creates a directory called `projectname` on the local machine, containing all the files in the remote Git repository. This includes source files for the project, as well as a `.git` sub-directory which contains the entire history and configuration for the project.

To specify a different name of the directory, e.g. `MyFolder`:

```git
git clone https://github.com/username/projectname.git MyFolder

```

Or to clone in the current directory:

```git
git clone https://github.com/username/projectname.git .

```

Note:

<li>
When cloning to a specified directory, the directory must be empty or non-existent.
</li>
<li>
You can also use the `ssh` version of the command:

```git
git clone git@github.com:username/projectname.git

```


</li>

The `https` version and the `ssh` version are equivalent. However, some hosting services such as GitHub [recommend](https://help.github.com/articles/set-up-git/#next-steps-authenticating-with-github-from-git) that you use `https` rather than `ssh`.



## Sharing code


To share your code you create a repository on a remote server to which you will copy your local repository.

To minimize the use of space on the remote server you create a bare repository: one which has only the `.git` objects and doesn't create a working copy in the filesystem. As a bonus you [set this remote](https://stackoverflow.com/documentation/git/218/getting-started-with-git/2627/setting-up-the-upstream-remote#t=201706051652194109347) as an upstream server to easily share updates with other programmers.

On the remote server:

```git
git init --bare /path/to/repo.git

```

On the local machine:

```git
git remote add origin ssh://username@server:/path/to/repo.git

```

(Note that `ssh:` is just one possible way of accessing the remote repository.)

Now copy your local repository to the remote:

```git
git push --set-upstream origin master

```

Adding `--set-upstream` (or `-u`) created an upstream (tracking) reference which is used by argument-less Git commands, e.g. `git pull`.



## Setting your user name and email


```git
You need to set who you are *before* creating any commit.  That will allow commits to have the right author name and email associated to them.

```

**It has nothing to do with authentication when pushing to a remote repository** (e.g. when pushing to a remote repository using your GitHub, BitBucket, or GitLab account)

To declare that identity for **all** repositories, use `git config --global`<br />
This will store the setting in your user's `.gitconfig` file: e.g. `$HOME/.gitconfig` or for Windows, `%USERPROFILE%\.gitconfig`.

```git
git config --global user.name "Your Name"
git config --global user.email mail@example.com

```

To declare an identity for a single repository, use `git config` inside a repo.<br />
This will store the setting inside the individual repository, in the file `$GIT_DIR/config`. e.g. `/path/to/your/repo/.git/config`.

```git
cd /path/to/my/repo
git config user.name "Your Login At Work"
git config user.email mail_at_work@example.com

```

Settings stored in a repository's config file will take precedence over the global config when you use that repository.

Tips: if you have different identities (one for open-source project, one at work, one for private repos, ...), and you don't want to forget to set the right one for each different repos you are working on:

<li>
**Remove a global identity**

```git
git config --global --remove-section user.name
git config --global --remove-section user.email

```


</li>

<li>
To force git to look for your identity only within a repository's settings, not in the global config:

```git
  git config --global user.useConfigOnly true

```


</li>

That way, if you forget to set your `user.name` and `user.email` for a given repository and try to make a commit, you will see:

```git
no name was given and auto-detection is disabled
no email was given and auto-detection is disabled

```



## Setting up the upstream remote


If you have cloned a fork (e.g. an open source project on Github) you may not have push access to the upstream repository, so you need both your fork but be able to fetch the upstream repository.

First check the remote names:

```git
$ git remote -v
origin    https://github.com/myusername/repo.git (fetch)
origin    https://github.com/myusername/repo.git (push)
upstream  # this line may or may not be here

```

If `upstream` is there already (it is on **some** Git versions) you need to set the URL (currently it's empty):

```git
$ git remote set-url upstream https://github.com/projectusername/repo.git

```

If the upstream is **not** there, or if you also want to add a friend/colleague's fork (currently they do not exist):

```git
$ git remote add upstream https://github.com/projectusername/repo.git
$ git remote add dave https://github.com/dave/repo.git

```



## Learning about a command


To get more information about any git command – i.e. details about what the command does, available options and other documentation – use the `--help` option or the `help` command.

For example, to get all available information about the `git diff` command, use:

```git
git diff --help
git help diff

```

Similarly, to get all available information about the `status` command, use:

```git
git status --help
git help status

```

If you only want a quick help showing you the meaning of the most used command line flags, use `-h`:

```git
git checkout -h

```



## Set up SSH for Git


If you are using **Windows** open [Git Bash](https://git-for-windows.github.io/).
If you are using **Mac** or **Linux** open your Terminal.

Before you generate an SSH key, you can check to see if you have any existing SSH keys.

List the contents of your  `~/.ssh` directory:

```git
$ ls -al ~/.ssh 
# Lists all the files in your ~/.ssh directory

```

Check the directory listing to see if you already have a public SSH key. By default the filenames of the public keys are one of the following:

```git
id_dsa.pub
id_ecdsa.pub
id_ed25519.pub
id_rsa.pub

```

If you see an existing public and private key pair listed that you would like to use on your Bitbucket, GitHub (or similar) account you can copy the contents of the `id_*.pub` file.

If not, you can create a new public and private key pair with the following command:

```git
$ ssh-keygen

```

Press the Enter or Return key to accept the default location.
Enter and re-enter a passphrase when prompted, or leave it empty.

Ensure your SSH key is added to the ssh-agent. Start the ssh-agent in the background if it's not already running:

```git
$ eval "$(ssh-agent -s)"

```

Add you SSH key to the ssh-agent. Notice that you'll need te replace `id_rsa` in the command with the name of your **private key file**:

```git
$ ssh-add ~/.ssh/id_rsa

```

If you want to change the upstream of an existing repository from HTTPS to SSH you can run the following command:

```git
$ git remote set-url origin ssh://git@bitbucket.server.com:7999/projects/your_project.git

```

In order to clone a new repository over SSH  you can run the following command:

```git
$ git clone ssh://git@bitbucket.server.com:7999/projects/your_project.git

```



## Git Installation


Let’s get into using some Git. First things first—you have to install it. You can get it a number of ways; the two major ones are to install it from source or to install an existing package for your platform.

**Installing from Source**

If you can, it’s generally useful to install Git from source, because you’ll get the most recent version. Each version of Git tends to include useful UI enhancements, so getting the latest version is often the best route if you feel comfortable compiling software from source. It is also the case that many Linux distributions contain very old packages; so unless you’re on a very up-to-date distro or are using backports, installing from source may be the best bet.

To install Git, you need to have the following libraries that Git depends on: curl, zlib, openssl, expat, and libiconv. For example, if you’re on a system that has yum (such as Fedora) or apt-get (such as a Debian based system), you can use one of these commands to install all of the dependencies:

```git
$ yum install curl-devel expat-devel gettext-devel \
  openssl-devel zlib-devel

$ apt-get install libcurl4-gnutls-dev libexpat1-dev gettext \
  libz-dev libssl-dev

```

When you have all the necessary dependencies, you can go ahead and grab the latest snapshot from the Git web site:

[http://git-scm.com/download](http://git-scm.com/download)
Then, compile and install:

```git
$ tar -zxf git-1.7.2.2.tar.gz
$ cd git-1.7.2.2
$ make prefix=/usr/local all
$ sudo make prefix=/usr/local install

```

After this is done, you can also get Git via Git itself for updates:

```git
$ git clone git://git.kernel.org/pub/scm/git/git.git

```

**Installing on Linux**

If you want to install Git on Linux via a binary installer, you can generally do so through the basic package-management tool that comes with your distribution. If you’re on Fedora, you can use yum:

```git
$ yum install git

```

Or if you’re on a Debian-based distribution like Ubuntu, try apt-get:

```git
$ apt-get install git

```

**Installing on Mac**

There are three easy ways to install Git on a Mac. The easiest is to use the graphical Git installer, which you can download from the SourceForge page.

[http://sourceforge.net/projects/git-osx-installer/](http://sourceforge.net/projects/git-osx-installer/)

Figure 1-7. Git OS X installer.
The other major way is to install Git via MacPorts ([http://www.macports.org)](http://www.macports.org)). If you have MacPorts installed, install Git via

```git
$ sudo port install git +svn +doc +bash_completion +gitweb

```

You don’t have to add all the extras, but you’ll probably want to include +svn in case you ever have to use Git with Subversion repositories (see Chapter 8).

Homebrew ([http://brew.sh/)](http://brew.sh/)) is another alternative to install Git. If you have Homebrew installed, install Git via

```git
$ brew install git

```

**Installing on Windows**

Installing Git on Windows is very easy. The msysGit project has one of the easier installation procedures. Simply download the installer exe file from the GitHub page, and run it:

```git
http://msysgit.github.io

```

After it’s installed, you have both a command-line version (including an SSH client that will come in handy later) and the standard GUI.

**Note on Windows usage:** you should use Git with the provided msysGit shell (Unix style), it allows to use the complex lines of command given in this book. If you need, for some reason, to use the native Windows shell / command line console, you have to use double quotes instead of single quotes (for parameters with spaces in them) and you must quote the parameters ending with the circumflex accent (^) if they are last on the line, as it is a continuation symbol in Windows.



#### Remarks


Git is a free, distributed version control system which allows programmers to keep track of code changes, via "snapshots" (commits), in its current state. Utilizing commits allows programmers to test, debug, and create new features collaboratively. All commits are kept in what is known as a "Git Repository" that can be hosted on your computer, private servers, or open source websites, such at Github.

Git also allows for users to create new "branches" of the code, which allows different versions of the code to live alongside each other. This enables scenarios where one branch contains the most recent stable version, a different branch contains a set of new features being developed, and a yet another branch contains a different set of features. Git makes the process of creating these branches, and then subsequently merging them back together, nearly painless.

Git has 3 different "areas" for your code:

- **Working directory**: The area that you will be doing all of your work in (creating, editing, deleting, and organizing files)
- **Staging area**: The area where you will list the changes that you have made to the working directory
- **Repository**: Where Git permanently stores the changes you have made as different versions of the project

Git was originally created for managing the Linux kernel source. By making them easier, it encourages small commits, forking of projects and merging between forks, and having lots of short-lived branches.

The biggest change for people who are used to CVS or Subversion is that every checkout contains not only the source tree, but also the whole history of the project. Common operations like diffing of revisions, checking out older revisions, committing (to your local history), creating a branch, checking out a different branch, merging branches or patch files can all be done locally without having to communicate with a central server. Thus the biggest source of latency and unreliability is removed. Communicating with the "upstream" repository is only needed to get the latest changes, and to publish your local changes to other developers. This turns what was previously a technical constraint (whoever has the repository owns the project) into an organisational choice (your "upstream" is whomever you choose to sync with).

