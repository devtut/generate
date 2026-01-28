---
metaTitle: "Node.js - Installing Node.js"
description: "Using Node Version Manager (nvm), Installing Node.js on Mac using package manager, Installing Node.js on Windows, Install Node.js on Ubuntu, Installing Node.js with n, Install Node.js From Source with APT package manager, Installing Node.js on Raspberry PI, Installing with Node Version Manager under Fish Shell with Oh My Fish!, Install Node.js from source on Centos, RHEL and Fedora, Installing using MacOS X Installer"
---

# Installing Node.js




## Using Node Version Manager (nvm)


[Node Version Manager](https://github.com/creationix/nvm), otherwise known as nvm, is a bash script that simplifies the management of multiple Node.js versions.

To install nvm, use the provided install script:

```js
$ curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.31.3/install.sh | bash

```

For windows there is a nvm-windows package with an installer. This [GithHub](https://github.com/coreybutler/nvm-windows) page has the details for installing and using the nvm-windows package.

After installing nvm, run "nvm on" from command line. This enables nvm to control the node versions.

**Note: You may need to restart your terminal for it to recognize the newly installed `nvm` command.**

Then install the latest Node version:

```js
$ nvm install node

```

You can also install a specific Node version, by passing the major, minor, and/or patch versions:

```js
$ nvm install 6
$ nvm install 4.2

```

To list the versions available for install:

```js
$ nvm ls-remote

```

You can then switch versions by passing the version the same way you do when installing:

```js
$ nvm use 5

```

You can set a specific version of Node that you installed to be the **default version** by entering:

```js
$ nvm alias default 4.2

```

To display a list of Node versions that are installed on your machine, enter:

```js
$ nvm ls

```

To use project-specific node versions, you can save the version in .nvmrc file. This way, starting to work with another project will be less error-prone after fetching it from its repository.

```js
$ echo "4.2" > .nvmrc
$ nvm use
Found '/path/to/project/.nvmrc' with version <4.2>
Now using node v4.2 (npm v3.7.3)

```

When Node is installed via nvm we don't have to use `sudo` to install global packages since they are installed in home folder. Thus `npm i -g http-server` works without any permission errors.



## Installing Node.js on Mac using package manager


### Homebrew

You can install Node.js using the [Homebrew](http://brew.sh) package manager.

Start by updating brew:

```js
brew update

```

You may need to change permissions or paths. It's best to run this before proceeding:

```js
brew doctor

```

Next you can install Node.js by running:

```js
brew install node

```

Once Node.js is installed, you can validate the version installed by running:

```js
node -v

```

### Macports

You can also install node.js through [Macports](https://www.macports.org/).

First update it to make sure the lastest packages are referenced:

```js
sudo port selfupdate

```

Then install nodejs and npm

```js
sudo port install nodejs npm

```

You can now run node through CLI directly by invoking `node`. Also, you can check your current node version with

```js
node -v

```



## Installing Node.js on Windows


**Standard installation**

All Node.js binaries, installers, and source files can be downloaded [here](https://nodejs.org/en/download/).

You can download just the `node.exe` runtime or use the Windows installer (`.msi`), which will also install `npm`, the recommended package manager for Node.js, and configure paths.

**Installation by package manager**

You can also install by package manager [**Chocolatey**](https://chocolatey.org/) (Software Management Automation).

```js
# choco install nodejs.install

```

More information about current version, you can find in the choco repository [here](https://chocolatey.org/packages/nodejs.install).



## Install Node.js on Ubuntu


### Using the apt package manager

```js
sudo apt-get update    
sudo apt-get install nodejs
sudo apt-get install npm
sudo ln -s /usr/bin/nodejs /usr/bin/node

# the node & npm versions in apt are outdated. This is how you can update them:
sudo npm install -g npm
sudo npm install -g n
sudo n stable # (or lts, or a specific version)

```

### Using the latest of specific version (e.g. LTS 6.x) directly from nodesource

```js
curl -sL https://deb.nodesource.com/setup_6.x | sudo -E bash -
apt-get install -y nodejs

```

Also, for the right way to install global npm modules, set the personal directory for them (eliminates the need for sudo and avoids EACCES errors):

```js
mkdir ~/.npm-global
echo "export PATH=~/.npm-global/bin:$PATH" >> ~/.profile
source ~/.profile
npm config set prefix '~/.npm-global'

```



## Installing Node.js with n


First, there is a really nice wrapper for setting up `n` on your system. Just run:

```js
curl -L https://git.io/n-install | bash

```

to install `n`. Then install binaries in a variety of ways:

**latest**

`n latest`

**stable**

`n stable`

**lts**

`n lts`

**Any other version**

`n <version>`

e.g. `n 4.4.7`

If this version is already installed, this command will activate that version.

**Switching versions**

`n` by itself will produce a selection list of installed binaries. Use up and down to find the one you want and Enter to activate it.



## Install Node.js From Source with APT package manager


**Prerequisites**

```js
sudo apt-get install build-essential
sudo apt-get install python

[optional]
sudo apt-get install git

```

**Get source and build**

```js
cd ~
git clone https://github.com/nodejs/node.git

```

**OR**
For the latest LTS Node.js version 6.10.2

```js
cd ~
wget https://nodejs.org/dist/v6.3.0/node-v6.10.2.tar.gz
tar -xzvf node-v6.10.2.tar.gz

```

Change to the source directory such as in `cd ~/node-v6.10.2`

```js
./configure
make
sudo make install

```



## Installing Node.js on Raspberry PI


To install v6.x update the packages

```js
curl -sL https://deb.nodesource.com/setup_6.x | sudo -E bash -

```

Using the apt package manager

```js
sudo apt-get install -y nodejs

```



## Installing with Node Version Manager under Fish Shell with Oh My Fish!


[Node Version Manager](https://github.com/creationix/nvm) (`nvm`) greatly simplifies the management of Node.js versions, their installation, and removes the need for `sudo` when dealing with packages (e.g. `npm install ...`). [Fish Shell](https://fishshell.com/) (`fish`) "<em>is a smart and user-friendly command line
shell for OS X, Linux, and the rest of the family</em>" that is a popular alternative among programmers to common shells such as `bash`. Lastly, [Oh My Fish](https://github.com/oh-my-fish/oh-my-fish) (`omf`) allows for customizing and installing packages within Fish shell.

**This guide assumes you are already using Fish as your shell**.

**Install nvm**

`curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.31.4/install.sh | bash`

**Install Oh My Fish**

`curl -L https://github.com/oh-my-fish/oh-my-fish/raw/master/bin/install | fish`

(Note: You will be prompted to restart your terminal at this point. Go ahead and do so now.)

**Install plugin-nvm for Oh My Fish**

We will install [plugin-nvm](https://github.com/derekstavis/plugin-nvm) via Oh My Fish to expose `nvm` capabilities within the Fish shell:

`omf install nvm`

**Install Node.js with Node Version Manager**

You are now ready to use `nvm`. You may install and use the version of Node.js of your liking. Some examples:

- Install the most recent Node version: `nvm install node`
- Install 6.3.1 specifically: `nvm install 6.3.1`
- List installed verisons: `nvm ls`
- Switch to a previously installed 4.3.1: `nvm use 4.3.1`

**Final Notes**

Remember again, that we no longer need `sudo` when dealing with Node.js using this method! Node versions, packages, and so on are installed in your home directory.



## Install Node.js from source on Centos, RHEL and Fedora


**Prerequisites**

- git
- `clang` and `clang++` 3.4^ or `gcc` and `g++` 4.8^
- Python 2.6 or 2.7
- GNU Make 3.81^

**Get source**

**Node.js v6.x LTS**

```js
git clone -b v6.x https://github.com/nodejs/node.git

```

**Node.js v7.x**

```js
git clone -b v7.x https://github.com/nodejs/node.git

```

**Build**

```js
cd node
./configure
make -jX
su -c make install

```

**X - the number of processor cores, greatly speeds up the build**

**Cleanup [Optional]**

```js
cd 
rm -rf node

```



## Installing using MacOS X Installer


You can find the installers on [Node.js download page](https://nodejs.org/en/download/). Normally, Node.js recommends two versions of Node, the LTS version (long term support) and the current version (latest release). If you are new to Node, just go for the LTS and then click the `Macintosh Installer` button to download the package.

If you want to find other NodeJS releases, go [here](https://nodejs.org/en/download/releases/), choose your release then click download. From the download page, look for a file with extension `.pkg`.

Once you downloaded the file (with extension `.pkg` ofcourse), double click it to install. The installer packed with `Node.js` and `npm`, by default, the package will install both but you can customize which one to install by clicking the `customize` button in the `Installation Type` step. Other than that, just follow the installation instructions, it's pretty straightforward.

### Check if Node is installed

Open `terminal` (if you don't know how to open your terminal, look at this [wikihow](http://www.wikihow.com/Get-to-the-Command-Line-on-a-Mac)). In the terminal type `node --version` then enter. Your terminal will look like this if Node is installed:

```js
$ node --version
v7.2.1

```

The `v7.2.1` is your Node.js version, if you receive the message `command not found: node` instead of that, then it's mean there is a problem with your installation.

