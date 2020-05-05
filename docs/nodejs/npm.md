---
metaTitle: "Node.js - npm"
description: "Installing packages, Uninstalling packages, Setting up a package configuration, Running scripts, Basic semantic versioning, Publishing a package, Removing extraneous packages, Scopes and repositories, Listing currently installed packages, Updating npm and packages, Locking modules to specific versions, Setting up for globally installed packages, Linking projects for faster debugging and development"
---

# npm


Node Package Manager (npm) provides following two main functionalities: Online repositories for node.js packages/modules which are searchable on search.nodejs.org. Command line utility to install Node.js packages, do version management and dependency management of Node.js packages.



## Installing packages


### Introduction

Package is a term used by npm to denote tools that developers can use for their projects. This includes everything from libraries and frameworks such as jQuery and AngularJS  to task runners such as Gulp.js. The packages will come in a folder typically called `node_modules`, which will also contain a `package.json` file. This file contains information regarding all the packages including any dependencies, which are additional modules needed to use a particular package.

Npm uses the command line to both install and manage packages, so users attempting to use npm should be familiar with basic commands on their operating system i.e.: traversing directories as well as being able to see the contents of directories.

### Installing NPM

Note that in order to install packages, you must have NPM installed.

The recommended way to install NPM is to use one of the installers from the [Node.js download page](https://nodejs.org/en/download/).  You can check to see if you already have node.js installed by running either the `npm -v` or the `npm version` command.

After installing NPM via the Node.js installer, be sure to check for updates.  This is because NPM gets updated more frequently than the Node.js installer.  To check for updates run the following command:

```js
npm install npm@latest -g

```

### How to install packages

To install one or more packages use the following:

```js
npm install <package-name>
# or
npm i <package-name>...

# e.g. to install lodash and express
npm install lodash express

```

> 
**Note**: This will install the package in the directory that the command line is currently in, thus it is important to check whether the appropriate directory has been chosen


If you already have a `package.json` file in your current working directory and dependencies are defined in it, then `npm install` will automatically resolve and install all dependencies listed in the file. You can also use the shorthand version of the `npm install` command which is: `npm i`

If you want to install a specific version of a package use:

```js
npm install <name>@<version>

# e.g. to install version 4.11.1 of the package lodash
npm install lodash@4.11.1

```

If you want to install a version which matches a specific version range use:

```js
npm install <name>@<version range>

# e.g. to install a version which matches "version >= 4.10.1" and "version < 4.11.1"
# of the package lodash
npm install lodash@">=4.10.1 <4.11.1"

```

If you want to install the latest version use:

```js
npm install <name>@latest

```

The above commands will search for packages in the central `npm` repository at [npmjs.com](https://www.npmjs.com/). If you are not looking to install from the `npm` registry, other options are supported, such as:

```js
# packages distributed as a tarball
npm install <tarball file>
npm install <tarball url>

# packages available locally
npm install <local path>

# packages available as a git repository
npm install <git remote url>

# packages available on GitHub
npm install <username>/<repository>

# packages available as gist (need a package.json)
npm install gist:<gist-id>

# packages from a specific repository
npm install --registry=http://myreg.mycompany.com <package name>

# packages from a related group of packages 
# See npm scope
npm install @<scope>/<name>(@<version>)

# Scoping is useful for separating private packages hosted on private registry from
# public ones by setting registry for specific scope
npm config set @mycompany:registry http://myreg.mycompany.com
npm install @mycompany/<package name>

```

Usually, modules will be installed locally in a folder named `node_modules`, which can be found in your current working directory. This is the directory `require()` will use to load modules in order to make them available to you.

If you already created a `package.json` file, you can use the `--save` (shorthand `-S`) option or one of its variants to automatically add the installed package to your `package.json` as a dependency. If someone else installs your package, `npm` will automatically read dependencies from the `package.json` file and install the listed versions. Note that you can still add and manage your dependencies by editing the file later, so it's usually a good idea to keep track of dependencies, for example using:

```js
npm install --save <name> # Install dependencies 
# or
npm install -S <name> # shortcut version --save 
# or
npm i -S <name>

```

In order to install packages and save them only if they are needed for development, not for running them, not if they are needed for the application to run, follow the following command:

```js
npm install --save-dev <name> # Install dependencies for development purposes
# or
npm install -D <name> # shortcut version --save-dev
# or
npm i -D <name>

```

### Installing dependencies

Some modules do not only provide a library for you to use, but they also provide one or more binaries which are intended to be used via the command line. Although you can still install those packages locally, it is often preferred to install them globally so the command-line tools can be enabled. In that case, `npm` will automatically link the binaries to appropriate paths (e.g. `/usr/local/bin/<name>`) so they can be used from the command line. To install a package globally, use:

```js
npm install --global <name>
# or
npm install -g <name>
# or
npm i -g <name>

# e.g. to install the grunt command line tool
npm install -g grunt-cli

```

If you want to see a list of all the installed packages and their associated versions in the current workspace, use:

```js
npm list
npm list <name>

```

Adding an optional name argument can check the version of a specific package.

**Note:** If you run into permission issues while trying to install an npm module globally, resist the temptation to issue a `sudo npm install -g ...` to overcome the issue. Granting third-party scripts to run on your system with elevated privileges is dangerous. The permission issue might mean that you have an issue with the way `npm` itself was installed. If you're interested in installing Node in sandboxed user environments, you might want to try using [nvm](https://github.com/creationix/nvm).

If you have build tools, or other development-only dependencies (e.g. Grunt), you might not want to have them bundled with the application you deploy. If that's the case, you'll want to have it as a development dependency, which is listed in the `package.json` under `devDependencies`. To install a package as a development-only dependency, use `--save-dev` (or `-D`).

```js
npm install --save-dev <name> // Install development dependencies which is not included in production 
# or
npm install -D <name>

```

You will see that the package is then added to the `devDependencies` of your `package.json`.

To install dependencies of a downloaded/cloned node.js project, you can simply use

```js
npm install
# or
npm i

```

npm will automatically read the dependencies from `package.json` and install them.

### NPM Behind A Proxy Server

If your internet access is through a proxy server, you might need to modify npm install commands that access remote repositories. npm uses a configuration file which can be updated via command line:

```js
npm config set

```

You can locate your proxy settings from your browser's settings panel. Once you have obtained the proxy settings (server URL, port, username and password); you need to configure your npm configurations as follows.

```js
$ npm config set proxy http://<username>:<password>@<proxy-server-url>:<port>
$ npm config set https-proxy http://<username>:<password>@<proxy-server-url>:<port>

```

`username`, `password`, `port` fields are optional. Once you have set these, your `npm install`, `npm i -g` etc. would work properly.



## Uninstalling packages


To uninstall one or more locally installed packages, use:

```js
npm uninstall <package name>

```

The uninstall command for npm has five aliases that can also be used:

```js
npm remove <package name>
npm rm <package name>
npm r <package name>

npm unlink <package name>
npm un <package name>

```

If you would like to remove the package from the `package.json` file as part of the uninstallation, use the `--save` flag (shorthand: `-S`):

```js
npm uninstall --save <package name>
npm uninstall -S <package name>

```

For a development dependency, use the `--save-dev` flag (shorthand: `-D`):

```js
npm uninstall --save-dev <package name>
npm uninstall -D <package name>

```

For an optional dependency, use the `--save-optional` flag (shorthand: `-O`):

```js
npm uninstall --save-optional <package name>
npm uninstall -O <package name>

```

For packages that are installed globally use the `--global` flag (shorthand: `-g`):

```js
npm uninstall -g <package name>

```



## Setting up a package configuration


Node.js package configurations are contained in a file called `package.json` that you can find at the root of each project. You can setup a brand new configuration file by calling:

```js
npm init

```

That will try to read the current working directory for Git repository information (if it exists) and environment variables to try and autocomplete some of the placeholder values for you. Otherwise, it will provide an input dialog for the basic options.

If you'd like to create a `package.json` with default values use:

```js
npm init --yes
# or
npm init -y 

```

If you're creating a `package.json` for a project that you are not going to be publishing as an npm package (i.e. solely for the purpose of rounding up your dependencies), you can convey this intent in your `package.json` file:

1. Optionally set the `private` property to true to prevent accidental publishing.
1. Optionally set the `license` property to "UNLICENSED" to deny others the right to use your package.

To install a package and automatically save it to your `package.json`, use:

```js
npm install --save <package>

```

The package and associated metadata (such as the package version) will appear in your dependencies. If you save if as a development dependency (using `--save-dev`), the package will instead appear in your `devDependencies`.

With this bare-bones `package.json`, you will encounter warning messages when installing or upgrading packages, telling you that you are missing a description and the repository field. While it is safe to ignore these messages, you can get rid of them by opening the package.json in any text editor and adding the following lines to the JSON object:

```js
[...]
"description": "No description",
"repository": {
  "private": true
},
[...]

```



## Running scripts


You may define scripts in your `package.json`, for example:

```js
{
  "name": "your-package",
  "version": "1.0.0",
  "description": "",
  "main": "index.js",
  "author": "",
  "license": "ISC",
  "dependencies": {},
  "devDependencies": {},
  "scripts": {
    "echo": "echo hello!"
  }
}


```

To run the `echo` script, run `npm run echo` from the command line. Arbitrary scripts, such as `echo` above, have to be be run with `npm run <script name>`. npm also  has a number of official scripts that it runs at certain stages of the package's life (like `preinstall`). See [here](https://docs.npmjs.com/misc/scripts) for the entire overview of how npm handles script fields.

npm scripts are used most often for things like starting a server, building the project, and running tests. Here's a more realistic example:

```

 "scripts": {
    "test": "mocha tests",
    "start": "pm2 start index.js"
  }

```

In the `scripts` entries, command-line programs like `mocha` will work when installed either globally or locally. If the command-line entry does not exist in the system PATH, npm will also check your locally installed packages.

If your scripts become very long, they can be split into parts, like this:

```

 "scripts": {
    "very-complex-command": "npm run chain-1 && npm run chain-2",
    "chain-1": "webpack",
    "chain-2": "node app.js"
  }

```



## Basic semantic versioning


Before publishing a package you have to version it. npm supports [semantic versioning](http://semver.org/), this means there are **patch, minor and major** releases.

For example, if your package is at version 1.2.3 to change version you have to:

1. patch release: `npm version patch` => 1.2.4
1. minor release: `npm version minor` => 1.3.0
1. major release: `npm version major` => 2.0.0

You can also specify a version directly with:

`npm version 3.1.4` => 3.1.4

When you set a package version using one of the npm commands above, npm will modify the version field of the package.json file, commit it, and also create a new Git tag with the version prefixed with a "v", as if you've issued the command:

`git tag v3.1.4`

Unlike other package managers like Bower, the npm registry doesn't rely on Git tags being created for every version. But, if you like using tags, you should remember to push the newly created tag after bumping the package version:

`git push origin master` (to push the change to package.json)

`git push origin v3.1.4` (to push the new tag)

Or you can do this in one swoop with:

`git push origin master --tags`



## Publishing a package


First, make sure that you have configured your package (as said in [Setting up a package configuration](http://stackoverflow.com/documentation/node.js/482/npm/2257/setting-up-a-package-configuration)​). Then, you have to be logged in to npmjs.

If you already have a npm user

```js
npm login

```

If you don't have a user

```js
npm adduser

```

To check that your user is registered in the current client

```js
npm config ls

```

After that, when your package is ready to be published use

```js
npm publish

```

And you are done.

If you need to publish a new version, ensure that you update your package version, as stated in [Basic semantic versioning](http://stackoverflow.com/documentation/node.js/482/npm/2178/basic-semantic-versionini). Otherwise, `npm` will not let you publish the package.

```js
{
    name: "package-name",
    version: "1.0.4"
}

```



## Removing extraneous packages


To remove extraneous packages (packages that are installed but not in dependency list) run the following command:

```js
npm prune

```

To remove all `dev` packages add `--production` flag:

```js
npm prune --production

```

[More on it](https://docs.npmjs.com/cli/prune)



## Scopes and repositories


```js
# Set the repository for the scope "myscope"
npm config set @myscope:registry http://registry.corporation.com

# Login at a repository and associate it with the scope "myscope"
npm adduser --registry=http://registry.corporation.com --scope=@myscope

# Install a package "mylib" from the scope "myscope"
npm install @myscope/mylib

```

If the name of your own package starts with `@myscope` and the scope "myscope" is associated with a different repository, `npm publish` will upload your package to that repository instead.

You can also persist these settings in a `.npmrc` file:

```js
@myscope:registry=http://registry.corporation.com
//registry.corporation.com/:_authToken=xxxxxxxx-xxxx-xxxx-xxxxxxxxxxxxxxx

```

This is useful when automating the build on a CI server f.e.



## Listing currently installed packages


To generate a list (tree view) of currently installed packages, use

```js
npm list

```

**ls**, **la** and **ll** are aliases of **list** command.
la and ll commands shows extended information like description and repository.

**Options**

The response format can be changed by passing options.

```js
npm list --json

```


- **json** - Shows information in json format
- **long** - Shows extended information
- **parseable** - Shows parseable list instead of tree
- **global** - Shows globally installed packages
- **depth** - Maximum display depth of dependency tree
- **dev**/**development** - Shows devDependencies
- **prod**/**production** - Shows dependencies

If you want, you can also go to the package's home page.

```js
npm home <package name>

```



## Updating npm and packages


Since npm itself is a Node.js module, it can be updated using itself.

**If OS is Windows must be running command prompt as Admin**

```js
npm install -g npm@latest

```

If you want to check for updated versions you can do:

```js
npm outdated

```

In order to update a specific package:

```js
npm update <package name>

```

> 
This will update the package to the latest version according to the restrictions in package.json


In case you also want to lock the updated version in package.json:

```js
npm update <package name> --save

```



## Locking modules to specific versions


By default, npm installs the latest available version of modules according to each dependencies' [semantic version](http://stackoverflow.com/documentation/node.js/482/npm/2178/basic-semantic-versioning). This can be problematic if a module author doesn't adhere to semver and introduces breaking changes in a module update, for example.

To lock down each dependencies' version (and the versions of their dependencies, etc) to the specific version installed locally in the `node_modules` folder, use

```js
npm shrinkwrap

```

This will then create a `npm-shrinkwrap.json` alongside your `package.json` which lists the specific versions of dependancies.



## Setting up for globally installed packages


You can use `npm install -g` to install a package "globally." This is typically done to install an executable that you can add to your path to run. For example:

```js
npm install -g gulp-cli

```

If you update your path, you can call `gulp` directly.

On many OSes, `npm install -g` will attempt to write to a directory that your user may not be able to write to such as `/usr/bin`. You should **not** use `sudo npm install` in this case since there is a possible security risk of running arbitrary scripts with `sudo` and the root user may create directories in your home that you cannot write to which makes future installations more difficult.

You can tell `npm` where to install global modules to via your configuration file, `~/.npmrc`. This is called the `prefix` which you can view with `npm prefix`.

```js
prefix=~/.npm-global-modules

```

This will use the prefix whenever you run `npm install -g`. You can also use `npm install --prefix ~/.npm-global-modules` to set the prefix when you install. If the prefix is the same as your configuration, you don't need to use `-g`.

In order to use the globally installed module, it needs to be on your path:

```js
export PATH=$PATH:~/.npm-global-modules/bin

```

Now when you run `npm install -g gulp-cli` you will be able to use `gulp`.

**Note:** When you `npm install` (without `-g`) the prefix will be the directory with `package.json` or the current directory if none is found in the hierarchy. This also creates a directory `node_modules/.bin` that has the executables. If you want to use an executable that is specific to a project, it's not necessary to use `npm install -g`. You can use the one in `node_modules/.bin`.



## Linking projects for faster debugging and development


Building project dependencies can sometimes be a tedious task. Instead of publishing a package version to NPM and installing the dependency to test the changes, use `npm link`. `npm link` creates a symlink so the latest code can be tested in a local environment. This makes testing global tools and project dependencies easier by allowing the latest code run before making a published version.

### Help text

```js
NAME
       npm-link - Symlink a package folder

SYNOPSIS
         npm link (in package dir)
         npm link [<@scope>/]<pkg>[@<version>]

         alias: npm ln

```

### Steps for linking project dependencies

When creating the dependency link, note that the package name is what is going to be referenced in the parent project.

1. CD into a dependency directory (ex: `cd ../my-dep`)
1. `npm link`
1. CD into the project that is going to use the dependency
1. `npm link my-dep` or if namespaced `npm link @namespace/my-dep`

### Steps for linking a global tool

1. CD into the project directory (ex: `cd eslint-watch`)
1. `npm link`
1. Use the tool
1. `esw --quiet`

### Problems that may arise

Linking projects can sometimes cause issues if the dependency or global tool is already installed. `npm uninstall (-g) <pkg>` and then running `npm link` normally resolves any issues that may arise.



#### Syntax


<li>npm <command> where <command> is one of:
<ul>
- [add-user](https://docs.npmjs.com/cli/adduser)
- [adduser](https://docs.npmjs.com/cli/adduser)
- apihelp
- author
- bin
- bugs
- c
- [cache](https://docs.npmjs.com/cli/cache)
- completion
- [config](https://docs.npmjs.com/cli/config)
- [ddp](https://docs.npmjs.com/cli/dedupe)
- [dedupe](https://docs.npmjs.com/cli/dedupe)
- deprecate
- docs
- edit
- explore
- faq
- find
- find-dupes
- [get](https://docs.npmjs.com/cli/config)
- [help](https://docs.npmjs.com/cli/help)
- [help-search](https://docs.npmjs.com/cli/help-search)
- home
- [i](http://stackoverflow.com/documentation/node.js/482/npm/1588/installing-packages#t=201608130932049548019)
- [install](http://stackoverflow.com/documentation/node.js/482/npm/1588/installing-packages#t=201608130932049548019)
- info
- [init](http://stackoverflow.com/documentation/node.js/482/npm/2257/setting-up-a-package-configuration#t=201608130932049548019)
- isntall
- issues
- la
- [link](https://docs.npmjs.com/cli/link)
- [list](http://stackoverflow.com/documentation/node.js/482/npm/8732/listing-currently-installed-packages#t=201608130932049548019)
- ll
- ln
- login
- ls
- outdated
- [owner](https://docs.npmjs.com/cli/owner)
- pack
- prefix
- [prune](http://stackoverflow.com/documentation/node.js/482/npm/8731/removing-extraneous-packages#t=201608130932049548019)
- [publish](http://stackoverflow.com/documentation/node.js/482/npm/1588/installing-packages#t=201608130932049548019)
- r
- [rb](https://docs.npmjs.com/cli/rebuild)
- [rebuild](https://docs.npmjs.com/cli/rebuild)
- remove
- [repo](https://docs.npmjs.com/cli/repo)
- [restart](https://docs.npmjs.com/cli/restart)
- [rm](https://docs.npmjs.com/cli/rm)
- root
- [run-script](http://stackoverflow.com/documentation/node.js/482/npm/4592/running-scripts#t=201608130932049548019)
- [s](https://docs.npmjs.com/cli/search)
- [se](https://docs.npmjs.com/cli/search)
- [search](https://docs.npmjs.com/cli/search)
- [set](https://docs.npmjs.com/cli/config)
- show
- shrinkwrap
- [star](https://docs.npmjs.com/cli/star)
- [stars](https://docs.npmjs.com/cli/stars)
- [start](https://docs.npmjs.com/cli/start)
- [stop](https://docs.npmjs.com/cli/stop)
- [submodule](https://docs.npmjs.com/cli/submodule)
- [tag](https://docs.npmjs.com/cli/tag)
- [test](https://docs.npmjs.com/cli/test)
- [tst](https://docs.npmjs.com/cli/test)
- [un](http://stackoverflow.com/documentation/node.js/482/npm/2153/uninstalling-packages#t=201608130932049548019)
- [uninstall](http://stackoverflow.com/documentation/node.js/482/npm/2153/uninstalling-packages#t=201608130932049548019)
- [unlink](https://docs.npmjs.com/cli/link)
- [unpublish](https://docs.npmjs.com/cli/unpublish)
- [unstar](https://docs.npmjs.com/cli/star)
- [up](https://docs.npmjs.com/cli/update)
- [update](https://docs.npmjs.com/cli/update)
- [v](https://docs.npmjs.com/cli/version)
- [version](https://docs.npmjs.com/cli/version)
- [view](https://docs.npmjs.com/cli/view)
- [whoami](https://docs.npmjs.com/cli/whoami)



#### Parameters


|Parameter|Example
|---|---
|[access](https://docs.npmjs.com/cli/access)|`npm publish --access=public`
|[bin](https://docs.npmjs.com/cli/bin)|`npm bin -g`
|[edit](https://docs.npmjs.com/cli/edit)|`npm edit connect`
|[help](https://docs.npmjs.com/cli/help)|`npm help init`
|[init](https://docs.npmjs.com/cli/init)|`npm init`
|[install](https://docs.npmjs.com/cli/install)|`npm install`
|[link](https://docs.npmjs.com/cli/link)|`npm link`
|[prune](https://docs.npmjs.com/cli/prune)|`npm prune`
|[publish](https://docs.npmjs.com/cli/publish)|`npm publish ./`
|[restart](https://docs.npmjs.com/cli/restart)|`npm restart`
|[start](https://docs.npmjs.com/cli/start)|`npm start`
|[stop](https://docs.npmjs.com/cli/stop)|`npm start`
|[update](https://docs.npmjs.com/cli/update)|`npm update`
|[version](https://docs.npmjs.com/cli/version)|`npm version`

