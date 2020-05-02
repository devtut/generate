---
metaTitle: "package.json"
description: "Scripts, Exploring package.json, Basic project definition, Dependencies, Extended project definition"
---

# package.json



## Scripts


You can define scripts that can be executed or are triggered before or after another script.

```js
{
  "scripts": {
    "pretest": "scripts/pretest.js",
    "test": "scripts/test.js",
    "posttest": "scripts/posttest.js"
  }
}

```

In this case, you can execute the script by running either of these commands:

```js
$ npm run-script test
$ npm run test
$ npm test
$ npm t

```

### Pre-defined scripts

|Script Name|Description
|------
|prepublish|Run before the package is published.
|publish, postpublish|Run after the package is published.
|preinstall|Run before the package is installed.
|install, postinstall|Run after the package is installed.
|preuninstall, uninstall|Run before the package is uninstalled.
|postuninstall|Run after the package is uninstalled.
|preversion, version|Run before bump the package version.
|postversion|Run after bump the package version.
|pretest, test, posttest|Run by the `npm test` command
|prestop, stop, poststop|Run by the `npm stop` command
|prestart, start, poststart|Run by the `npm start` command
|prerestart, restart, postrestart|Run by the `npm restart` command

### User-defined scripts

You can also define your own scripts the same way you do with the pre-defined scripts:

```js
{
  "scripts": {
    "preci": "scripts/preci.js",
    "ci": "scripts/ci.js",
    "postci": "scripts/postci.js"
  }
}

```

In this case, you can execute the script by running either of these commands:

```js
$ npm run-script ci
$ npm run ci

```

User-defined scripts also supports **pre** and **post** scripts, as shown in the example above.



## Exploring package.json


A `package.json` file, usually present in the project root, contains metadata about your app or module as well as the list of dependencies to install from npm when running `npm install`.

To initialize a `package.json` type `npm init` in your command prompt.

To create a `package.json` with default values use:

```js
npm init --yes
# or
npm init -y

```

To install a package and save it to `package.json` use:

```js
npm install {package name} --save

```

You can also use the shorthand notation:

```

npm i -S {package name}

```

NPM aliases `-S` to `--save` and `-D` to `--save-dev` to save in your production or development dependencies respectively.

The package will appear in your dependencies; if you use
`--save-dev` instead of `--save`, the package will appear in your devDependencies.

Important properties of `package.json`:

```js
{
  "name": "module-name",
  "version": "10.3.1",
  "description": "An example module to illustrate the usage of a package.json",
  "author": "Your Name <your.name@example.org>",
  "contributors": [{
    "name": "Foo Bar",
    "email": "foo.bar@example.com"
  }],
  "bin": {
    "module-name": "./bin/module-name"
  },
  "scripts": {
    "test": "vows --spec --isolate",
    "start": "node index.js",
    "predeploy": "echo About to deploy",
    "postdeploy": "echo Deployed",
    "prepublish": "coffee --bare --compile --output lib/foo src/foo/*.coffee"
  },
  "main": "lib/foo.js",
  "repository": {
    "type": "git",
    "url": "https://github.com/username/repo"
  },
  "bugs": {
    "url": "https://github.com/username/issues"
  },
  "keywords": [
    "example"
  ],
  "dependencies": {
    "express": "4.2.x"
  },
  "devDependencies": {
    "assume": "<1.0.0 || >=2.3.1 <2.4.5 || >=2.5.2 <3.0.0"
  },
  "peerDependencies": {
    "moment": ">2.0.0"
  },
  "preferGlobal": true,
  "private": true,
  "publishConfig": {
    "registry": "https://your-private-hosted-npm.registry.domain.com"
  },
  "subdomain": "foobar",
  "analyze": true,
  "license": "MIT",
  "files": [
    "lib/foo.js"
  ]
}

```

Information about some important properties:

```js
name

```

The unique name of your package and should be down in lowercase. This property is required and your package will not install without it.

1. The name must be less than or equal to 214 characters.
1. The name can't start with a dot or an underscore.
1. New packages must not have uppercase letters in the name.

```js
version

```

The version of the package is specified by [Semantic Versioning](http://semver.org/) (semver). Which assumes that a version number is written as MAJOR.MINOR.PATCH and you increment the:

1. MAJOR version when you make incompatible API changes
<li>MINOR version
when you add functionality in a backwards-compatible manner</li>
<li>PATCH
version when you make backwards-compatible bug fixes</li>

```js
description

```

The description of the project. Try to keep it short and concise.

```js
author

```

The author of this package.

```js
bin

```

An object which is used to expose binary scripts from your package. The object assumes that the key is the name of the binary script and the value a relative path to the script.

This property is used by packages that contain a CLI (command line interface).

```js
script

```

A object which exposes additional npm commands. The object assumes that the key is the npm command and the value is the script path. These scripts can get executed when you run `npm run {command name}` or `npm run-script {command name}`.

Packages that contain a command line interface and are installed locally can be called without a relative path. So instead of calling `./node-modules/.bin/mocha` you can directly call `mocha`.

```js
main

```

The main entry point to your package. When calling `require('{module name}')` in node, this will be actual file that is required.

It's highly advised that requiring the main file does not generate any side affects. For instance, requiring the main file should not start up a HTTP server or connect to a database. Instead, you should create something like `exports.init = function () {...}` in your main script.

```js
keywords

```

An array of keywords which describe your package. These will help people find your package.

```js
devDependencies

```

These are the dependencies that are only intended for development and testing of your module. The dependencies will be installed automatically unless the `NODE_ENV=production` environment variable has been set. If this is the case you can still these packages using `npm install --dev`

```js
peerDependencies

```

If you are using this module, then peerDependencies lists the modules you must install alongside this one. For example, `moment-timezone` must be installed alongside `moment` because it is a plugin for moment, even if it doesn't directly `require("moment")`.

```js
preferGlobal

```

A property that indicates that this page prefers to be installed globally using `npm install -g {module-name}`. This property is used by packages that contain a CLI (command line interface).

In all other situations you should NOT use this property.

```js
publishConfig

```

The publishConfig is an object with configuration values that will be used for publishing modules. The configuration values that are set override your default npm configuration.

The most common use of the `publishConfig` is to publish your package to a private npm registry so you still have the benefits of npm but for private packages. This is done by simply setting URL of your private npm as value for the registry key.

```js
files

```

This is an array of all the files to include in the published package. Either a file path or folder path can be used. All the contents of a folder path will be included. This reduces the total size of your package by only including the correct files to be distributed. This field works in conjunction with a `.npmignore` rules file.

[Source](http://browsenpm.org/package.json)



## Basic project definition


```js
{
    "name": "my-project",
    "version": "0.0.1",
    "description": "This is a project.",
    "author": "Someone <someone@example.com>",
    "contributors": [{
        "name": "Someone Else",
        "email": "else@example.com"
    }],
    "keywords": ["improves", "searching"]
}

```

|Field|Description
|------
|name|a **required** field for a package to install. Needs to be lowercase, single word without spaces. (Dashes and underscores allowed)
|version|a **required** field for the package version using [semantic versioning](https://docs.npmjs.com/getting-started/semantic-versioning).
|description|a short description of the project
|author|specifies the author of the package
|contributors|an array of objects, one for each contributor
|keywords|an array of strings, this will help people finding your package



## Dependencies


"dependencies": {
"module-name": "0.1.0"
}

- **exact**: `0.1.0` will install that specific version of the module.
- **newest minor version**: `^0.1.0` will install the newest minor version, for example `0.2.0`, but won't install a module with a higher major version e.g. `1.0.0`
- **newest patch**: `0.1.x` or `~0.1.0` will install the newest patch version available, for example `0.1.4`, but won't install a module with higher major or minor version, e.g. `0.2.0` or `1.0.0`.
- **wildcard**: `*` will install the latest version of the module.
<li>**git repository**: the following will install a tarball from the master branch of a git repo. A `#sha`, `#tag` or `#branch` can also be provided:
<ul>
- **GitHub**: `user/project` or `user/project#v1.0.0`
- **url**: `git://gitlab.com/user/project.git` or `git://gitlab.com/user/project.git#develop`

After adding them to your package.json, use the command `npm install`  in your project directory in terminal.

### devDependencies

```js
"devDependencies": {
    "module-name": "0.1.0"
}

```

For dependencies required only for development, like testing styling proxies ext.
Those dev-dependencies won't be installed when running "npm install" in production mode.



## Extended project definition


Some of the additional attributes are parsed by the npm website like `repository`, `bugs` or `homepage` and shown in the infobox for this packages

```js
{
  "main": "server.js",  
  "repository" :  {
    "type": "git",
    "url": "git+https://github.com/<accountname>/<repositoryname>.git"
  },
  "bugs": {
    "url": "https://github.com/<accountname>/<repositoryname>/issues"
  },
  "homepage": "https://github.com/<accountname>/<repositoryname>#readme",
  "files": [
    "server.js", // source files
    "README.md", // additional files
    "lib" // folder with all included files
  ]
}

```

|Field|Description
|------
|main|Entry script for this package. This script is returned when a user requires the package.
|repository|Location and type of the public repository
|bugs|Bugtracker for this package (e.g. github)
|homepage|Homepage for this package or the general project
|files|List of files and folders which should be downloaded when a user does a `npm install <packagename>`



#### Remarks


You can create `package.json` with

```js
npm init

```

which will ask you about basic facts about your projects, including [license identifier](https://spdx.org/licenses/).

