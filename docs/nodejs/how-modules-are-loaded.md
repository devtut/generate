---
metaTitle: "How modules are loaded"
description: "Global Mode, Loading modules"
---

# How modules are loaded



## Global Mode


If you installed Node using the default directory, while in the global mode, NPM installs packages into `/usr/local/lib/node_modules`. If you type the following in the shell, NPM will search for, download, and install the latest version of the package named sax inside the directory `/usr/local/lib/node_modules/express`:

```js
$ npm install -g express

```

Make sure that you have sufficient access rights to the folder. These modules will be available for all node process which will be running in that machine

In local mode installation. Npm will down load and install modules in the current working folders by creating a new folder called `node_modules` for example if you are in `/home/user/apps/my_app` a new folder will be created called `node_modules` `/home/user/apps/my_app/node_modules` if its not already exist



## Loading modules


When we refer the module in the code, node first looks up the `node_module` folder inside the referenced folder in required statement
If the module name is not relative and is not a core module, Node will try to find it inside the `node_modules` folder in the current directory.
For instance, if you do the following, Node will try to look for the file `./node_modules/myModule.js`:

```js
var myModule = require('myModule.js');

```

If Node fails to find the file, it will look inside the parent folder called `../node_modules/myModule.js`. If it fails again, it will try the parent folder and keep descending until it reaches the root or finds the required module.

You can also omit the `.js` extension if you like to, in which case node will append the `.js` extension and will search for the file.

### Loading a Folder Module

You can use the path for a folder to load a module like this:

```js
var myModule = require('./myModuleDir');

```

If you do so, Node will search inside that folder. Node will presume this folder is a package and will try to look for a package definition. That package definition should be a file named `package.json`.
If that folder does not contain a package definition file named `package.json`, the package entry point will assume the default value of `index.js`, and Node will look, in this case, for a file under the path `./myModuleDir/index.js`.

The last resort if module is not found in any of the folders is the global module  installation folder.

