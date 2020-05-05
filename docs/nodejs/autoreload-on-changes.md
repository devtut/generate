---
metaTitle: "Node.js - Autoreload on changes"
description: "Autoreload on source code changes using nodemon, Browsersync"
---

# Autoreload on changes




## Autoreload on source code changes using nodemon


The nodemon package makes it possible to automatically reload your program when you modify any file in the source code.

### Installing nodemon globally

`npm install -g nodemon` (or `npm i -g nodemon`)

### Installing nodemon locally

In case you don't want to install it globally

`npm install --save-dev nodemon` (or `npm i -D nodemon`)

### Using nodemon

Run your program with `nodemon entry.js` (or `nodemon entry`)

This replaces the usual use of `node entry.js` (or `node entry`).

You can also add your nodemon startup as an npm script, which might be useful if you want to supply parameters and not type them out every time.

Add **package.json:**

```

 "scripts": {
    "start": "nodemon entry.js -devmode -something 1"
  }

```

This way you can just use `npm start` from your console.



## Browsersync


### Overview

[Browsersync](https://browsersync.io) is a tool that allows for live file watching and browser reloading. It's available as a [NPM package](https://www.npmjs.com/package/browser-sync).

### Installation

To install Browsersync you'll first need to have [Node.js](https://nodejs.org/en/) and NPM installed. For more information see the SO documentation on [Installing and Running Node.js](http://stackoverflow.com/documentation/node.js/340/getting-started-with-node-js#t=201703151714075932365).

Once your project is set up you can install Browsersync with the following command:

```js
$ npm install browser-sync -D

```

This will install Browsersync in the local `node_modules` directory and save it to your developer dependencies.

If you'd rather install it globally use the `-g` flag in place of the `-D` flag.

### Windows Users

If you're having trouble installing Browsersync on Windows you may need to install Visual Studio so you can access the build tools to install Browsersync. You'll then need to specify the version of Visual Studio you're using like so:

```js
$ npm install browser-sync --msvs_version=2013 -D

```

This command specifies the 2013 version of Visual Studio.

### Basic Usage

To automatically reload your site whenever you change a JavaScript file in your project use the following command:

```js
$ browser-sync start --proxy "myproject.dev" --files "**/*.js"

```

Replace `myproject.dev` with the web address that you are using to access your project. Browsersync will output an alternate address that can be used to access your site through the proxy.

### Advanced Usage

Besides the command line interface that was described above Browsersync can also be used with [Grunt.js](http://stackoverflow.com/tags/gruntjs/info) and [Gulp.js](http://stackoverflow.com/tags/gulp/info).

### Grunt.js

Usage with Grunt.js requires a plugin that can be installed like so:

```js
$ npm install grunt-browser-sync -D

```

Then you'll add this line to your `gruntfile.js`:

```js
grunt.loadNpmTasks('grunt-browser-sync');

```

### Gulp.js

Browsersync works as a [CommonJS](http://stackoverflow.com/tags/commonjs/info) module, so there's no need for a Gulp.js plugin. Simply require the module like so:

```js
var browserSync = require('browser-sync').create();

```

You can now use the [Browsersync API](https://browsersync.io/docs/api) to configure it to your needs.

### API

The Browsersync API can be found here: [https://browsersync.io/docs/api](https://browsersync.io/docs/api)

