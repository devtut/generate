---
metaTitle: "Node.js - grunt"
description: "Introduction To GruntJs, Installing gruntplugins"
---

# grunt




## Introduction To GruntJs


Grunt is a JavaScript Task Runner, used for automation of repetitive tasks like minification, compilation, unit testing, linting, etc.

In order to get started, you'll want to install Grunt's command line interface (CLI) globally.

```js
npm install -g grunt-cli

```

**Preparing a new Grunt project:**
A typical setup will involve adding two files to your project: package.json and the Gruntfile.

package.json: This file is used by npm to store metadata for projects published as npm modules. You will list grunt and the Grunt plugins your project needs as devDependencies in this file.

Gruntfile: This file is named Gruntfile.js and is used to configure or define tasks and load Grunt plugins.

```js
Example package.json:

{
  "name": "my-project-name",
  "version": "0.1.0",
  "devDependencies": {
    "grunt": "~0.4.5",
    "grunt-contrib-jshint": "~0.10.0",
    "grunt-contrib-nodeunit": "~0.4.1",
    "grunt-contrib-uglify": "~0.5.0"
  }
}

```

**Example gruntfile:**

```js
module.exports = function(grunt) {

  // Project configuration.
  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),
    uglify: {
      options: {
        banner: '/*! <%= pkg.name %> <%= grunt.template.today("yyyy-mm-dd") %> */\n'
      },
      build: {
        src: 'src/<%= pkg.name %>.js',
        dest: 'build/<%= pkg.name %>.min.js'
      }
    }
  });

  // Load the plugin that provides the "uglify" task.
  grunt.loadNpmTasks('grunt-contrib-uglify');

  // Default task(s).
  grunt.registerTask('default', ['uglify']);

};

```



## Installing gruntplugins


**Adding dependcy**

To use a gruntplugin, you first need to add it as a dependency to your project. Let's use the jshint plugin as an example.

```js
npm install grunt-contrib-jshint --save-dev

```

The `--save-dev` option is used to add the plugin in the `package.json`, this way the plugin is always installed after a `npm install`.

**Loading the plugin**

You can load your plugin in the gruntfile file using `loadNpmTasks`.

```js
grunt.loadNpmTasks('grunt-contrib-jshint');

```

**Configuring the task**

You configure the task in the gruntfile adding a property called `jshint` to the object passed to `grunt.initConfig`.

```js
grunt.initConfig({
  jshint: {
    all: ['Gruntfile.js', 'lib/**/*.js', 'test/**/*.js']
  }
});

```

Don't forget you can have other properties for other plugins you are using.

**Running the task**

To just run the task with the plugin you can use the command line.

```js
grunt jshint

```

Or you can add `jshint` to another task.

```js
grunt.registerTask('default', ['jshint']);

```

The default task runs with the grunt command in the terminal without any options.



#### Remarks


**Further reading:**

The [Installing grunt guide](http://gruntjs.com/installing-grunt) has detailed information about installing specific, production or in-development, versions of Grunt and grunt-cli.

The [Configuring Tasks guide](http://gruntjs.com/configuring-tasks) has an in-depth explanation on how to configure tasks, targets, options and files inside the Gruntfile, along with an explanation of templates, globbing patterns and importing external data.

The [Creating Tasks guide](http://gruntjs.com/creating-tasks) lists the differences between the types of Grunt tasks and shows a number of sample tasks and configurations.

