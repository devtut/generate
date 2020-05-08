---
metaTitle: "Angular 2 - Create an Angular 2+ NPM package"
description: "Simplest package"
---

# Create an Angular 2+ NPM package


Sometimes we need to share some component between some apps and publishing it in npm is one of the best ways of doing this.

There are some tricks that we need to know to be able to use a normal component as npm package without changing the structure as inlining external styles.

You can see a minimal example [here](https://github.com/vinagreti/angular-x-minimal-npm-package)



## Simplest package


Here we are sharing some minimal workflow to build and publish an Angular 2+ npm package.

### Configuration files

We need some config files to tell `git`, `npm`, `gulp` and `typescript` how to act.

### .gitignore

First we create a `.gitignore` file to avoid versioning unwanted files and folders. The content is:

```js
npm-debug.log
node_modules
jspm_packages
.idea
build

```

### .npmignore

Second we create a `.npmignore` file to avoid publishing unwanted files and folders. The content is:

```js
examples
node_modules
src

```

### gulpfile.js

We need to create a `gulpfile.js` to tell Gulp how to compile our application. This part is necessary because we need to minimize and inline all the external templates and styles before publishing our package. The content is:

```js
var gulp = require('gulp');
var embedTemplates = require('gulp-angular-embed-templates');
var inlineNg2Styles = require('gulp-inline-ng2-styles');

gulp.task('js:build', function () {
    gulp.src('src/*.ts') // also can use *.js files
        .pipe(embedTemplates({sourceType:'ts'}))
        .pipe(inlineNg2Styles({ base: '/src' }))
        .pipe(gulp.dest('./dist'));
});

```

### index.d.ts

The `index.d.ts` file is used by typescript when importing an external module. It helps editor with auto-completion and function tips.

```js
export * from './lib';

```

### index.js

This is the package entry point. When you install this package using NPM and import in your application, you just need to pass the package name and your application will learn where to find any EXPORTED component of your package.

```js
exports.AngularXMinimalNpmPackageModule = require('./lib').AngularXMinimalNpmPackageModule;

```

We used `lib` folder because when we compile our code, the output is placed inside `/lib` folder.

### package.json

This file is used to configure your npm publication and defines the necessary packages to it to work.

```js
{
  "name": "angular-x-minimal-npm-package",
  "version": "0.0.18",
  "description": "An Angular 2+ Data Table that uses HTTP to create, read, update and delete data from an external API such REST.",
  "main": "index.js",
  "scripts": {
    "watch": "tsc -p src -w",
    "build": "gulp js:build && rm -rf lib && tsc -p dist"
  },
  "repository": {
    "type": "git",
    "url": "git+https://github.com/vinagreti/angular-x-minimal-npm-package.git"
  },
  "keywords": [
    "Angular",
    "Angular2",
    "Datatable",
    "Rest"
  ],
  "author": "bruno@tzadi.com",
  "license": "MIT",
  "bugs": {
    "url": "https://github.com/vinagreti/angular-x-minimal-npm-package/issues"
  },
  "homepage": "https://github.com/vinagreti/angular-x-minimal-npm-package#readme",
  "devDependencies": {
    "gulp": "3.9.1",
    "gulp-angular-embed-templates": "2.3.0",
    "gulp-inline-ng2-styles": "0.0.1",
    "typescript": "2.0.0"
  },
  "dependencies": {
    "@angular/common": "2.4.1",
    "@angular/compiler": "2.4.1",
    "@angular/core": "2.4.1",
    "@angular/http": "2.4.1",
    "@angular/platform-browser": "2.4.1",
    "@angular/platform-browser-dynamic": "2.4.1",
    "rxjs": "5.0.2",
    "zone.js": "0.7.4"
  }
}

```

### dist/tsconfig.json

Create a dist folder and place this file inside. This file is used to tell Typescript how to compile your application. Where to to get the typescript folder and where to put the compiled files.

```js
{
  "compilerOptions": {
    "emitDecoratorMetadata": true,
    "experimentalDecorators": true,
    "mapRoot": "",
    "rootDir": ".",
    "target": "es5",
    "lib": ["es6", "es2015", "dom"],
    "inlineSources": true,
    "stripInternal": true,
    "module": "commonjs",
    "moduleResolution": "node",
    "removeComments": true,
    "sourceMap": true,
    "outDir": "../lib",
    "declaration": true
  }
}

```

After create the configuration files, we must create our component and module.
This component receives a click and displays a message. It is used like a html tag `<angular-x-minimal-npm-package></angular-x-minimal-npm-package>`. Just instal this npm package and load its module in the model you want to use it.

### src/angular-x-minimal-npm-package.component.ts

```js
import {Component} from '@angular/core';
@Component({
    selector: 'angular-x-minimal-npm-package',
    styleUrls: ['./angular-x-minimal-npm-package.component.scss'],
    templateUrl: './angular-x-minimal-npm-package.component.html'
})
export class AngularXMinimalNpmPackageComponent {
    message = "Click Me ...";
    onClick() {
        this.message = "Angular 2+ Minimal NPM Package. With external scss and html!";
    }
}

```

### src/angular-x-minimal-npm-package.component.html

```js
<div>
  <h1 (click)="onClick()">{{message}}</h1>
</div>

```

### src/angular-x-data-table.component.css

```js
h1{
    color: red;
}

```

### src/angular-x-minimal-npm-package.module.ts

```js
import { NgModule } from '@angular/core';
import { CommonModule  } from '@angular/common';

import { AngularXMinimalNpmPackageComponent } from './angular-x-minimal-npm-package.component';

@NgModule({
  imports: [ CommonModule ],
  declarations: [ AngularXMinimalNpmPackageComponent ],
  exports:  [ AngularXMinimalNpmPackageComponent ],
  entryComponents: [ AngularXMinimalNpmPackageComponent ],
})
export class AngularXMinimalNpmPackageModule {}

```

After that, you must compile, build and publish your package.

### Build and compile

For build we use `gulp` and for compiling we use `tsc`. The command are set in package.json file, at `scripts.build` option. We have this set `gulp js:build && rm -rf lib && tsc -p dist`. This is our chain tasks that will do the job for us.

To build and compile, run the following command at the root of your package:

`npm run build`

This will trigger the chain and you will end up with your build in `/dist` folder and the compiled package in your `/lib` folder. This is why in `index.js` we exported the code from `/lib` folder and not from `/src`.

### Publish

Now we just need to publish our package so we can install it through npm. For that, just run the command:

`npm publish`

That is all!!!

