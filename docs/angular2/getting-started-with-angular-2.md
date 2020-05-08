---
metaTitle: "Angular 2 - Getting started with Angular 2"
description: "Install angular2 with angular-cli, Getting started with Angular 2 without angular-cli., Getting started with Angular 2 with node.js/expressjs backend (http example included), Getting through that pesky company proxy, Keeping Visual Studios in sync with NPM and NODE Updates, Let's dive into Angular 4!"
---

# Getting started with Angular 2



## Install angular2 with angular-cli


This example is a quick setup of Angular 2 and how to generate a quick example project.

### Prerequisites:

- [Node.js v4](https://nodejs.org) or greater.
- [npm](https://www.npmjs.com/) v3 or greater or [yarn](https://yarnpkg.com).

Open a terminal and run the commands one by one:

```js
npm install -g @angular/cli

```

or

```js
yarn global add @angular/cli

```

depending on your choice of package manager.

The previous command installs **@angular/cli** globally, adding the executable `ng` to PATH.

### To setup a new project

Navigate with the terminal to a folder where you want to set up the new project.

Run the commands:

```js
ng new PROJECT_NAME
cd PROJECT_NAME
ng serve

```

That is it, you now have a simple example project made with Angular 2. You can now navigate to the link displayed in terminal and see what it is running.

### To add to an existing project

Navigate to the root of your current project.

Run the command:

```js
ng init

```

This will add the necessary scaffolding to your project. The files will be created in the current directory so be sure to run this in an empty directory.

### Running The Project Locally

In order to see and interact with your application while it's running in the browser you must start a local development server hosting the files for your project.

```js
ng serve

```

If the server started successfully it should display an address at which the server is running. Usually is this:

```js
http://localhost:4200

```

Out of the box this local development server is hooked up with Hot Module Reloading, so any changes to the html, typescript, or css, will trigger the browser to be automatically reloaded (but can be disabled if desired).

### Generating Components, Directives, Pipes and Services

The `ng generate <scaffold-type> <name>` (or simply `ng g <scaffold-type> <name>`) command allows you to automatically generate Angular components:

```js
# The command below will generate a component in the folder you are currently at
ng generate component my-generated-component
# Using the alias (same outcome as above)
ng g component my-generated-component

```

There are several possible types of scaffolds angular-cli can generate:

|Scaffold Type|Usage
|---|---|---|---|---|---|---|---|---|---
|Module|`ng g module my-new-module`
|Component|`ng g component my-new-component`
|Directive|`ng g directive my-new-directive`
|Pipe|`ng g pipe my-new-pipe`
|Service|`ng g service my-new-service`
|Class|`ng g class my-new-class`
|Interface|`ng g interface my-new-interface`
|Enum|`ng g enum my-new-enum`

You can also replace the type name by its first letter. For example:

`ng g m my-new-module` to generate a new module or `ng g c my-new-component` to create a component.

**Building/Bundling**

When you are all finished building your Angular 2 web app and you would like to install it on a web server like Apache Tomcat, all you need to do is run the build command either with or without the production flag set. Production will minifiy the code and optimize for a production setting.

```js
ng build

```

or

```js
ng build --prod

```

Then look in the projects root directory for a `/dist` folder, which contains the build.

If you'd like the benefits of a smaller production bundle, you can also use Ahead-of-Time template compilation, which removes the template compiler from the final build:

```js
ng build --prod --aot

```

**Unit Testing**

Angular 2 provides built-in unit testing, and every item created by angular-cli generates a basic unit test, that can be expanded.
The unit tests are written using jasmine, and executed through Karma.
In order to start testing execute the following command:

```js
ng test

```

This command will execute all the tests in the project, and will re-execute them every time a source file changes, whether it is a test or code from the application.

For more info also visit: [angular-cli github page](https://github.com/angular/angular-cli)



## Getting started with Angular 2 without angular-cli.


Angular 2.0.0-rc.4

In this example we'll create a "Hello World!" app with only one root component (`AppComponent`) for the sake of simplicity.

**Prerequisites:**

- [Node.js](https://nodejs.org) v5 or later
- npm v3 or later

> 
**Note:** You can check versions by running `node -v` and `npm -v` in the console/terminal.


### Step 1

Create and enter a new folder for your project. Let's call it `angular2-example`.

```js
mkdir angular2-example
cd angular2-example

```

### Step 2

Before we start writing our app code, we'll add the 4 files provided below: `package.json`, `tsconfig.json`, `typings.json`, and `systemjs.config.js`.

> 
**Disclaimer:** The same files can be found in the [Official 5 Minute Quickstart](https://angular.io/docs/ts/latest/quickstart.html).


`package.json` - Allows us to download all dependencies with npm and provides simple script execution to make life easier for simple projects. (You should consider using something like [Gulp](http://gulpjs.com) in the future to automate tasks).

```js
{
  "name": "angular2-example",
  "version": "1.0.0",
  "scripts": {
    "start": "tsc && concurrently \"npm run tsc:w\" \"npm run lite\" ",
    "lite": "lite-server",
    "postinstall": "typings install",
    "tsc": "tsc",
    "tsc:w": "tsc -w",
    "typings": "typings"
  },
  "license": "ISC",
  "dependencies": {
    "@angular/common": "2.0.0-rc.4",
    "@angular/compiler": "2.0.0-rc.4",
    "@angular/core": "2.0.0-rc.4",
    "@angular/forms": "0.2.0",
    "@angular/http": "2.0.0-rc.4",
    "@angular/platform-browser": "2.0.0-rc.4",
    "@angular/platform-browser-dynamic": "2.0.0-rc.4",
    "@angular/router": "3.0.0-beta.1",
    "@angular/router-deprecated": "2.0.0-rc.2",
    "@angular/upgrade": "2.0.0-rc.4",
    "systemjs": "0.19.27",
    "core-js": "^2.4.0",
    "reflect-metadata": "^0.1.3",
    "rxjs": "5.0.0-beta.6",
    "zone.js": "^0.6.12",
    "angular2-in-memory-web-api": "0.0.14",
    "bootstrap": "^3.3.6"
  },
  "devDependencies": {
    "concurrently": "^2.0.0",
    "lite-server": "^2.2.0",
    "typescript": "^1.8.10",
    "typings":"^1.0.4"
  }
}

```

`tsconfig.json` - Configures the TypeScript transpiler.

```js
{
  "compilerOptions": {
    "target": "es5",
    "module": "commonjs",
    "moduleResolution": "node",
    "sourceMap": true,
    "emitDecoratorMetadata": true,
    "experimentalDecorators": true,
    "removeComments": false,
    "noImplicitAny": false
  }
}

```

`typings.json` - Makes TypeScript recognize libraries we're using.

```js
{
  "globalDependencies": {
    "core-js": "registry:dt/core-js#0.0.0+20160602141332",
    "jasmine": "registry:dt/jasmine#2.2.0+20160621224255",
    "node": "registry:dt/node#6.0.0+20160621231320"
  }
}

```

`systemjs.config.js` - Configures [SystemJS](https://github.com/systemjs/systemjs) (you can also use [webpack](https://webpack.github.io)).

```js
/**
 * System configuration for Angular 2 samples
 * Adjust as necessary for your application's needs.
 */
(function(global) {
  // map tells the System loader where to look for things
  var map = {
    'app':                        'app', // 'dist',
    '@angular':                   'node_modules/@angular',
    'angular2-in-memory-web-api': 'node_modules/angular2-in-memory-web-api',
    'rxjs':                       'node_modules/rxjs'
  };
  // packages tells the System loader how to load when no filename and/or no extension
  var packages = {
    'app':                        { main: 'main.js',  defaultExtension: 'js' },
    'rxjs':                       { defaultExtension: 'js' },
    'angular2-in-memory-web-api': { main: 'index.js', defaultExtension: 'js' },
  };
  var ngPackageNames = [
    'common',
    'compiler',
    'core',
    'forms',
    'http',
    'platform-browser',
    'platform-browser-dynamic',
    'router',
    'router-deprecated',
    'upgrade',
  ];
  // Individual files (~300 requests):
  function packIndex(pkgName) {
    packages['@angular/'+pkgName] = { main: 'index.js', defaultExtension: 'js' };
  }
  // Bundled (~40 requests):
  function packUmd(pkgName) {
    packages['@angular/'+pkgName] = { main: '/bundles/' + pkgName + '.umd.js', defaultExtension: 'js' };
  }
  // Most environments should use UMD; some (Karma) need the individual index files
  var setPackageConfig = System.packageWithIndex ? packIndex : packUmd;
  // Add package entries for angular packages
  ngPackageNames.forEach(setPackageConfig);
  var config = {
    map: map,
    packages: packages
  };
  System.config(config);
})(this);

```

### Step 3

Let's install the dependencies by typing

```js
npm install

```

in the console/terminal.

**Step 4**

Create `index.html` inside of the `angular2-example` folder.

```js
<html>
  <head>
    <title>Angular2 example</title>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <!-- 1. Load libraries -->
    <!-- Polyfill(s) for older browsers -->
    <script src="node_modules/core-js/client/shim.min.js"></script>
    <script src="node_modules/zone.js/dist/zone.js"></script>
    <script src="node_modules/reflect-metadata/Reflect.js"></script>
    <script src="node_modules/systemjs/dist/system.src.js"></script>
    <!-- 2. Configure SystemJS -->
    <script src="systemjs.config.js"></script>
    <script>
      System.import('app').catch(function(err){ console.error(err); });
    </script>
  </head>
  <!-- 3. Display the application -->
  <body>
    <my-app></my-app>
  </body>
</html>

```

Your application will be rendered between the `my-app` tags.

However, Angular still doesn't know **what** to render. To tell it that, we'll define `AppComponent`.

### Step 5

Create a subfolder called `app` where we can define the components and [services](http://stackoverflow.com/documentation/angular2/4187/services-and-dependency-injection#t=201610132311345717631) that make up our app. (In this case, it'll just contain the `AppComponent` code and `main.ts`.)

```js
mkdir app

```

### Step 6

Create the file `app/app.component.ts`

```js
import { Component } from '@angular/core';

@Component({
  selector: 'my-app',
  template: `
    <h1>{{title}}</h1>
    <ul>
        <li *ngFor="let message of messages">
            {{message}}
        </li>
    </ul>
  `
})
export class AppComponent { 
    title = "Angular2 example";
    messages = [
        "Hello World!",
        "Another string",
        "Another one"
    ];
}

```

What's happening? First, we're importing the `@Component` decorator which we use to give Angular the HTML tag and template for this component. Then, we're creating the  class `AppComponent` with `title` and `messages` variables that we can use in the template.

Now let's look at that template:

```js
<h1>{{title}}</h1>
<ul>
    <li *ngFor="let message of messages">
        {{message}}
    </li>
</ul>

```

We're displaying the `title` variable in an `h1` tag and then making a list showing each element of the `messages` array by using the `*ngFor` directive. For each element in the array, `*ngFor` creates a `message` variable that we use within the `li` element. The result will be:

```js
<h1>Angular 2 example</h1>
<ul>
    <li>Hello World!</li>
    <li>Another string</li>
    <li>Another one</li>
</ul>

```

### Step 7

Now we create a `main.ts` file, which will be the first file that Angular looks at.

Create the file `app/main.ts`.

```js
import { bootstrap }    from '@angular/platform-browser-dynamic';
import { AppComponent } from './app.component';

bootstrap(AppComponent);

```

We're importing the `bootstrap` function and `AppComponent` class, then using `bootstrap`  to tell Angular which component to use as the root.

### Step 8

It's time to fire up your first app. Type

```js
npm start

```

in your console/terminal. This will run a prepared script from `package.json` that starts lite-server, opens your app in a browser window, and runs the TypeScript transpiler in watch mode (so `.ts` files will be transpiled and the browser will refresh when you save changes).

### What now?

Check out [the official Angular 2 guide](https://angular.io/docs/ts/latest/guide) and the other topics on [StackOverflow's documentation](http://stackoverflow.com/documentation/angular2/topics).

You can also edit `AppComponent` to use external templates, styles or add/edit component variables. You should see your changes immediately after saving files.



## Getting started with Angular 2 with node.js/expressjs backend (http example included)


We will create a simple "Hello World!" app with Angular2 2.4.1 (`@NgModule` change) with a node.js (expressjs) backend.

### Prerequisites

- [Node.js](https://nodejs.org/en/) v4.x.x or higher
- [npm](https://www.npmjs.com/) v3.x.x or higher or [yarn](https://yarnpkg.com)

Then run `npm install -g typescript` or `yarn global add typescript`to install typescript globally

### Roadmap

### Step 1

Create a new folder (and the root dir of our back-end) for our app. Let's call it `Angular2-express`.

**command line**:

```js
mkdir Angular2-express
cd Angular2-express

```

### Step2

Create the `package.json` (for dependencies) and `app.js` (for bootstrapping)  for our `node.js` app.

**package.json:**

```js
{
  "name": "Angular2-express",
  "version": "1.0.0",
  "description": "",
  "scripts": {
    "start": "node app.js"
  },
  "author": "",
  "license": "ISC",
  "dependencies": {
    "body-parser": "^1.13.3",
    "express": "^4.13.3"
  }
}

```

**app.js:**

```js
var express = require('express');
var app = express();
var server = require('http').Server(app);
var bodyParser = require('body-parser');

server.listen(process.env.PORT || 9999, function(){
    console.log("Server connected. Listening on port: " + (process.env.PORT || 9999));
});

app.use(bodyParser.json());
app.use(bodyParser.urlencoded({extended: true}) );

app.use( express.static(__dirname + '/front' ) );

app.get('/test', function(req,res){ //example http request receiver
  return res.send(myTestVar);
});

//send the index.html on every page refresh and let angular handle the routing
app.get('/*',  function(req, res, next) {
    console.log("Reloading");
    res.sendFile('index.html', { root: __dirname }); 
});

```

Then run an `npm install` or `yarn` to install the dependencies.

Now our back-end structure is complete. Let's move on to the front-end.

### Step3

Our front-end should be in a folder named `front` inside our `Angular2-express` folder.

**command line:**

```js
mkdir front
cd front

```

Just like we did with our back-end our front-end needs the dependency files too. Let's go ahead and create the following files: `package.json`, `systemjs.config.js`, `tsconfig.json`

**package.json**:

```js
{
  "name": "Angular2-express",
  "version": "1.0.0",
  "scripts": {
    "tsc": "tsc",
    "tsc:w": "tsc -w"
  },
  "licenses": [
    {
      "type": "MIT",
      "url": "https://github.com/angular/angular.io/blob/master/LICENSE"
    }
  ],
  "dependencies": {
    "@angular/common": "~2.4.1",
    "@angular/compiler": "~2.4.1",
    "@angular/compiler-cli": "^2.4.1",
    "@angular/core": "~2.4.1",
    "@angular/forms": "~2.4.1",
    "@angular/http": "~2.4.1",
    "@angular/platform-browser": "~2.4.1",
    "@angular/platform-browser-dynamic": "~2.4.1",
    "@angular/platform-server": "^2.4.1",
    "@angular/router": "~3.4.0",
    "core-js": "^2.4.1",
    "reflect-metadata": "^0.1.8",
    "rxjs": "^5.0.2",
    "systemjs": "0.19.40",
    "zone.js": "^0.7.4"
  },
  "devDependencies": {
    "@types/core-js": "^0.9.34",
    "@types/node": "^6.0.45",
    "typescript": "2.0.2"
  }
}

```

**systemjs.config.js:**

```js
/**
 * System configuration for Angular samples
 * Adjust as necessary for your application needs.
 */
(function (global) {
  System.config({
    defaultJSExtensions:true,
    paths: {
      // paths serve as alias
      'npm:': 'node_modules/'
    },
    // map tells the System loader where to look for things
    map: {
      // our app is within the app folder
      app: 'app',
      // angular bundles
      '@angular/core': 'npm:@angular/core/bundles/core.umd.js',
      '@angular/common': 'npm:@angular/common/bundles/common.umd.js',
      '@angular/compiler': 'npm:@angular/compiler/bundles/compiler.umd.js',
      '@angular/platform-browser': 'npm:@angular/platform-browser/bundles/platform-browser.umd.js',
      '@angular/platform-browser-dynamic': 'npm:@angular/platform-browser-dynamic/bundles/platform-browser-dynamic.umd.js',
      '@angular/http': 'npm:@angular/http/bundles/http.umd.js',
      '@angular/router': 'npm:@angular/router/bundles/router.umd.js',
      '@angular/forms': 'npm:@angular/forms/bundles/forms.umd.js',
      // other libraries
      'rxjs':                      'npm:rxjs',
      'angular-in-memory-web-api': 'npm:angular-in-memory-web-api',
    },
    // packages tells the System loader how to load when no filename and/or no extension
    packages: {
      app: {
        main: './main.js',
        defaultExtension: 'js'
      },
      rxjs: {
        defaultExtension: 'js'
      }
    }
  });
})(this);

```

**tsconfig.json:**

```js
{
  "compilerOptions": {
    "target": "es5",
    "module": "commonjs",
    "moduleResolution": "node",
    "sourceMap": true,
    "emitDecoratorMetadata": true,
    "experimentalDecorators": true,
    "removeComments": false,
    "noImplicitAny": false
  },
  "compileOnSave": true,
  "exclude": [
    "node_modules/*"
  ]
}

```

Then run an `npm install` or `yarn` to install the dependencies.

Now that our dependency files are complete. Let's move on to our `index.html`:

**index.html:**

```js
<html>
  <head>
    <base href="/">
    <title>Angular2-express</title>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <!-- 1. Load libraries -->
     <!-- Polyfill(s) for older browsers -->
    <script src="node_modules/core-js/client/shim.min.js"></script>
    <script src="node_modules/zone.js/dist/zone.js"></script>
    <script src="node_modules/reflect-metadata/Reflect.js"></script>
    <script src="node_modules/systemjs/dist/system.src.js"></script>
    <!-- 2. Configure SystemJS -->
    <script src="systemjs.config.js"></script>
    <script>
      System.import('app').catch(function(err){ console.error(err); });
    </script>
    
  </head>
  <!-- 3. Display the application -->
  <body>
    <my-app>Loading...</my-app>
  </body>
</html>

```

Now we're ready to create our first component. Create a folder named `app` inside our `front` folder.

**command line:**

```js
mkdir app
cd app

```

Let's make the following files named `main.ts`, `app.module.ts`, `app.component.ts`

**main.ts:**

```js
import { platformBrowserDynamic } from '@angular/platform-browser-dynamic';

import { AppModule } from './app.module';

const platform = platformBrowserDynamic();
platform.bootstrapModule(AppModule);

```

**app.module.ts:**

```js
import { NgModule }      from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { HttpModule } from "@angular/http";

import { AppComponent }   from './app.component';

@NgModule({
  imports:      [ 
    BrowserModule,
    HttpModule    
  ],
  declarations: [ 
    AppComponent
  ],
  providers:[ ],
  bootstrap:    [ AppComponent ]
})
export class AppModule {}

```

**app.component.ts:**

```js
import { Component } from '@angular/core';
import { Http } from '@angular/http';

@Component({
  selector: 'my-app',
  template: 'Hello World!',
  providers: []
})
export class AppComponent { 
  constructor(private http: Http){
    //http get example
    this.http.get('/test')
     .subscribe((res)=>{
       console.log(res);
     });
  }
}

```

After this, compile the typescript files to javascript files. Go 2 levels up from the current dir (inside Angular2-express folder) and run the command below.

**command line:**

```js
cd ..
cd ..
tsc -p front

```

Our folder structure should look like:

```js
Angular2-express
├── app.js
├── node_modules
├── package.json
├── front
│   ├── package.json
│   ├── index.html
│   ├── node_modules
│   ├── systemjs.config.js
│   ├── tsconfig.json
│   ├── app
│   │   ├── app.component.ts
│   │   ├── app.component.js.map
│   │   ├── app.component.js
│   │   ├── app.module.ts
│   │   ├── app.module.js.map
│   │   ├── app.module.js
│   │   ├── main.ts
│   │   ├── main.js.map
│   │   ├── main.js

```

Finally, inside Angular2-express folder, run `node app.js` command in the command line. Open your favorite browser and check `localhost:9999` to see your app.



## Getting through that pesky company proxy


If you are attempting to get an Angular2 site running on your Windows work computer at XYZ MegaCorp the chances are that you are having problems getting through the company proxy.

There are (at least) two package managers that need to get through the proxy:

1. NPM
1. Typings

For NPM you need to add the following lines to the `.npmrc` file:

```js
proxy=http://[DOMAIN]%5C[USER]:[PASS]@[PROXY]:[PROXYPORT]/
https-proxy=http://[DOMAIN]%5C[USER]:[PASS]@[PROXY]:[PROXYPORT]/

```

For Typings you need to add the following lines to the `.typingsrc` file:

```js
proxy=http://[DOMAIN]%5C[USER]:[PASS]@[PROXY]:[PROXYPORT]/
https-proxy=http://[DOMAIN]%5C[USER]:[PASS]@[PROXY]:[PROXYPORT]/
rejectUnauthorized=false

```

These files probably don't exist yet, so you can create them as blank text files. They can be added to the project root (same place as `package.json` or you can put them in `%HOMEPATH%` and they will be available to all your projects.

The bit that isn't obvious and is the main reason people think the proxy settings aren't working is the `%5C` which is the URL encode of the `\` to separate the domain and user names. Thanks to Steve Roberts for that one: [Using npm behind corporate proxy .pac](http://stackoverflow.com/questions/25660936/using-npm-behind-corporate-proxy-pac)



## Keeping Visual Studios in sync with NPM and NODE Updates


**Step 1:** Locate your download of Node.js, typically it is installed under C:/program files/nodejs

**Step 2:** Open Visual Studios and navigate to "Tools>Options"

**Step 3:** In the options window navigate to "Projects and Solutions>External Web Tools"

**Step 4:** Add new entry with you Node.js file location (C:/program files/nodejs), IMPORTANT use the arrow buttons on menu to move your reference to the top of the list.

[<img src="http://i.stack.imgur.com/trZa7.png" alt="enter image description here" />](http://i.stack.imgur.com/trZa7.png)

**Step 5:** Restart Visual Studios and Run an npm install, against your project, from npm command window



## Let's dive into Angular 4!


Angular 4 is now available! Actually Angular uses semver since Angular 2, which requires the major number being increased when breaking changes were introduced. The Angular team postponed features that cause breaking changes, which will be released with Angular 4. Angular 3 was skipped to be able to align the version numbers of the core modules, because the Router already had version 3.

As per the Angular team, Angular 4 applications are going to be less space consuming and faster than before. They have separated animation package from @angular/core package. If anybody is not using animation package so extra space of code will not end up in the production. The template binding syntax now supports if/else style syntax. Angular 4 is now compatible with most recent version of Typescript 2.1 and 2.2. So, Angular 4 is going to be more exciting.

Now I’ll show you how to do setup of Angular 4 in your project.

**Let’s start Angular setup with three different ways:**

You can use Angular-CLI  (Command Line Interface) , It will install all dependencies for you.

<li>
You can migrate from Angular 2 to Angular 4.
</li>
<li>
You can use github and clone the Angular4-boilerplate. (It is the easiest one.😉 )
</li>
<li>
Angular Setup using Angular-CLI(command Line Interface).
</li>

Before You start using Angular-CLI , make sure You have node installed in your machine. Here, I am using node v7.8.0. Now, Open your terminal and type the following command for Angular-CLI.

```js
npm install -g @angular/cli

```

or

```js
yarn global add @angular/cli

```

depending on the package manager you use.

Let’s install Angular 4 using Angular-CLI.

```js
ng new Angular4-boilerplate

```

cd Angular4-boilerplate
We are all set for Angular 4. Its pretty easy and straightforward method.😌

Angular Setup by migrating from Angular 2 to Angular 4

Now Let’s see the second approach. I ll show you how to migrate Angular 2 to Angular 4. For that You need clone any Angular 2 project and update Angular 2 dependencies with the Angular 4 Dependency in your package.json as following:

```js
"dependencies": {
    "@angular/animations": "^4.1.0",
    "@angular/common": "4.0.2",
    "@angular/compiler": "4.0.2",
    "@angular/core": "^4.0.1",
    "@angular/forms": "4.0.2",
    "@angular/http": "4.0.2",
    "@angular/material": "^2.0.0-beta.3",
    "@angular/platform-browser": "4.0.2",
    "@angular/platform-browser-dynamic": "4.0.2",
    "@angular/router": "4.0.2",
    "typescript": "2.2.2"
   }

```

These are the main dependencies for Angular 4. Now You can npm install and then npm start to run the application. For reference my package.json.

**Angular setup from github project**

Before starting this step make sure you have git installed in your machine. Open your terminal and clone the angular4-boilerplate using below command:

```js
git@github.com:CypherTree/angular4-boilerplate.git

```

Then install all dependencies and run it.

```js
npm install

npm start

```

And you are done with the Angular 4 setup. All the steps are very straightforward so you can opt any of them.

**Directory Structure of the angular4-boilerplate**

```js
Angular4-boilerplate
-karma
-node_modules
-src
   -mocks
   -models
      -loginform.ts
      -index.ts
   -modules
      -app
        -app.component.ts
     -app.component.html
      -login
    -login.component.ts
    -login.component.html
    -login.component.css
      -widget
     -widget.component.ts
     -widget.component.html
     -widget.component.css
    ........    
   -services
       -login.service.ts
    -rest.service.ts
   -app.routing.module.ts
   -app.module.ts
   -bootstrap.ts
   -index.html
   -vendor.ts
-typings
-webpack
-package.json
-tsconfig.json
-tslint.json
-typings.json 

```

Basic understanding for Directory structure:

All the code resides in src folder.

mocks folder is for mock data that is used in testing purpose.

model folder contains the class and interface that used in component.

modules folder contains list of components such as app, login, widget etc. All component contains typescript, html and css file. index.ts is for exporting all the class.

services folder contains list of services used in application. I have separated rest service and different component service. In rest service contains different http methods. Login service works as mediator between login component and rest service.

app.routing.ts file describes all possible routes for the application.

app.module.ts describes app module as root component.

bootstrap.ts will run the whole application.

webpack folder contains webpack configuration file.

package.json file is for all list of dependencies.

karma contains karma configuration for unit test.

node_modules contains list of package bundles.

Lets start with Login component. In login.component.html

```js
<form>Dreamfactory - Addressbook 2.0 
 <label>Email</label> <input id="email" form="" name="email" type="email" /> 
 <label>Password</label> <input id="password" form="" name="password" 
 type="password" /> 
 <button form="">Login</button>
</form>

```

In login.component.ts

```js
import { Component } from '@angular/core';
import { Router } from '@angular/router';
import { Form, FormGroup } from '@angular/forms';
import { LoginForm } from '../../models';
import { LoginService } from '../../services/login.service';

@Component({
    selector: 'login',
    template: require('./login.component.html'),
    styles: [require('./login.component.css')]
})
export class LoginComponent {

    constructor(private loginService: LoginService, private router: Router, form: LoginForm) { }

    getLogin(form: LoginForm): void {
      let username = form.email;
      let password = form.password;
      this.loginService.getAuthenticate(form).subscribe(() => {
          this.router.navigate(['/calender']);
      });
    }
}

```

We need to export this component to in index.ts.

```js
export * from './login/login.component';

```

we need to set routes for login in app.routes.ts

```js
const appRoutes: Routes = [
   {
       path: 'login',
       component: LoginComponent
   },
   ........
   {
       path: '',
       pathMatch: 'full',
       redirectTo: '/login'
   }
];

```

In root component, app.module.ts  file you just need to import that component.

```js
.....
import { LoginComponent } from './modules';
......
@NgModule({
    bootstrap: [AppComponent],
    declarations: [
       LoginComponent
       .....
       .....
       ]
      ..... 
  })
  export class AppModule { }

```

and after that npm install and npm start. Here, you go! You can check login screen in your localhost. In case of any difficulty, You can refer the angular4-boilerplate.

Basically I can feel less building package and more faster response with Angular 4 application and Although I found Exactly similar to Angular 2 in coding.



#### Remarks


This section provides an overview of how to install and configure Angular2+ for use in various environments and IDE's using tools like the community developed [angular-cli](https://cli.angular.io/).

The previous version of Angular is [AngularJS](https://angularjs.org/) or also named Angular 1. See here the [documentation](http://stackoverflow.com/documentation/angularjs/topics).

