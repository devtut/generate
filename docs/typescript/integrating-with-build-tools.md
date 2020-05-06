---
metaTitle: "TypeScript - Integrating with Build Tools"
description: "Browserify, Webpack, Grunt, Gulp, MSBuild, NuGet, Install and configure webpack + loaders"
---

# Integrating with Build Tools



## Browserify


### Install

```js
npm install tsify

```

### Using Command Line Interface

```js
browserify main.ts -p [ tsify --noImplicitAny ] > bundle.js

```

### Using API

```js
var browserify = require("browserify");
var tsify = require("tsify");

browserify()
  .add("main.ts")
  .plugin("tsify", { noImplicitAny: true })
  .bundle()
  .pipe(process.stdout);

```

More details: [smrq/tsify](https://github.com/smrq/tsify)



## Webpack


### Install

```js
npm install ts-loader --save-dev

```

### Basic webpack.config.js

### webpack 2.x, 3.x

```js
module.exports = {
    resolve: {
        extensions: ['.ts', '.tsx', '.js']
    },
    module: {
        rules: [
            {
                // Set up ts-loader for .ts/.tsx files and exclude any imports from node_modules.
                test: /\.tsx?$/,
                loaders: ['ts-loader'],
                exclude: /node_modules/
            }
        ]
    },
    entry: [
        // Set index.tsx as application entry point.
        './index.tsx'
    ],
    output: {
      filename: "bundle.js"
    }
};

```

### webpack 1.x

See more details on [ts-loader here](https://www.npmjs.com/package/ts-loader).

Alternatives:

- [awesome-typescript-loader](https://www.npmjs.com/package/awesome-typescript-loader)



## Grunt


### Install

```js
npm install grunt-ts

```

### Basic Gruntfile.js

More details: [TypeStrong/grunt-ts](https://github.com/TypeStrong/grunt-ts)



## Gulp


### Install

```js
npm install gulp-typescript

```

### Basic gulpfile.js

### gulpfile.js using an existing tsconfig.json

More details: [ivogabe/gulp-typescript](https://github.com/ivogabe/gulp-typescript)



## MSBuild


Update project file to include locally installed `Microsoft.TypeScript.Default.props` (at the top) and `Microsoft.TypeScript.targets` (at the bottom) files:

More details about defining MSBuild compiler options: [Setting Compiler Options in MSBuild projects](http://www.typescriptlang.org/docs/handbook/compiler-options-in-msbuild.html)



## NuGet


- Right-Click -> Manage NuGet Packages
- Search for `Microsoft.TypeScript.MSBuild`
- Hit `Install`
- When install is complete, rebuild!

More details can be found at [Package Manager Dialog](http://docs.nuget.org/Consume/Package-Manager-Dialog) and [using nightly builds with NuGet](https://github.com/Microsoft/TypeScript/wiki/Nightly-drops#using-nuget-with-msbuild)



## Install and configure webpack + loaders


Installation

```js
npm install -D webpack typescript ts-loader

```

webpack.config.js

```js
module.exports = {
  entry: {
    app: ['./src/'],
  },
  output: {
    path: __dirname,
    filename: './dist/[name].js',
  },
  resolve: {
    extensions: ['', '.js', '.ts'],
  },
  module: {
    loaders: [{
      test: /\.ts(x)$/, loaders: ['ts-loader'], exclude: /node_modules/
    }],
  }
};

```



#### Remarks


For more information you can go on official web page [typescript integrating with build tools](http://www.typescriptlang.org/docs/handbook/integrating-with-build-tools.html)

