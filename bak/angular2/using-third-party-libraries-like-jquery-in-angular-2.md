---
metaTitle: "Angular 2 - Using third party libraries like jQuery in Angular 2"
description: "Configuration using angular-cli, Using jQuery in Angular 2.x components"
---

# Using third party libraries like jQuery in Angular 2


When building applications using Angular 2.x there are times when it's required to use any third party libraries like jQuery, Google Analytics, Chat Integration JavaScript APIs and etc.



## Configuration using angular-cli


### NPM

If external library like `jQuery` is installed using NPM

`npm install --save jquery`

Add script path into your `angular-cli.json`

```js
"scripts": [
    "../node_modules/jquery/dist/jquery.js"
]

```

### Assets Folder

You can also save the library file in your `assets/js` directory and include the same in `angular-cli.json`

```js
"scripts": [
    "assets/js/jquery.js"
]

```

### Note

Save your main library `jquery` and their dependencies like `jquery-cycle-plugin` into the assets directory and add both of them into `angular-cli.json`, make sure the order is maintained for the dependencies.



## Using jQuery in Angular 2.x components


To use `jquery` in your Angular 2.x components, declare a global variable on the top

If using `$` for jQuery

`declare var $: any;`

If using `jQuery` for jQuery

`declare var jQuery: any`

This will allow using `$` or `jQuery` into your Angular 2.x component.

