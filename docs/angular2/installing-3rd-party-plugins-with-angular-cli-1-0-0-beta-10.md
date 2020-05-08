---
metaTitle: "Angular 2 - Installing 3rd party plugins with angular-cli@1.0.0-beta.10"
description: "Adding jquery library in angular-cli project, Add 3rd party library that does not have typings"
---

# Installing 3rd party plugins with angular-cli@1.0.0-beta.10



## Adding jquery library in angular-cli project


1. Install jquery via npm :

```

npm install jquery --save 

```

> 
Install typings for the library:


To add typings for a library, do the following:

```js
typings install jquery --global --save

```


<li>
Add jquery to angular-cli-build.js file to vendorNpmFiles array:
This is required so the build system will pick up the file. After setup the angular-cli-build.js should look like this:
</li>

> 
Browse the `node_modules` and look for files and folders you want to add to the vendor folder.


```js
var Angular2App = require('angular-cli/lib/broccoli/angular2-app');

module.exports = function(defaults) {
  return new Angular2App(defaults, {
    vendorNpmFiles: [
      // ...
      'jquery/dist/*.js'


    ]
  });
};

```


<li>
Configure SystemJS mappings to know where to look for jquery :
SystemJS configuration is located in system-config.ts and after the custom configuration is done the related section should look like:
</li>

```js
/** Map relative paths to URLs. */
const map: any = {
  'jquery': 'vendor/jquery'
};

/** User packages configuration. */
const packages: any = {
            
// no need to add anything here for jquery

};

```


1. In your src/index.html add this line

```js
<script src="vendor/jquery/dist/jquery.min.js" type="text/javascript"></script>

```

> 
Your other options are:


```js
<script src="vendor/jquery/dist/jquery.js" type="text/javascript"></script>

```

or

```js
<script src="/vendor/jquery/dist/jquery.slim.js" type="text/javascript"></script>

```

and

```js
<script src="/vendor/jquery/dist/jquery.slim.min.js" type="text/javascript"></script>

```


<li>
Importing and using jquery library in your project source files:
Import jquery library in your source .ts files like this:
</li>

```js
declare var $:any;

@Component({
})
export class YourComponent {
  ngOnInit() {
    $.("button").click(function(){
       // now you can DO, what ever you want 
     });
     console.log();
  }
}

```

If you followed the steps correctly you should now have jquery library working in your project. Enjoy!



## Add 3rd party library that does not have typings


> 
Notice, this is only for angular-cli up to 1.0.0-beta.10 version !


Some libraries or plugins may not have typings. Without these, TypeScript can't type check them and therefore causes compilation errors. These libraries can still be used but differently than imported modules.

<li>
Include a script reference to the library on your page (`index.html`)

```js
<script src="//cdn.somewhe.re/lib.min.js" type="text/javascript"></script>
<script src="/local/path/to/lib.min.js" type="text/javascript"></script>

```


<ul>
1. These scripts should add a global (eg. `THREE`, `mapbox`, `$`, etc.) or attach to a global
</ul>
</li>
<li>
In the component that requires these, use `declare` to initialize a variable matching the global name used by the lib. This lets TypeScript know that it has already been initialized. [<sup>1</sup>](http://stackoverflow.com/a/13252853/2554793)

```js
declare var <globalname>: any;

```


Some libs attach to `window`, which would need to be extended in order to be accessible in the app.

```js
interface WindowIntercom extends Window { Intercom: any; }
declare var window: WindowIntercom;

```


</li>
<li>
Use the lib in your components as needed.

```js
@Component { ... }
export class AppComponent implements AfterViewInit {
    ...
    ngAfterViewInit() {
        var geometry = new THREE.BoxGeometry( 1, 1, 1 );
        window.Intercom('boot', { ... }
    }
}

```


<ul>
1. NOTE: Some libs may interact with the DOM and should be used in the appropriate [component lifecycle](https://angular.io/docs/ts/latest/guide/lifecycle-hooks.html) method.
</ul>
</li>

- NOTE: Some libs may interact with the DOM and should be used in the appropriate [component lifecycle](https://angular.io/docs/ts/latest/guide/lifecycle-hooks.html) method.



#### Remarks


It is possible to install other libraries following, this approach, however, there might be a need to specify the module type,  main file, and default extension.

```

'lodash': {
   format: 'cjs',
   defaultExtension: 'js',
   main: 'index.js'
 }

```

```js
 'moment': {
   main: 'moment.js'
 }

```

