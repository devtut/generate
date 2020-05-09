---
metaTitle: "AngularJS - Prepare for Production - Grunt"
description: "View preloading, Script optimisation "
---

# Prepare for Production - Grunt



## View preloading


When the first time view is requested, normally Angular makes `XHR` request to get that view. For mid-size projects, the view count can be significant and it can slow down the application responsiveness.

The **good practice** **is to pre-load** all the views at once for small and mid size projects. For larger projects it is good to aggregate them in some meaningful bulks as well, but some other methods can be handy to split the load. To automate this task it is handy to use Grunt or Gulp tasks.

To pre-load the views, we can use `$templateCache` object. That is an object, where angular stores every received view from the server.

It is possible to use `html2js` module, that will convert all our views to one module - js file. Then we will need to inject that module into our application and that's it.

To create concatenated file of all the views we can use this task

```js
module.exports = function (grunt) {
 //set up the location of your views here
 var viewLocation = ['app/views/**.html'];
 
 grunt.initConfig({
        pkg: require('./package.json'),
            //section that sets up the settings for concatenation of the html files into one file
            html2js: {
                        options: {
                            base: '',
                            module: 'app.templates', //new module name
                            singleModule: true,
                            useStrict: true,
                            htmlmin: {
                                collapseBooleanAttributes: true,
                                collapseWhitespace: true
                            }
                        },
                        main: {
                            src: viewLocation,
                            dest: 'build/app.templates.js'
                        }
                    },
            //this section is watching for changes in view files, and if there was a change, it will regenerate the production file. This task can be handy during development.
            watch: {
                views:{
                    files: viewLocation,
                    tasks: ['buildHTML']
                },
            }
        });
        
        //to automatically generate one view file
        grunt.loadNpmTasks('grunt-html2js');
        
        //to watch for changes and if the file has been changed, regenerate the file
        grunt.loadNpmTasks('grunt-contrib-watch');
        
        //just a task with friendly name to reference in watch
        grunt.registerTask('buildHTML', ['html2js']);
};

```

To use this way of concatination, you need to make 2 changes:
In your `index.html` file you need to reference the concatenated view file

```js
<script src="build/app.templates.js"></script>

```

In the file, where you are declaring your app, you need to inject the dependency

```js
angular.module('app', ['app.templates'])

```

If you are using popular routers like `ui-router`, there are no changes in the way, how you are referencing templates

```

   .state('home', {
        url: '/home',
        views: {
            "@": {
                controller: 'homeController',
                //this will be picked up from $templateCache
                templateUrl: 'app/views/home.html'
            },
        }

    })

```



## Script optimisation 


It is **good practice to combine JS files together** and minify them. For larger project there could be hundreds of JS files and it adds unnecessary latency to load each file separately from the server.

For angular minification it is required to to have all functions annotated. That in necessary for Angular dependency injection proper minificaiton. (During minification, function names and variables will be renamed and it will break dependency injection if no extra actions will be taken.)

During minificaiton `$scope` and `myService` variables will be replaced by some other values. Angular dependency injection works based on the name, as a result, these names shouldn't change

```js
.controller('myController', function($scope, myService){
})

```

Angular will understand the array notation, because minification won't replace string literals.

```js
.controller('myController', ['$scope','myService', function($scope, myService){
}])

```


- Firstly we will concatinate all files end to end.
- Secondly we will use `ng-annotate` module, that will prepare code for minification
- Finally we will apply `uglify` module.

module.exports = function (grunt) {
//set up the location of your scripts here for reusing it in code
var scriptLocation = ['app/scripts/*.js'];

```

grunt.initConfig({
        pkg: require('./package.json'),
            //add necessary annotations for safe minification
         ngAnnotate: {
            angular: {
                src: ['staging/concatenated.js'],
                dest: 'staging/anotated.js'
            }
        },
        //combines all the files into one file
        concat: {
                js: {
                    src: scriptLocation,
                    dest: 'staging/concatenated.js'
                }
            },
        //final uglifying
        uglify: {
            options: {
                report: 'min',
                mangle: false,
                sourceMap:true
            },
            my_target: {
                files: {
                    'build/app.min.js': ['staging/anotated.js']
                }
            }
        },
        
        //this section is watching for changes in JS files, and if there was a change, it will regenerate the production file. You can choose not to do it, but I like to keep concatenated version up to date
        watch: {
            scripts: {
                files: scriptLocation,
                tasks: ['buildJS']
            }
        }
            
});

    //module to make files less readable
    grunt.loadNpmTasks('grunt-contrib-uglify');
    
    //mdule to concatenate files together
    grunt.loadNpmTasks('grunt-contrib-concat');
    
    //module to make angularJS files ready for minification
    grunt.loadNpmTasks('grunt-ng-annotate');
    
    //to watch for changes and if the file has been changed, regenerate the file
    grunt.loadNpmTasks('grunt-contrib-watch');
    
    //task that sequentially executes all steps to prepare JS file for production
    //concatinate all JS files
    //annotate JS file (prepare for minification
    //uglify file
     grunt.registerTask('buildJS', ['concat:js', 'ngAnnotate', 'uglify']);
};

```

