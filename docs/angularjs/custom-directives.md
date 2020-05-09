---
metaTitle: "AngularJS - Custom Directives"
description: "Creating and consuming custom directives, Directive Definition Object Template, How to create resuable component using directive, Basic Directive example, Basic directive with template and an isolated scope, Building a reusable component, Directive decorator, Directive inheritance and interoperability"
---

# Custom Directives


Here you will learn about the Directives feature of AngularJS. Below you will find information on what Directives are, as well as Basic and Advanced examples of how to use them.



## Creating and consuming custom directives


Directives are one of the most powerful features of angularjs. Custom angularjs directives are used to extend functionality of html by creating new html elements or custom attributes to provide certain behavior to an html tag.

**directive.js**

```js
// Create the App module if you haven't created it yet
var demoApp= angular.module("demoApp", []);

// If you already have the app module created, comment the above line and create a reference of the app module
var demoApp = angular.module("demoApp"); 


// Create a directive using the below syntax
// Directives are used to extend the capabilities of html element 
// You can either create it as an Element/Attribute/class      
// We are creating a directive named demoDirective. Notice it is in CamelCase when we are defining the directive just like ngModel
// This directive will be activated as soon as any this element is encountered in html

demoApp.directive('demoDirective', function () {
    
  // This returns a directive definition object
  // A directive definition object is a simple JavaScript object used for configuring the directive’s behaviour,template..etc
  return {
    // restrict: 'AE', signifies that directive is Element/Attribute directive, 
    // "E" is for element, "A" is for attribute, "C" is for class, and "M" is for comment. 
    // Attributes are going to be the main ones as far as adding behaviors that get used the most.
    // If you don't specify the restrict property it will default to "A"
    restrict :'AE',  

    // The values of scope property decides how the actual scope is created and used inside a directive. These values can be either “false”, “true” or “{}”. This creates an isolate scope for the directive.
    // '@' binding is for passing strings. These strings support { {} } expressions for interpolated values.
    // '=' binding is for two-way model binding. The model in parent scope is linked to the model in the directive's isolated scope.
    // '&' binding is for passing a method into your directive's scope so that it can be called within your directive. 
    // The method is pre-bound to the directive's parent scope, and supports arguments.
    scope: { 
      name: "@",  // Always use small casing here even if it's a mix of 2-3 words
    },                  

    // template replaces the complete element with its text. 
    template: "<div>Hello { {name} }!</div>",
                
    // compile is called during application initialization. AngularJS calls it once when html page is loaded.
    compile: function(element, attributes) {
      element.css("border", "1px solid #cccccc");
                    
      // linkFunction is linked with each element with scope to get the element specific data.
      var linkFunction = function($scope, element, attributes) {
        element.html("Name: <b>"+$scope.name +"</b>");
        element.css("background-color", "#ff00ff");
      };
      return linkFunction;
    }
  };
});

```

This directive can then be used in App as :

```js
<html>
   
   <head>
      <title>Angular JS Directives</title>
   </head>
   <body>
   <script src = "http://ajax.googleapis.com/ajax/libs/angularjs/1.3.14/angular.min.js"></script>
   <script src="directive.js"></script>   
    <div ng-app = "demoApp">
       <!-- Notice we are using Spinal Casing here -->  
       <demo-directive name="World"></demo-directive>
     
      </div>
   </body>
</html>

```



## Directive Definition Object Template


```js
demoApp.directive('demoDirective', function () {
  var directiveDefinitionObject = {
    multiElement:  
    priority:
    terminal:
    scope: {},  
    bindToController: {},
    controller:
    controllerAs:
    require:
    restrict:
    templateNamespace:
    template:
    templateUrl:
    transclude:
    compile: 
    link: function(){}                                  
  };
  return directiveDefinitionObject;
});

```


1. **`multiElement`** - set to true and any DOM nodes between the start and end of the directive name will be collected and grouped together as directive elements
1. **`priority`** - allows specification of the order to apply directives when multiple directives are defined on a single DOM element. Directives with higher numbers are compiled first.
1. **`terminal`** - set to true and the current priority will be the last set of directives to execute
1. **`scope`** - sets scope of the directive
1. **`bind to controller`** - binds scope properties directly to directive controller
1. **`controller`** - controller constructor function
1. **`require`** - require another directive and inject its controller as the fourth argument to the linking function
1. **`controllerAs`** - name reference to the controller in the directive scope to allow the controller to be referenced from the directive template.
1. **`restrict`** - restrict directive to Element, Attribute, Class, or Comment
1. **`templateNameSpace`** - sets document type used by directive template: html, svg, or math. html is the default
1. **`template`** - html markup that defaults to replacing the content of the directive's element, or wraps the contents of the directive element if transclude is true
1. **`templateUrl`** - url provided asynchronously for the template
1. **`transclude`** - Extract the contents of the element where the directive appears and make it available to the directive. The contents are compiled and provided to the directive as a transclusion function.
1. **`compile`** - function to transform the template DOM
1. **`link`** - only used if the compile property is not defined. The link function is responsible for registering DOM listeners as well as updating the DOM. It is executed after the template has been cloned.



## How to create resuable component using directive


AngularJS directives are what controls the rendering of the HTML inside an AngularJS application. They can be an Html element, attribute, class or a comment. Directives are used to manipulate the DOM, attaching new behavior to HTML elements, data binding and many more. Some of examples of directives which angular provides are ng-model, ng-hide, ng-if.

Similarly one can create his own custom directive and make them resuable. For creating Custom directives [Reference](http://stackoverflow.com/documentation/angularjs/965/custom-directives/3151/creating-and-consuming-custom-directives#t=201607221330234981046). The sense behind creating reusable directives is to make a set of directives/components written by you just like angularjs provides us using angular.js . These reusable directives can be particularly very helpful when you have suite of applications/application which requires a consistent behavior, look and feel. An example of such reusable component can be a simple toolbar which you may want to use across your application or different applications but you want them to behave the same or look the same.

Firstly , Make a folder named resuableComponents in your app Folder and make reusableModuleApp.js

**reusableModuleApp.js:**

```js
(function(){

 var reusableModuleApp = angular.module('resuableModuleApp', ['ngSanitize']);

 //Remember whatever dependencies you have in here should be injected in the app module where it is intended to be used or it's scripts should be included in your main app  
                               //We will be injecting ng-sanitize

 resubaleModuleApp.directive('toolbar', toolbar)

 toolbar.$inject=['$sce'];

 function toolbar($sce){

     return{ 
     restrict :'AE',
        //Defining below isolate scope actually provides window for the directive to take data from app that will be using this.
        scope : {
                value1: '=',
                value2: '=',
                },

         }
     template : '<ul>  <li><a ng-click="Add()" href="">{ {value1} }</a></li>  <li><a ng-click="Edit()" href="#">{ {value2} }</a></li> </ul> ',
         link : function(scope, element, attrs){
           
              //Handle's Add function
              scope.Add = function(){


              };

              //Handle's Edit function
              scope.Edit = function(){

              };
     }
  }

});

```

**mainApp.js:**

```js
(function(){
   var mainApp = angular.module('mainApp', ['reusableModuleApp']); //Inject resuableModuleApp in your application where you want to use toolbar component
   
   mainApp.controller('mainAppController', function($scope){
      $scope.value1 = "Add";
      $scope.value2 = "Edit"; 
   
   });
 
 });

```

**index.html:**

```

<!doctype html>
 <html ng-app="mainApp">
 <head>
  <title> Demo Making a reusable component
 <head>
  <body ng-controller="mainAppController">
   
     <!-- We are providing data to toolbar directive using mainApp'controller -->
     <toolbar value1="value1" value2="value2"></toolbar>
  
    <!-- We need to add the dependent js files on both apps here -->
     <script src="js/angular.js"></script>
     <script src="js/angular-sanitize.js"></script>
    
     <!-- your mainApp.js should be added afterwards --->
      <script src="mainApp.js"></script>

     <!-- Add your reusable component js files here -->
      <script src="resuableComponents/reusableModuleApp.js"></script>

  </body>
 </html>

```

Directive are reusable components by default. When you make directives in separate angular module, It actually makes it exportable and reusable across different angularJs applications.
New directives can simply be added inside reusableModuleApp.js and  reusableModuleApp can have it's own controller, services, DDO object inside directive to define the behavior.



## Basic Directive example


**superman-directive.js**

```js
angular.module('myApp', [])
  .directive('superman', function() {
    return {
      // restricts how the directive can be used
      restrict: 'E',
      templateUrl: 'superman-template.html',
      controller: function() {
        this.message = "I'm superman!"
      },
      controllerAs: 'supermanCtrl',
      // Executed after Angular's initialization. Use commonly 
      // for adding event handlers and DOM manipulation
      link: function(scope, element, attributes) {
        element.on('click', function() {
          alert('I am superman!')
        });
      }
    }
  });

```

**superman-template.html**

```js
<h2>{ {supermanCtrl.message} }</h2>

```

**index.html**

```js
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>Document</title>
  <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.5.0/angular.js"></script>
  <script src="superman-directive.js"><script/>
</head>
<body>
<div ng-app="myApp">
  <superman></superman>
</div>
</body>
</html>

```

You can check out more about directive's `restrict` and `link` functions on [AngularJS's official documentation on Directives](https://docs.angularjs.org/guide/directive)



## Basic directive with template and an isolated scope


Creating a custom directive with **isolated scope** will separate the scope **inside** the directive from the **outside** scope, in order to prevent our directive from accidentally change the data in the parent scope and restricting it from reading private data from the parent scope.

To create an isolated scope and still allow our custom directive to communicate with the outside scope, we can use the `scope` option that describe how to **map** the bindings of the directive's inner scope with the outside scope.

The actual bindings are made with extra **attributes** attached to the directive. The binding settings are defined with the `scope` option and an object with key-value pairs:

- A **key**, which is corresponded to our directive's isolated scope property

- A **value**, which tells Angular how do bind the directive inner scope to a matching **attribute**

Simple example of a directive with an isolated scope:

```js
var ProgressBar = function() {
  return {
    scope: { // This is how we define an isolated scope
      current: '=', // Create a REQUIRED bidirectional binding by using the 'current' attribute
      full: '=?maxValue' // Create an OPTIONAL (Note the '?'): bidirectional binding using 'max-value' attribute to the `full` property in our directive isolated scope 
    }
    template: '<div class="progress-back">' +
              '  <div class="progress-bar"' +
              '       ng-style="{width: getProgress()}">' +
              '  </div>' + 
              '</div>',
    link: function(scope, el, attrs) {
      if (scope.full === undefined) {
        scope.full = 100;
      }
      scope.getProgress = function() {
        return (scope.current / scope.size * 100) + '%';
      }
    }
  }
}

ProgressBar.$inject = [];
angular.module('app').directive('progressBar', ProgressBar);

```

Example how to use this directive and bind data from the controller's scope to the directive's inner scope:

**Controller:**

```js
angular.module('app').controller('myCtrl', function($scope) {
    $scope.currentProgressValue = 39;
    $scope.maxProgressBarValue = 50;
});

```

**View:**

```js
<div ng-controller="myCtrl">
    <progress-bar current="currentProgressValue"></progress-bar>
    <progress-bar current="currentProgressValue" max-value="maxProgressBarValue"></progress-bar>
</div>

```



## Building a reusable component


Directives can be used to build reusable components. Here is an example of a "user box" component:

**userBox.js**

```js
angular.module('simpleDirective', []).directive('userBox', function() {
  return {
    scope: {
        username: '=username',
        reputation: '=reputation'
    },
    templateUrl: '/path/to/app/directives/user-box.html'
  };
});

```

**Controller.js**

```js
var myApp = angular.module('myApp', ['simpleDirective']);

myApp.controller('Controller', function($scope) {

    $scope.user = "John Doe";
    $scope.rep = 1250;

    $scope.user2 = "Andrew";
    $scope.rep2 = 2850;

});

```

**myPage.js**

```js
<html lang="en" ng-app="myApp">
  <head>
    <script src="/path/to/app/angular.min.js"></script>
    <script src="/path/to/app/js/controllers/Controller.js"></script>
    <script src="/path/to/app/js/directives/userBox.js"></script>
  </head>

  <body>
  
    <div ng-controller="Controller">
        <user-box username="user" reputation="rep"></user-box>
        <user-box username="user2" reputation="rep2"></user-box>
    </div>
    
  </body>
</html>

```

**user-box.html**

```js
<div>{ {username} }</div>
<div>{ {reputation} } reputation</div>

```

**The result will be:**

```js
John Doe
1250 reputation
Andrew
2850 reputation

```



## Directive decorator


Sometimes you may need additional features from a directive. Instead of rewriting (copy) the directive, you can modify how the directive behaves.

The decorator will be executed during $inject phase.

To do so, provde a .config to your module.
The directive is called myDirective, so you have to config myDirectiveDirective. (this in an angular convention [read about providers] ).

This example will change the templateUrl of the directive:

```js
angular.module('myApp').config(function($provide){
        $provide.decorator('myDirectiveDirective', function($delegate){
             var directive = $delegate[0]; // this is the actual delegated, your directive
             directive.templateUrl = 'newTemplate.html'; // you change the directive template
        return $delegate;
    })
});

```

This example add an onClick event to the directive element when clicked, this happens during compile phase.

```js
angular.module('myApp').config(function ($provide) {
        $provide.decorator('myDirectiveTwoDirective', function ($delegate) {
            var directive = $delegate[0];
            var link = directive.link; // this is directive link phase
            directive.compile = function () { // change the compile of that directive
                return function (scope, element, attrs) {
                    link.apply(this, arguments); // apply this at the link phase
                    element.on('click', function(){ // when add an onclick that log hello when the directive is clicked.
                            console.log('hello!');
                    }); 
                };
            };
            return $delegate;
        });

    });

```

Similar approach can be used for both Providers and Services.



## Directive inheritance and interoperability


Angular js directives can be nested or be made interoperable.

In this example, directive Adir exposes to directive Bdir it's controller $scope, since Bdir requires Adir.

```js
angular.module('myApp',[]).directive('Adir', function () {
        return {
            restrict: 'AE',
            controller: ['$scope', function ($scope) {
                    $scope.logFn = function (val) {
                            console.log(val);
                        }
                  }]
            }
    })

```

Make sure to set require: '^Adir' (look at the angular documentation, some versions doesn't require ^ character).

```js
.directive('Bdir', function () {
        return {
            restrict: 'AE',
            require: '^Adir', // Bdir require Adir
            link: function (scope, elem, attr, Parent) { 
            // Parent is Adir but can be an array of required directives.
                elem.on('click', function ($event) {
                    Parent.logFn("Hello!"); // will log "Hello! at parent dir scope
                    scope.$apply(); // apply to parent scope.
                });
            }
        }
    }]);

```

You can nest your directive in this way:

```js
<div a-dir><span b-dir></span></div>
<a-dir><b-dir></b-dir> </a-dir>

```

**Is not required that directives are nested in your HTML.**



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|scope|Property to set the scope of the directive. It can be set as false, true or as an isolate scope: { @, =, <, & }.
|scope: falsy|Directive uses parent scope. No scope created for directive.
|scope: true|Directive inherits parent scope prototypically as a new child scope. If there are multiple directives on the same element requesting a new scope, then they will share one new scope.
|scope: { @ }|One way binding of a directive scope property to a DOM attribute value. As the attribute value bound in the parent, it will change in the directive scope.
|scope: { = }|Bi-directional attribute binding that changes the attribute in the parent if the directive attribute changes and vice-versa.
|scope: { < }|One way binding of a directive scope property and a DOM attribute expression. The expression  is evaluated in the parent. This watches the identity of the parent value so changes to an object property in the parent won't be reflected in the directive. Changes to an object property in a directive will be reflected in the parent, since both reference the same object
|scope: { & }|Allows the directive to pass data to an expression to be evaluated in the parent.
|compile: function|This function is used to perform DOM transformation on the directive template before the link function runs. It accepts `tElement` ( the directive template ) and `tAttr` ( list of attributes declared on the directive ). It does not have access to the scope. It may return a function that will be registered as a `post-link` function or it may return an object with `pre` and `post` properties with will be registered as the `pre-link` and `post-link` functions.
|link: function/object|The link property can be configured as a function or object. It can receive the following arguments: scope(directive scope), iElement( DOM element where directive is applied ), iAttrs( collection of DOM element attributes ), controller( array of controllers required by directive ), transcludeFn. It is mainly used to for setting up DOM listeners, watching model properties for changes, and updating the DOM. It executes after the template is cloned. It is configured independently if there is no compile function.
|pre-link function|Link function that executes before any child link functions. By default, child directive link functions execute before parent directive link functions and the pre-link function enables the parent to link first. One use case is if the child requires data from the parent.
|post-link function|Link function that executives after child elements are linked to parent. It is commonly used for attaching event handlers and accessing child directives, but data required by the child directive should not be set here because the child directive will have already been linked.
|restrict: string|Defines how to call the directive from within the DOM. Possible values (Assuming our directive name is `demoDirective`): `E` -  Element name (`<demo-directive></demo-directive>`), `A` - Attribute (`<div demo-directive></div>`), `C` - Matching class (`<div class="demo-directive"></div>`), `M` - By comment (`<!-- directive: demo-directive -->`). The `restrict` property can also support multiple options, for example - `restrict: "AC"` will restrict the directive to **Attribute** OR **Class**. If omitted, the **default** value is `"EA"` (Element or Attribute).
|require: 'demoDirective'|Locate demoDirective's controller on the current element and inject its controller as the fourth argument to the linking function. Throw an error if not found.
|require: '?demoDirective'|Attempt to locate the demoDirective's controller or pass null to the link fn if not found.
|require: '^demoDirective'|Locate the demoDirective's controller by searching the element and its parents. Throw an error if not found.
|require: '^^demoDirective'|Locate the demoDirective's controller by searching the element's parents. Throw an error if not found.
|require: '?^demoDirective'|Attempt to locate the demoDirective's controller by searching the element and its parents or pass null to the link fn if not found.
|require: '?^^demoDirective'|Attempt to locate the demoDirective's controller by searching the element's parents, or pass null to the link fn if not found.

