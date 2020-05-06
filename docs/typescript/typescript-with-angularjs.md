---
metaTitle: "TypeScript - TypeScript with AngularJS"
description: "Directive, Simple example, Component"
---

# TypeScript with AngularJS



## Directive




## Simple example


```js
export function myDirective($location: ng.ILocationService): ng.IDirective {
    return {

        link: (scope: ng.IScope,
            element: ng.IAugmentedJQuery,
            attributes: ng.IAttributes): void => {

            element.text("Current URL: " + $location.url());

        },

        replace: true,
        require: "ngModel",
        restrict: "A",
        templateUrl: templatesUrl.myDirective,
    };
}

// Using slug naming across the projects simplifies change of the directive name
myDirective.prototype.slug = "myDirective";

// You can place this in some bootstrap file, or have them at the same file
angular.module("myApp").
    directive(myDirective.prototype.slug, [
        Templates.prototype.slug,
        myDirective
    ]);

```



## Component


For an easier transition to Angular 2, it's recommended to use `Component`, available since Angular 1.5.8

**myModule.ts**

```js
import { MyModuleComponent } from "./components/myModuleComponent";
import { MyModuleService } from "./services/MyModuleService";

angular
    .module("myModule", [])
    .component("myModuleComponent", new MyModuleComponent())
    .service("myModuleService", MyModuleService);

```

**components/myModuleComponent.ts**

```js
import IComponentOptions = angular.IComponentOptions;
import IControllerConstructor = angular.IControllerConstructor;
import Injectable = angular.Injectable;
import { MyModuleController } from "../controller/MyModuleController";

export class MyModuleComponent implements IComponentOptions {
    public templateUrl: string = "./app/myModule/templates/myComponentTemplate.html";
    public controller: Injectable<IControllerConstructor> = MyModuleController;
    public bindings: {[boundProperty: string]: string} = {};
}

```

**templates/myModuleComponent.html**

```js
<div class="my-module-component">
    {{$ctrl.someContent}}
</div>

```

**controller/MyModuleController.ts**

```js
import IController = angular.IController;
import { MyModuleService } from "../services/MyModuleService";

export class MyModuleController implements IController {
    public static readonly $inject: string[] = ["$element", "myModuleService"];
    public someContent: string = "Hello World";

    constructor($element: JQuery, private myModuleService: MyModuleService) {
        console.log("element", $element);
    }

    public doSomething(): void {
        // implementation..
    }
}

```

**services/MyModuleService.ts**

```js
export class MyModuleService {
    public static readonly $inject: string[] = [];

    constructor() {
    }

    public doSomething(): void {
        // do something
    }
}

```

**somewhere.html**

```js
<my-module-component></my-module-component>

```



#### Parameters


|Name|Description
|---|---|---|---|---|---|---|---|---|---
|`controllerAs`|is an alias name, to which variables or functions can be assigned to. @see: [https://docs.angularjs.org/guide/directive](https://docs.angularjs.org/guide/directive)
|`$inject`|Dependency Injection list, it is resolved by angular and passing as an argument to constuctor functions.



#### Remarks


While doing the directive in TypeScript, keep in mind, that power of this language of custom type and interfaces that you can create. This is extremely helpfull when developing huge applications. Code completion supported by many IDE will show you the possible value by corresponding type you are working with, so there is far more less that should be kept in mind (comparing to VanillaJS).

"Code Against Interfaces, Not Implementations"

