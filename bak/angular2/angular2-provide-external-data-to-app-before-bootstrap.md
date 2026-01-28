---
metaTitle: "Angular 2 - Angular2 provide external data to App before bootstrap"
description: "Via Dependency Injection"
---

# Angular2 provide external data to App before bootstrap


In this post I will demonstrate how to pass external data to Angular app before the app bootstraps. This external data could be configuration data, legacy data, server rendered etc.



## Via Dependency Injection


Instead of invoking the Angular’s bootstrap code directly, wrap the bootstrap code into a function and export the function. This function can also accept parameters.

```js
import { platformBrowserDynamic } from "@angular/platform-browser-dynamic";
import { AppModule } from "./src/app";
export function runAngular2App(legacyModel: any) {
       platformBrowserDynamic([
           { provide: "legacyModel", useValue: model }
       ]).bootstrapModule(AppModule)
       .then(success => console.log("Ng2 Bootstrap success"))
       .catch(err => console.error(err));
}

```

Then, in any services or components we can inject the “legacy model” and gain access to it.

```js
import { Injectable } from "@angular/core";
@Injectable()
export class MyService {
   constructor(@Inject("legacyModel") private legacyModel) {
      console.log("Legacy data — ", legacyModel);
    }
 }

```

Require the app and then run it.

```js
require(["myAngular2App"], function(app) {
   app.runAngular2App(legacyModel); // Input to your APP
});

```

