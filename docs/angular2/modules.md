---
metaTitle: "Angular 2 - Modules"
description: "A simple module, Nesting modules"
---

# Modules


Angular modules are containers for different parts of your app.

You can have nested modules, your `app.module` is already actually nesting other modules such as `BrowserModule` and you can add `RouterModule` and so on.



## A simple module


A module is a class with the `@NgModule` decorator. To create a module we add `@NgModule` passing some parameters:

- `bootstrap`: The component that will be the root of your application. This configuration is only present on your root module
- `declarations`: Resources the module declares. When you add a new component you have to update the declarations (`ng generate component` does it automatically)
- `exports`: Resources the module exports that can be used in other modules
- `imports`: Resources the module uses from other modules (only module classes are accepted)
- `providers`: Resources that can be injected (di) in a component

A simple example:

```js
import { AppComponent } from './app.component';
import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
 
@NgModule({
  bootstrap: [AppComponent]
  declarations: [AppComponent],
  exports: [],
  imports: [BrowserModule],
  providers: [],
})
export class AppModule { }

```



## Nesting modules


Modules can be nested by using the `imports` parameter of `@NgModule` decorator.

We can create a `core.module` in our application that will contain generic things, like a `ReservePipe` (a pipe that reverse a string) and bundle those in this module:

```js
import { CommonModule } from '@angular/common';
import { NgModule } from '@angular/core';
import { ReversePipe } from '../reverse.pipe';

@NgModule({
  imports: [
    CommonModule
  ],
  exports: [ReversePipe], // export things to be imported in another module
  declarations: [ReversePipe],
})
export class CoreModule { }

```

Then in the `app.module`:

```js
import { CoreModule } from 'app/core/core.module';

@NgModule({
  declarations: [...], // ReversePipe is available without declaring here
                       // because CoreModule exports it
  imports: [
    CoreModule,        // import things from CoreModule
    ...
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }

```

