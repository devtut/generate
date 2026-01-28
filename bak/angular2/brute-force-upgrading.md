---
metaTitle: "Angular 2 - Brute Force Upgrading"
description: "Scaffolding a New Angular CLI Project"
---

# Brute Force Upgrading


If you want to upgrade the Angular CLI version of your project you may run into tough-to-fix errors and bugs from simply changing the Angular CLI version number in your project. Also, because the Angular CLI hides a lot of what's going on in the build and bundles process, you can't really do much when things go wrong there.

**Sometimes the easiest way to update the Angular CLI version of the project is to just scaffold out a new proejct with the Angular CLI version that you wish to use.**



## Scaffolding a New Angular CLI Project


```js
ng new NewProject

```

or

```js
ng init NewProject

```



#### Remarks


Because Angular 2 is **so** modular and encapsulated you can pretty much just copy over all of your components, services, pipes, directives and then fill out the NgModule as it was in the old project.

