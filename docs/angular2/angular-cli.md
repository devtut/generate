---
metaTitle: "Angular 2 - Angular-cli"
description: "Create empty Angular2 application with angular-cli, Generating Components, Directives, Pipes and Services, Adding 3rd party libs, build with angular-cli, New project with scss/sass as stylesheet, Set yarn as default package manager for @angular/cli"
---

# Angular-cli


Here you will find how to start with angular-cli , generating new component/service/pipe/module with angular-cli , add 3 party like bootstrap , build angular project.



## Create empty Angular2 application with angular-cli


Requirements:

- NodeJS : [Download page](https://nodejs.org/en/)
- [npm](https://www.npmjs.com/) or [yarn](https://yarnpkg.com)

Run the following commands with cmd from new directory folder:

1. `npm install -g @angular/cli` or `yarn global add @angular/cli`
1. `ng new PROJECT_NAME`
1. `cd PROJECT_NAME`
1. `ng serve`

Open your browser at localhost:4200



## Generating Components, Directives, Pipes and Services


just use your cmd: You can use the ng generate (or just ng g) command to generate Angular components:

- Component:  `ng g component my-new-component`
- Directive:     `ng g directive my-new-directive`
- Pipe:     `ng g pipe my-new-pipe`
- Service: `ng g service my-new-service`
- Class: `ng g class my-new-classt`
- Interface: `ng g interface my-new-interface`
- Enum: `ng g enum my-new-enum`
- Module: `ng g module my-module`



## Adding 3rd party libs


In angular-cli.json you can change the app configuration.

If you want to add ng2-bootstrap for example:

<li>
`npm install ng2-bootstrap --save` or `yarn add ng2-bootstrap`
</li>
<li>
In angular-cli.json just add the path of the bootstrap at node-modules.

```js
"scripts": [
    "../node_modules/jquery/dist/jquery.js",
    "../node_modules/bootstrap/dist/js/bootstrap.js"
 ]

```


</li>



## build with angular-cli


In angular-cli.json at outDir key you can define your build directory;

these are equivalent

```js
ng build --target=production --environment=prod
ng build --prod --env=prod
ng build --prod

```

and so are these

```js
ng build --target=development --environment=dev
ng build --dev --e=dev
ng build --dev
ng build

```

When building you can modify base tag () in your index.html with --base-href your-url option.

Sets base tag href to /myUrl/ in your index.html

```js
ng build --base-href /myUrl/
ng build --bh /myUrl/

```



## New project with scss/sass as stylesheet


The default style files generated and compiled by `@angular/cli` are **css**.

If you want to use **scss** instead, generate your project with:

```js
ng new project_name --style=scss

```

If you want to use **sass**, generate your project with:

```js
ng new project_name --style=sass

```



## Set yarn as default package manager for @angular/cli


Yarn is an alternative for npm, the default package manager on @angular/cli. If you want to use yarn as package manager for @angular/cli follow this steps:

### Requirements

- [yarn](https://yarnpkg.com) (`npm install --global yarn` or see the [installation page](https://yarnpkg.com/en/docs/install))
- [@angular/cli](https://github.com/angular/angular-cli) (`npm install -g @angular/cli` or `yarn global add @angular/cli`)

To set yarn as @angular/cli package manager:

`ng set --global packageManager=yarn`

To set back npm as @angular/cli package manager:

`ng set --global packageManager=npm`

