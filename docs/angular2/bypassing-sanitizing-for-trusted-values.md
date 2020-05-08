---
metaTitle: "Angular 2 - Bypassing Sanitizing for trusted values"
description: "Bypassing Sanitizing with pipes (for code re-use)"
---

# Bypassing Sanitizing for trusted values



## Bypassing Sanitizing with pipes (for code re-use)


Project is following the structure from the Angular2 Quickstart guide [here](https://angular.io/docs/ts/latest/quickstart.html).

```js
RootOfProject
|
+-- app
|   |-- app.component.ts
|   |-- main.ts
|   |-- pipeUser.component.ts
|   \-- sanitize.pipe.ts
|
|-- index.html
|-- main.html
|-- pipe.html

```

> 
main.ts


```js
import { bootstrap } from '@angular/platform-browser-dynamic';
import { AppComponent } from './app.component';

bootstrap(AppComponent);

```

This finds the index.html file in the root of the project, and builds off of that.

> 
app.component.ts


```js
import { Component } from '@angular/core';
import { PipeUserComponent } from './pipeUser.component';

@Component({
    selector: 'main-app',
    templateUrl: 'main.html',
    directives: [PipeUserComponent]
})

export class AppComponent { }

```

This is the top level component that groups other components that are used.

> 
pipeUser.component.ts


```js
import { Component } from '@angular/core';
import { IgnoreSanitize } from "./sanitize.pipe";

@Component({
    selector: 'pipe-example',
    templateUrl: "pipe.html",
    pipes: [IgnoreSanitize]
})

export class PipeUserComponent{
    constructor () { }        
    unsafeValue: string = "unsafe/picUrl?id=";
    docNum: string;

    getUrl(input: string): any {
        if(input !== undefined) {
            return this.unsafeValue.concat(input);
            // returns : "unsafe/picUrl?id=input"
        } else {
            return "fallback/to/something";
        }
    }
}

```

This component provides the view for the Pipe to work with.

> 
sanitize.pipe.ts


```js
import { Pipe, PipeTransform } from '@angular/core';
import { DomSanitizationService } from '@angular/platform-browser';

@Pipe({
    name: 'sanitaryPipe'
})
export class IgnoreSanitize implements PipeTransform {

   constructor(private sanitizer: DomSanitizationService){}

   transform(input: string) : any {
       return this.sanitizer.bypassSecurityTrustUrl(input);
   }

}

```

This is the logic that describes what the pipe formats.

> 
index.html


```js
<head>
    Stuff goes here...
</head>
<body>
    <main-app> 
        main.html will load inside here.
    </main-app>
</body>

```

> 
main.html


```js
<othertags> 
</othertags>

<pipe-example>  
    pipe.html will load inside here.
</pipe-example>

<moretags>
</moretags>

```

> 
pipe.html


```js
<img [src]="getUrl('1234') | sanitaryPipe">
<embed [src]="getUrl() | sanitaryPipe">

```

If you were to inspect the html while the app is running you would see that it looks like this:

```js
<head>
    Stuff goes here...
</head>

<body>

    <othertags> 
    </othertags>
    
    <img [src]="getUrl('1234') | sanitaryPipe">
    <embed [src]="getUrl() | sanitaryPipe">
    
    <moretags>
    </moretags>

</body>

```



#### Parameters


|Params|Details
|---|---|---|---|---|---|---|---|---|---
|selector|tag name you reference your component by in the html
|template(templateUrl)|a string that represents html which will be inserted wherever the `<selector>` tag is. templateUrl is a path to an html file with the same behavior
|pipes|an array of pipes that are used by this component.



#### Remarks


### SUPER IMPORTANT!

### DISABLING SANITIZING LEAVES YOU AT RISK OF XSS (Cross-Site Scripting) AND OTHER ATTACK VECTORS. PLEASE MAKE SURE YOU TRUST WHAT YOU'RE GETTING 100%

Using Pipes relegates you to only changing attribute values like so :

```js
<tag [attribute]="expression or variable reference | pipeName">

```

you are not able to use pipes this way :

```js
<tag attribute="expression or variable reference | pipeName">

```

or this way

```js
<tag attribute={{expression or variable reference | pipeName}}

```

