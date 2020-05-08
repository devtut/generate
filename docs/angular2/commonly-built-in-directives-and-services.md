---
metaTitle: "Angular 2 - Commonly built-in directives and services"
description: "Location Class, AsyncPipe, Displaying current angular2 version used in your project, Currency Pipe"
---

# Commonly built-in directives and services




## Location Class


**Location** is a service that applications can use to interact with a browser's URL. Depending on which LocationStrategy is used, Location will either persist to the URL's path or the URL's hash segment.

Location is responsible for normalizing the URL against the application's base href.

```js
import {Component} from '@angular/core';
import {Location} from '@angular/common';

@Component({
    selector: 'app-component'
})
class AppCmp {

  constructor(_location: Location) {

    //Changes the browsers URL to the normalized version of the given URL, 
    //and pushes a new item onto the platform's history.
    _location.go('/foo');

  }

  backClicked() {
    //Navigates back in the platform's history.
    this._location.back();
  }

  forwardClicked() {
    //Navigates forward in the platform's history.
    this._location.back();
  }
}

```



## AsyncPipe


The async pipe subscribes to an Observable or Promise and returns the latest value it has emitted. When a new value is emitted, the async pipe marks the component to be checked for changes. When the component gets destroyed, the async pipe unsubscribes automatically to avoid potential memory leaks.

```js
@Component({
  selector: 'async-observable-pipe',
  template: '<div><code>observable|async</code>: Time: {{ time | async }}</div>'
})
export class AsyncObservablePipeComponent {
  time = new Observable<string>((observer: Subscriber<string>) => {
    setInterval(() => observer.next(new Date().toString()), 1000);
  });
}

```



## Displaying current angular2 version used in your project


To display current version, we can use **VERSION** from @angular/core package.

```js
import { Component, VERSION } from '@angular/core';

@Component({
  selector: 'my-app',
  template: `<h1>Hello {{name}}</h1>
  <h2>Current Version: {{ver}}</h2>
  `,
})
export class AppComponent  {
   name = 'Angular2'; 
   ver = VERSION.full;
}

```



## Currency Pipe


The currency pipe allows you to work with you data as regular numbers but display it with standard currency formatting (currency symbol, decimal places, etc.) in the view.

```js
@Component({
  selector: 'currency-pipe',
  template: `<div>
    <p>A: {{myMoney | currency:'USD':false}}</p>
    <p>B: {{yourMoney | currency:'USD':true:'4.2-2'}}</p>
  </div>`
})
export class CurrencyPipeComponent {
  myMoney: number = 100000.653;
  yourMoney: number = 5.3495;
}

```

The pipe takes three optional parameters:

- **currencyCode**: Allows you to specify the ISO 4217 currency code.
- **symbolDisplay**: Boolean indicating whether to use the currency symbol
- **digitInfo**: Allows you to specify how the decimal places should be displayed.

More documentation on the currency pipe: [https://angular.io/docs/ts/latest/api/common/index/CurrencyPipe-pipe.html](https://angular.io/docs/ts/latest/api/common/index/CurrencyPipe-pipe.html)

