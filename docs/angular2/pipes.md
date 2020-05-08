---
metaTitle: "Angular 2 - Pipes"
description: "Custom Pipes, Built-in Pipes, Chaining Pipes, Debugging With JsonPipe, Dynamic Pipe, Unwrap async values with async pipe, Stateful Pipes, Creating Custom Pipe, Globally Available Custom Pipe, Extending an Existing Pipe, Testing a pipe"
---

# Pipes


The pipe `|` character is used to apply pipes in Angular 2. Pipes are very similar to filters in AngularJS in that they both help to transform the data into a specified format.



## Custom Pipes


**my.pipe.ts**

```js
import { Pipe, PipeTransform } from '@angular/core';

@Pipe({name: 'myPipe'})
export class MyPipe implements PipeTransform {

  transform(value:any, args?: any):string {
    let transformedValue = value; // implement your transformation logic here
    return transformedValue;
  }

}

```

**my.component.ts**

```js
import { Component } from '@angular/core';

@Component({
  selector: 'my-component',
  template: `{{ value | myPipe }}`
})
export class MyComponent {

    public value:any;

}

```

**my.module.ts**

```js
import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';

import { MyComponent } from './my.component';
import { MyPipe } from './my.pipe';

@NgModule({
  imports: [
    BrowserModule,
  ],
  declarations: [
    MyComponent,
    MyPipe
  ],
})
export class MyModule { }

```



## Built-in Pipes


### Angular2 comes with a few built-in pipes:

|Pipe|Usage|Example
|---|---|---|---|---|---|---|---|---|---
|[`DatePipe`](https://angular.io/docs/ts/latest/api/common/index/DatePipe-pipe.html)|`date`|`{{ dateObj | date }} // output is 'Jun 15, 2015'`
|[`UpperCasePipe`](https://angular.io/docs/ts/latest/api/common/index/UpperCasePipe-pipe.html)|`uppercase`|`{{ value | uppercase }} // output is 'SOMETEXT'`
|[`LowerCasePipe`](https://angular.io/docs/ts/latest/api/common/index/LowerCasePipe-pipe.html)|`lowercase`|`{{ value | lowercase }} // output is 'sometext'`
|[`CurrencyPipe`](https://angular.io/docs/ts/latest/api/common/index/CurrencyPipe-pipe.html)|`currency`|`{{ 31.00 | currency:'USD':true }} // output is '$31'`
|[`PercentPipe`](https://angular.io/docs/ts/latest/api/common/index/PercentPipe-pipe.html)|`percent`|`{{ 0.03 | percent }} //output is %3`

There are others. Look [here](https://angular.io/docs/ts/latest/api/#!?apiFilter=pipe) for their documentation.

### Example

### hotel-reservation.component.ts

```js
import { Component } from '@angular/core';

@Component({
    moduleId: module.id,
    selector: 'hotel-reservation',
    templateUrl: './hotel-reservation.template.html'
})
export class HotelReservationComponent {
    public fName: string =  'Joe';
    public lName: string = 'SCHMO';
    public reservationMade: string = '2016-06-22T07:18-08:00'
    public reservationFor: string = '2025-11-14';
    public cost: number = 99.99;
}

```

### hotel-reservation.template.html

```js
<div>
    <h1>Welcome back {{fName | uppercase}} {{lName | lowercase}}</h1>
    <p>
        On {reservationMade | date} at {reservationMade | date:'shortTime'} you 
        reserved room 205 for {reservationDate | date} for a total cost of 
        {cost | currency}.
    </p>
</div>

```

### Output

```js
Welcome back JOE schmo
On Jun 26, 2016 at 7:18 you reserved room 205 for Nov 14, 2025 for a total cost of 
$99.99.

```



## Chaining Pipes


Pipes may be chained.

```js
<p>Today is {{ today | date:'fullDate' | uppercase}}.</p>

```



## Debugging With JsonPipe


The JsonPipe can be used for debugging the state of any given internal.

### Code

```js
@Component({
  selector: 'json-example',
  template: `<div>
    <p>Without JSON pipe:</p>
    <pre>{{object}}</pre>
    <p>With JSON pipe:</p>
    <pre>{{object | json}}</pre>
  </div>`
})
export class JsonPipeExample {
  object: Object = {foo: 'bar', baz: 'qux', nested: {xyz: 3, numbers: [1, 2, 3, 4, 5]}};
}

```

### Output

```js
Without JSON Pipe:
object
With JSON pipe:
{object:{foo: 'bar', baz: 'qux', nested: {xyz: 3, numbers: [1, 2, 3, 4, 5]}}

```



## Dynamic Pipe


Use case scenario:
A table view consists of different columns with different data format that needs to be transformed with different pipes.

**table.component.ts**

**dynamic.pipe.ts**

**table.component.html**

**Result**



## Unwrap async values with async pipe


```js
import { Component } from '@angular/core';
import { Observable } from 'rxjs/Observable';
import 'rxjs/add/observable/of';

@Component({
  selector: 'async-stuff',
  template: `
    <h1>Hello, {{ name | async }}</h1>
    Your Friends are:
    <ul>
      <li *ngFor="let friend of friends | async">
        {{friend}}
      </li>
    </ul>
  `
})
class AsyncStuffComponent {
  name = Promise.resolve('Misko');
  friends = Observable.of(['Igor']);
}

```

Becomes:

```js
<h1>Hello, Misko</h1>
Your Friends are:
<ul>
  <li>
    Igor
  </li>
</ul>

```



## Stateful Pipes


Angular 2 offers two different types of pipes - stateless and stateful. Pipes are stateless by default. However, we can implement stateful pipes by setting the `pure` property to `false`. As you can see in the parameter section, you can specify a `name` and declare whether the pipe should be pure or not, meaning stateful or stateless. While data flows through a stateless pipe (which is a pure function) that **does not** remember anything, data can be managed and remembered by stateful pipes. A good example of a stateful pipe is the `AsyncPipe` that is provided by Angular 2.

**Important**

Notice that most pipes should fall into the category of stateless pipes. That's important for performance reasons since Angular can optimize stateless pipes for the change detector. So use stateful pipes cautiously. In general, the optimization of pipes in Angular 2 have a major performance enhancement over filters in Angular 1.x. In Angular 1 the digest cycle always had to re-run all filters even though the data hasn't changed at all. In Angular 2, once a pipe's value has been computed, the change detector knows not to run this pipe again unless the input changes.

**Implementation of a stateful pipe**

```js
import {Pipe, PipeTransform, OnDestroy} from '@angular/core';

@Pipe({
  name: 'countdown',
  pure: false
})
export class CountdownPipe implements PipeTransform, OnDestroy  {
  private interval: any;
  private remainingTime: number;

  transform(value: number, interval: number = 1000): number {
    if (!parseInt(value, 10)) {
      return null;
    }
    
    if (typeof this.remainingTime !== 'number') {
      this.remainingTime = parseInt(value, 10);
    }
    
    if (!this.interval) {
      this.interval = setInterval(() => {
        this.remainingTime--;
        
        if (this.remainingTime <= 0) {
          this.remainingTime = 0;
          clearInterval(this.interval);
          delete this.interval;
        }
      }, interval);
    }
    
    return this.remainingTime;
  }
  
  ngOnDestroy(): void {
    if (this.interval) {
      clearInterval(this.interval);
    }
  }
}

```

You can then use the pipe as usual:

```js
{{ 1000 | countdown:50 }}
{{ 300 | countdown }}

```

It's important that your pipe also implements the `OnDestroy` interface so you can clean up once your pipe gets destroyed. In the example above, it's necessary to clear the interval to avoid memory leaks.



## Creating Custom Pipe


app/pipes.pipe.ts

```js
import { Pipe, PipeTransform } from '@angular/core';

@Pipe({name: 'truthy'})
export class Truthy implements PipeTransform {
  transform(value: any, truthy: string, falsey: string): any {
    if (typeof value === 'boolean'){return value ? truthy : falsey;}
    else return value
  }
}

```

app/my-component.component.ts

```js
import { Truthy} from './pipes.pipe';

@Component({
  selector: 'my-component',
  template: `
    <p>{{value | truthy:'enabled':'disabled' }}</p>
  `,
  pipes: [Truthy]
})
export class MyComponent{ }

```



## Globally Available Custom Pipe


To make a custom pipe available application wide, During application bootstrap, extending PLATFORM_PIPES.

```js
import { bootstrap }    from '@angular/platform-browser-dynamic';
import { provide, PLATFORM_PIPES } from '@angular/core';

import { AppComponent } from './app.component';
import { MyPipe } from './my.pipe'; // your custom pipe

bootstrap(AppComponent, [
  provide(PLATFORM_PIPES, {
            useValue: [
                MyPipe 
            ],
            multi: true
        })
]);

```

Tutorial here: [https://scotch.io/tutorials/create-a-globally-available-custom-pipe-in-angular-2](https://scotch.io/tutorials/create-a-globally-available-custom-pipe-in-angular-2)



## Extending an Existing Pipe


```js
import { Pipe, PipeTransform } from '@angular/core';
import { DatePipe } from '@angular/common'


@Pipe({name: 'ifDate'})
export class IfDate implements PipeTransform {
  private datePipe: DatePipe = new DatePipe();

  transform(value: any, pattern?:string) : any {
    if (typeof value === 'number') {return value}
    try {
      return this.datePipe.transform(value, pattern)
    } catch(err) {
      return value
    }
  }
}

```



## Testing a pipe


Given a pipe that reverse a string

```js
import { Pipe, PipeTransform } from '@angular/core';

@Pipe({ name: 'reverse' })
export class ReversePipe implements PipeTransform {
  transform(value: string): string {
    return value.split('').reverse().join('');
  }
}

```

It can be tested configuring the spec file like this

```js
import { TestBed, inject } from '@angular/core/testing';

import { ReversePipe } from './reverse.pipe';

describe('ReversePipe', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [ReversePipe],
    });
  });

  it('should be created', inject([ReversePipe], (reversePipe: ReversePipe) => {
    expect(reversePipe).toBeTruthy();
  }));

  it('should reverse a string', inject([ReversePipe], (reversePipe: ReversePipe) => {
    expect(reversePipe.transform('abc')).toEqual('cba');
  }));
});

```



#### Parameters


|Function/Parameter|Explanation
|---|---|---|---|---|---|---|---|---|---
|**@Pipe({name, pure})**|metadata for pipe, must immediately precede pipe class
|name: **string**|what you will use inside the template
|pure: **boolean**|defaults to true, mark this as false to have your pipe re-evaluated more often
|**transform( value, args[]? )**|the function that is called to transform the values in the template
|value: **any**|the value that you want to transform
|args: **any[]**|the arguments that you may need included in your transform. Mark optional args with the ? operator like so transform(value, arg1, arg2?)



#### Remarks


This topic covers [Angular2 Pipes](https://angular.io/docs/ts/latest/guide/pipes.html), a mechanism for transforming and formatting data within HTML templates in an Angular2 application.

