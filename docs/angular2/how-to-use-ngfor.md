---
metaTitle: "Angular 2 - How to use ngfor"
description: "*ngFor with pipe, Unordered list example, More complext template example, Tracking current interaction example, Angular2 aliased exported values"
---

# How to use ngfor




## *ngFor with pipe


```js
import { Pipe, PipeTransform } from '@angular/core';
@Pipe({
  name: 'even'
})

export class EvenPipe implements PipeTransform {
    transform(value: string): string {
        if(value && value %2 === 0){
          return value;
        }
    }
}

@Component({
      selector: 'example-component',
      template: '<div>
                      <div *ngFor="let number of numbers | even">
                          {{number}}
                      </div>
                </div>'
})

export class exampleComponent {
    let numbers : List<number> = Array.apply(null, {length: 10}).map(Number.call, Number)
}

```



## Unordered list example


```js
<ul>
  <li *ngFor="let item of items">{{item.name}}</li>
</ul>

```



## More complext template example


```js
<div *ngFor="let item of items">
  <p>{{item.name}}</p>
  <p>{{item.price}}</p>
  <p>{{item.description}}</p>
</div>

```



## Tracking current interaction example


```js
<div *ngFor="let item of items; let i = index">
  <p>Item number: {{i}}</p>    
</div>

```

In this case, i will take the value of index, which is the current loop iteration.



## Angular2 aliased exported values


Angular2 provides several exported values that can be aliased to local variables. These are:

- index
- first
- last
- even
- odd

Except `index`, the other ones take a `Boolean` value. As the previous example using index, it can be used any of these exported values:

```js
<div *ngFor="let item of items; let firstItem = first; let lastItem = last">
  <p *ngIf="firstItem">I am the first item and I am gonna be showed</p>
  <p *ngIf="firstItem">I am not the first item and I will not show up :(</p>
  <p *ngIf="lastItem">But I'm gonna be showed as I am the last item :)</p>
</div>

```

