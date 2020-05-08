---
metaTitle: "Angular 2 - Angular - ForLoop"
description: "NgFor - Markup For Loop, *ngFor with component, Angular 2 for-loop, *ngFor X amount of items per row, *ngFor in the Table Rows"
---

# Angular - ForLoop



## NgFor - Markup For Loop


The **NgFor** directive instantiates a template once per item from an iterable. The context for each instantiated template inherits from the outer context with the given loop variable set to the current item from the iterable.

To customize the default tracking algorithm, NgFor supports **trackBy** option. **trackBy** takes a function which has two arguments: index and item. If **trackBy** is given, Angular tracks changes by the return value of the function.

```js
<li *ngFor="let item of items; let i = index; trackBy: trackByFn">
    {{i}} - {{item.name}}
</li>

```

**Additional Options**:
NgFor provides several exported values that can be aliased to local variables:

- **index** will be set to the current loop iteration for each template context.
- **first** will be set to a boolean value indicating whether the item is the first one in the iteration.
- **last** will be set to a boolean value indicating whether the item is the last one in the iteration.
- **even** will be set to a boolean value indicating whether this item has an even index.
- **odd** will be set to a boolean value indicating whether this item has an odd index.



## *ngFor with component


```

  @Component({
     selector: 'main-component',
     template: '<example-component    
                   *ngFor="let hero of heroes"
                   [hero]="hero"></example-component>'
   })


   @Component({
      selector: 'example-component',
      template: '<div>{{hero?.name}}</div>'
   })

   export class ExampleComponent {
     @Input() hero : Hero = null;
   }

```



## Angular 2 for-loop


For live [plnkr click...](http://embed.plnkr.co/5btVMZ/preview)

```js
<!doctype html>
<html>
<head>
    <title>ng for loop in angular 2 with ES5.</title>
    <script type="text/javascript" src="https://code.angularjs.org/2.0.0-alpha.28/angular2.sfx.dev.js"></script>
    <script>
        var ngForLoop = function () {
            this.msg = "ng for loop in angular 2 with ES5.";
            this.users = ["Anil Singh", "Sunil Singh", "Sushil Singh", "Aradhya", 'Reena'];
        };

        ngForLoop.annotations = [
                new angular.Component({
                    selector: 'ngforloop'
                }),
                new angular.View({
                    template: '<H1>{{msg}}</H1>' +
                            '<p> User List : </p>' +
                            '<ul>' +
                            '<li *ng-for="let user of users">' +
                            '{{user}}' +
                            '</li>' +
                            '</ul>',
                    directives: [angular.NgFor]
                })
        ];

        document.addEventListener("DOMContentLoaded", function () {
            angular.bootstrap(ngForLoop);
        });
    </script>
</head>
<body>
    <ngforloop></ngforloop>
    <h2>
      <a href="http://www.code-sample.com/" target="_blank">For more detail...</a>
    </h2>
</body>
</html>

```



## *ngFor X amount of items per row


Example shows 5 items per row:

```js
<div *ngFor="let item of items; let i = index">
  <div *ngIf="i % 5 == 0" class="row">
    {{ item }}
    <div *ngIf="i + 1 < items.length">{{ items[i + 1] }}</div>
    <div *ngIf="i + 2 < items.length">{{ items[i + 2] }}</div>
    <div *ngIf="i + 3 < items.length">{{ items[i + 3] }}</div>
    <div *ngIf="i + 4 < items.length">{{ items[i + 4] }}</div>
  </div>
</div>

```



## *ngFor in the Table Rows


```js
<table>
    <thead>
        <th>Name</th>
        <th>Index</th>
    </thead>
    <tbody>
        <tr *ngFor="let hero of heroes">
            <td>{{hero.name}}</td>
        </tr>
    </tbody>
</table>

```



#### Syntax


1. < div *ngFor="let item of items; let i = index">{{i}} {{item}}</ div>



#### Remarks


The `*ngFor` structural directive runs as a loop in a collection and repeats a piece of html for each element of a collection.

`@View` decorator is now deprecated. Developers should be using `template` or 'templateUrl' properties for `@Component` decorator.

