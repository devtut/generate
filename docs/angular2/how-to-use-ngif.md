---
metaTitle: "Angular 2 - How to Use ngif"
description: "To run a function at the start or end of *ngFor loop Using *ngIf, Display a loading message, Show Alert Message on a condition, Use *ngIf with*ngFor"
---

# How to Use ngif


***NgIf**: It removes or recreates a part of DOM tree depending on an expression evaluation.  It is a structural directive and structural directives alter the layout of the DOM by adding, replacing and removing its elements.



## To run a function at the start or end of *ngFor loop Using *ngIf


NgFor  provides Some values that can be aliased to local variables

- **index** -(variable) position of the current item in the iterable starting at 0
- **first** -(boolean) true if the current item is the first item in the iterable
- **last** -(boolean) true if the current item is the last item in the iterable
- **even** -(boolean) true if the current index is an even number
- **odd** -(boolean) true if the current index is an odd number

```js
<div *ngFor="let note of csvdata; let i=index; let lastcall=last">
      <h3>{{i}}</h3> <-- to show index position
      <h3>{{note}}</h3>
      <span *ngIf="lastcall">{{anyfunction()}} </span><-- this lastcall boolean value will be true only if this is last in loop
      // anyfunction() will run at the end of loop same way we can do at start
    </div>

```



## Display a loading message


If our component is not ready and waiting for data from server, then we can add loader using *ngIf.
**Steps:**

First declare a boolean:

```js
loading: boolean = false;

```

Next, in your component add a lifecycle hook called `ngOnInit`

```js
ngOnInit() {
   this.loading = true;
}

```

and after you get complete data from server set you loading boolean to false.

```

this.loading=false; 

```

In your html template use *ngIf with the `loading` property:

```js
<div *ngIf="loading" class="progress">
   <div class="progress-bar info" style="width: 125%;"></div>
</div>

```



## Show Alert Message on a condition


```js
<p class="alert alert-success" *ngIf="names.length > 2">Currently there are more than 2 names!</p>

```



## Use *ngIf with*ngFor


While you are not allowed to use `*ngIf` and `*ngFor` in the same div (it will gives an error in the runtime) you can nest the `*ngIf` in the `*ngFor` to get the desired behavior.

Example 1: General syntax

```js
<div *ngFor="let item of items; let i = index">
  <div *ngIf="<your condition here>">

   <!-- Execute code here if statement true -->

  </div>
</div>

```

Example 2: Display elements with even index

```js
<div *ngFor="let item of items; let i = index">
  <div *ngIf="i % 2 == 0">
    {{ item }}
  </div>
</div>

```

The downside is that an additional outer `div` element needs to be added.

**But consider this use case** where a `div` element needs to be iterated (using *ngFor) and also includes a check whether the element need to be removed or not (using *ngIf), but adding an additional `div` is not preferred. In this case you can use the `template` tag for the *ngFor:

```js
<template ngFor let-item [ngForOf]="items">
    <div *ngIf="item.price > 100">
    </div>
</template>

```

This way adding an additional outer `div` is not needed and furthermore the `<template>` element won't be added to the DOM. The only elements added in the DOM from the above example are the iterated `div` elements.

Note: In Angular v4 `<template>` has been deprecated in favour of `<ng-template>` and will be removed in v5. In Angular v2.x releases `<template>` is still valid.



#### Syntax


- <div *ngIf="false"> test </div> <!-- evaluates to false -->
- <div *ngIf="undefined"> test </div> <!-- evaluates to false -->
- <div *ngIf="null"> test </div> <!-- evaluates to false -->
- <div *ngIf="0"> test </div> <!-- evaluates to false -->
- <div *ngIf="NaN"> test </div> <!-- evaluates to false -->
- <div *ngIf=""> test </div> <!-- evaluates to false -->
- All other values evaluate to true.

