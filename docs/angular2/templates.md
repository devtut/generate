---
metaTitle: "Angular 2 - Templates"
description: "Angular 2 Templates"
---

# Templates


Templates are very similar to templates in Angular 1, though there are many small syntactical changes that make it more clear what is happening.



## Angular 2 Templates


**A SIMPLE TEMPLATE**

Let’s start with a very simple template that shows our name and our favorite thing:

```js
<div>
  Hello my name is {{name}} and I like {{thing}} quite a lot.
</div>

```

`{}:` RENDERING

To render a value, we can use the standard double-curly syntax:

```js
My name is {{name}}

```

Pipes, previously known as “Filters,” transform a value into a new value, like localizing a string or converting a floating point value into a currency representation:

`[]:` BINDING PROPERTIES

To resolve and bind a variable to a component, use the [] syntax. If we have this.currentVolume in our component, we will pass this through to our component and the values will stay in sync:

```js
<video-control [volume]="currentVolume"></video-control>
(): HANDLING EVENTS

```

`():` HANDLING EVENTS
To listen for an event on a component, we use the () syntax

```js
<my-component (click)="onClick($event)"></my-component>

```

`[()]:` TWO-WAY DATA BINDING

To keep a binding up to date given user input and other events, use the [()] syntax. Think of it as a combination of handling an event and binding a property:

<input [(ngModel)]="myName">
The this.myName value of your component will stay in sync with the input value.

`*`: THE ASTERISK

Indicates that this directive treats this component as a template and will not draw it as-is. For example, ngFor takes our  and stamps it out for each item in items, but it never renders our initial  since it’s a template:

```js
<my-component *ngFor="#item of items">
</my-component>

```

Other similar directives that work on templates rather than rendered components are *ngIf and *ngSwitch.

