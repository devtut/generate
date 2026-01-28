---
metaTitle: "Angular 2 - Detecting resize events"
description: "A component listening in on the window resize event."
---

# Detecting resize events



## A component listening in on the window resize event.


Suppose we have a component which will hide at a certain window width.

```js
import { Component } from '@angular/core';

@Component({
  ...
  template: `
    <div>
      <p [hidden]="!visible" (window:resize)="onResize($event)" >Now you see me...</p>
      <p>now you dont!</p>
    </div>
  `
  ...
})
export class MyComponent {
  visible: boolean = false;
  breakpoint: number = 768;

  constructor() {
  }
  
  onResize(event) {
    const w = event.target.innerWidth;
    if (w >= this.breakpoint) {
      this.visible = true;
    } else {
      // whenever the window is less than 768, hide this component.
      this.visible = false;
    }
  }
}

```

A `p` tag in our template will hide whenever `visible` is false. `visible` will change value whenever the `onResize` event handler is invoked. Its call occurs every time `window:resize` fires an event.

