---
metaTitle: "Angular 2 - Optimizing rendering using ChangeDetectionStrategy"
description: "Default vs OnPush"
---

# Optimizing rendering using ChangeDetectionStrategy



## Default vs OnPush


Consider the following component with one input `myInput` and an internal value called `someInternalValue`. Both of them are used in a component's template.

```js
import {Component, Input} from '@angular/core';

@Component({
  template:`
  <div>
    <p>{{myInput}}</p>
    <p>{{someInternalValue}}</p>
  </div>
  `
})
class MyComponent {
  @Input() myInput: any;

  someInternalValue: any;

  // ...
}

```

By default, the `changeDetection:` property in the component decorator will be set to `ChangeDetectionStrategy.Default`; implicit in the example. In this situation, any changes to any of the values in the template will trigger a re-render of `MyComponent`. In other words, if I change `myInput` or `someInternalValue` angular 2 will exert energy and re-render the component.

Suppose, however, that we only want to re-render when the inputs change. Consider the following component with `changeDetection:` set to `ChangeDetectionStrategy.OnPush`

```js
import {Component, ChangeDetectionStrategy, Input} from '@angular/core';

@Component({
  changeDetection: ChangeDetectionStrategy.OnPush
  template:`
  <div>
    <p>{{myInput}}</p>
    <p>{{someInternalValue}}</p>
  </div>
  `
})
class MyComponent {
  @Input() myInput: any;

  someInternalValue: any;

  // ...
}

```

By setting `changeDetection:` to `ChangeDetectionStrategy.OnPush`, `MyComponent` will only re-render when its inputs change. In this case, `myInput` will need to receive a new value from its parent to trigger a re-render.

