---
metaTitle: "Angular 2 - Directives"
description: "*ngFor, Attribute directive, Component is a directive with template, Structural directives, Custom directive, Copy to Clipboard directive, Testing a custom directive"
---

# Directives



## *ngFor


form1.component.ts:

```js
import { Component } from '@angular/core';

// Defines example component and associated template
@Component({
    selector: 'example',
    template: `
      <div *ngFor="let f of fruit"> {{f}} </div>
      <select required>
        <option *ngFor="let f of fruit" [value]="f"> {{f}} </option>
      </select>
    `
})

// Create a class for all functions, objects, and variables
export class ExampleComponent { 
    // Array of fruit to be iterated by *ngFor
    fruit = ['Apples', 'Oranges', 'Bananas', 'Limes', 'Lemons'];
}

```

**Output:**

```js
<div>Apples</div>
<div>Oranges</div>
<div>Bananas</div>
<div>Limes</div>
<div>Lemons</div>
<select required>
  <option value="Apples">Apples</option>
  <option value="Oranges">Oranges</option>
  <option value="Bananas">Bananas</option>
  <option value="Limes">Limes</option>
  <option value="Lemons">Lemons</option>
</select>

```

In its most simple form, `*ngFor` has two parts : `let **variableName** of **object/array**`

In the case of `fruit = ['Apples', 'Oranges', 'Bananas', 'Limes', 'Lemons'];`,

Apples, Oranges, and so on are the values inside the array `fruit`.

`[value]="f"` will be equal to each current `fruit` (`f`) that `*ngFor` has iterated over.

Unlike AngularJS, Angular2 has not continued with the use of `ng-options` for `<select>` and `ng-repeat` for all other general repetitions.

`*ngFor` is very similar to `ng-repeat` with slightly varied syntax.

References:

Angular2 | [Displaying Data](https://angular.io/docs/ts/latest/guide/displaying-data.html)

Angular2 | [ngFor](https://angular.io/docs/ts/latest/guide/template-syntax.html#!#ngFor)

Angular2 | [Forms](https://angular.io/docs/ts/latest/guide/forms.html)



## Attribute directive


```js
<div [class.active]="isActive"></div>

<span [style.color]="'red'"></span>

<p [attr.data-note]="'This is value for data-note attribute'">A lot of text here</p>

```



## Component is a directive with template


```js
import { Component } from '@angular/core';
@Component({
  selector: 'my-app',
  template: `
    <h1>Angular 2 App</h1>
    <p>Component is directive with template</p>
  `
})
export class AppComponent {
}

```



## Structural directives


```js
<div *ngFor="let item of items">{{ item.description }}</div>

<span *ngIf="isVisible"></span>

```



## Custom directive


```js
import {Directive, ElementRef, Renderer} from '@angular/core';

@Directive({
  selector: '[green]',
})

class GreenDirective {
  constructor(private _elementRef: ElementRef, 
              private _renderer: Renderer) {
    _renderer.setElementStyle(_elementRef.nativeElement, 'color', 'green');
  }
}

```

Usage:

```js
<p green>A lot of green text here</p>

```



## Copy to Clipboard directive


In this example we are going to create a directive to copy a text into the clipboard by clicking on an element

**copy-text.directive.ts**

**some-page.component.html**

Remember to inject TEXT_COPY_DIRECTIVES into the directives array of your component



## Testing a custom directive


Given a directive that highlights text on mouse events

```js
import { Directive, ElementRef, HostListener, Input } from '@angular/core';

@Directive({ selector: '[appHighlight]' })
export class HighlightDirective {
  @Input('appHighlight') // tslint:disable-line no-input-rename
  highlightColor: string;

  constructor(private el: ElementRef) { }

  @HostListener('mouseenter')
  onMouseEnter() {
    this.highlight(this.highlightColor || 'red');
  }

  @HostListener('mouseleave')
  onMouseLeave() {
    this.highlight(null);
  }

  private highlight(color: string) {
    this.el.nativeElement.style.backgroundColor = color;
  }
}

```

It can be tested like this

```js
import { ComponentFixture, ComponentFixtureAutoDetect, TestBed } from '@angular/core/testing';

import { Component } from '@angular/core';
import { HighlightDirective } from './highlight.directive';

@Component({
  selector: 'app-test-container',
  template: `
    <div>
      <span id="red" appHighlight>red text</span>
      <span id="green" [appHighlight]="'green'">green text</span>
      <span id="no">no color</span>
    </div>
  `
})
class ContainerComponent { }

const mouseEvents = {
  get enter() {
    const mouseenter = document.createEvent('MouseEvent');
    mouseenter.initEvent('mouseenter', true, true);
    return mouseenter;
  },
  get leave() {
    const mouseleave = document.createEvent('MouseEvent');
    mouseleave.initEvent('mouseleave', true, true);
    return mouseleave;
  },
};

describe('HighlightDirective', () => {
  let fixture: ComponentFixture<ContainerComponent>;
  let container: ContainerComponent;
  let element: HTMLElement;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [ContainerComponent, HighlightDirective],
      providers: [
        { provide: ComponentFixtureAutoDetect, useValue: true },
      ],
    });

    fixture = TestBed.createComponent(ContainerComponent);
    // fixture.detectChanges(); // without the provider
    container = fixture.componentInstance;
    element = fixture.nativeElement;
  });

  it('should set background-color to empty when mouse leaves with directive without arguments', () => {
    const targetElement = <HTMLSpanElement>element.querySelector('#red');

    targetElement.dispatchEvent(mouseEvents.leave);
    expect(targetElement.style.backgroundColor).toEqual('');
  });

  it('should set background-color to empty when mouse leaves with directive with arguments', () => {
    const targetElement = <HTMLSpanElement>element.querySelector('#green');

    targetElement.dispatchEvent(mouseEvents.leave);
    expect(targetElement.style.backgroundColor).toEqual('');
  });

  it('should set background-color red with no args passed', () => {
    const targetElement = <HTMLSpanElement>element.querySelector('#red');

    targetElement.dispatchEvent(mouseEvents.enter);
    expect(targetElement.style.backgroundColor).toEqual('red');
  });

  it('should set background-color green when passing green parameter', () => {
    const targetElement = <HTMLSpanElement>element.querySelector('#green');

    targetElement.dispatchEvent(mouseEvents.enter);
    expect(targetElement.style.backgroundColor).toEqual('green');
  });
});

```



#### Syntax


<li>
`<input [value]="value">` - Binds attribute value class member `name`.
</li>
<li>
`<div [attr.data-note]="note">` - Binds attribute `data-note` to variable `note`.
</li>
<li>
`<p green></p>` - Custom directive
</li>



#### Remarks


The main source of information about Angular 2 directives is the official documentation [https://angular.io/docs/ts/latest/guide/attribute-directives.html](https://angular.io/docs/ts/latest/guide/attribute-directives.html)

