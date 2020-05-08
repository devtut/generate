---
metaTitle: "Angular 2 - Components"
description: "A simple component, Templates & Styles, Testing a Component, Nesting components"
---

# Components


Angular components are elements composed by a template that will render your application.



## A simple component


To create a component we add `@Component` decorator in a class passing some parameters:

- `providers`: Resources that will be injected into the component constructor
- `selector`: The query selector that will find the element in the HTML and replace by the component
- `styles`: Inline styles. NOTE: DO NOT use this parameter with require, it works on development but when you build the application in production all your styles are lost
- `styleUrls`: Array of path to style files
- `template`: String that contains your HTML
- `templateUrl`: Path to a HTML file

There are other parameters you can configure, but the listed ones are what you will use the most.

A simple example:

```js
import { Component } from '@angular/core';
 
@Component({
  selector: 'app-required',
  styleUrls: ['required.component.scss'],
  // template: `This field is required.`,
  templateUrl: 'required.component.html',
})
export class RequiredComponent { }

```



## Templates & Styles


Templates are HTML files that may contain logic.

You can specify a template in two ways:

### Passing template as a file path

```js
@Component({
  templateUrl: 'hero.component.html',
})

```

### Passing a template as an inline code

```js
@Component({
  template: `<div>My template here</div>`,
})

```

Templates may contain styles. The styles declared in `@Component` are different from your application style file, anything applied in the component will be restricted to this scope. For example, say you add:

```js
div { background: red; }

```

All `div`s inside the component will be red, but if you have other components, other divs in your HTML they will not be changed at all.

The generated code will look like this:

[<img src="https://i.stack.imgur.com/8nO5H.png" alt="generated code" />](https://i.stack.imgur.com/8nO5H.png)

You can add styles to a component in two ways:

### Passing an array of file paths

```js
@Component({
  styleUrls: ['hero.component.css'],
})

```

### Passing an array of inline codes

```js
styles: [ `div { background: lime; }` ]

```

You shouldn't use `styles` with `require` as it will not work when you build your application to production.



## Testing a Component


hero.component.html

```js
<form (ngSubmit)="submit($event)" [formGroup]="form" novalidate>
  <input type="text" formControlName="name" />
  <button type="submit">Show hero name</button>
</form>

```

hero.component.ts

```js
import { FormControl, FormGroup, Validators } from '@angular/forms';

import { Component } from '@angular/core';

@Component({
  selector: 'app-hero',
  templateUrl: 'hero.component.html',
})
export class HeroComponent {
  public form = new FormGroup({
    name: new FormControl('', Validators.required),
  });

  submit(event) {
    console.log(event);
    console.log(this.form.controls.name.value);
  }
}

```

hero.component.spec.ts

```js
import { ComponentFixture, TestBed, async } from '@angular/core/testing';

import { HeroComponent } from './hero.component';
import { ReactiveFormsModule } from '@angular/forms';

describe('HeroComponent', () => {
  let component: HeroComponent;
  let fixture: ComponentFixture<HeroComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [HeroComponent],
      imports: [ReactiveFormsModule],
    }).compileComponents();

    fixture = TestBed.createComponent(HeroComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  }));

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  it('should log hero name in the console when user submit form', async(() => {
    const heroName = 'Saitama';
    const element = <HTMLFormElement>fixture.debugElement.nativeElement.querySelector('form');

    spyOn(console, 'log').and.callThrough();

    component.form.controls['name'].setValue(heroName);

    element.querySelector('button').click();

    fixture.whenStable().then(() => {
      fixture.detectChanges();
      expect(console.log).toHaveBeenCalledWith(heroName);
    });
  }));

  it('should validate name field as required', () => {
    component.form.controls['name'].setValue('');
    expect(component.form.invalid).toBeTruthy();
  });
});

```



## Nesting components


Components will render in their respective `selector`, so you can use that to nest components.

If you have a component that shows a message:

```js
import { Component, Input } from '@angular/core';

@Component({
  selector: 'app-required',
  template: `{{name}} is required.`
})
export class RequiredComponent {
  @Input()
  public name: String = '';
}

```

You can use it inside another component using `app-required` (this component's selector):

```js
import { Component, Input } from '@angular/core';

@Component({
  selector: 'app-sample',
  template: `
    <input type="text" name="heroName" />
    <app-required name="Hero Name"></app-required>
  `
})
export class RequiredComponent {
  @Input()
  public name: String = '';
}

```

