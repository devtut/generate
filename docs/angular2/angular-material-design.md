---
metaTitle: "Angular 2 - Angular material design"
description: "Md2Accordion and Md2Collapse, Md2Select, Md2Toast, Md2Datepicker, Md2Tooltip"
---

# Angular material design




## Md2Accordion and Md2Collapse


**Md2Collapse** : Collapse is a directive, it's allow the user to toggle visiblity of the section.

**Examples**

> 
A collapse would have the following markup.


```js
<div [collapse]="isCollapsed">
  Lorum Ipsum Content
</div>

```

**Md2Accordion** : Accordion it's allow the user to toggle visiblity of the multiple sections.

**Examples**

> 
A accordion would have the following markup.


```js
<md2-accordion [multiple]="multiple">
  <md2-accordion-tab *ngFor="let tab of accordions" 
                     [header]="tab.title" 
                     [active]="tab.active" 
                     [disabled]="tab.disabled">
    {{tab.content}}
  </md2-accordion-tab>
  <md2-accordion-tab>
    <md2-accordion-header>Custom Header</md2-accordion-header>
    test content
  </md2-accordion-tab>
</md2-accordion>

```



## Md2Select


**Component**:

```js
<md2-select [(ngModel)]="item" (change)="change($event)" [disabled]="disabled">
<md2-option *ngFor="let i of items" [value]="i.value" [disabled]="i.disabled">
{{i.name}}</md2-option>
</md2-select>

```

> 
Select allow the user to select option from options.


```js
<md2-select></md2-select>
<md2-option></md2-option>
<md2-select-header></md2-select-header>

```



## Md2Toast


Toast is a service, which show notifications in the view.

> 
Creates and show a simple toast noticiation.


```js
import {Md2Toast} from 'md2/toast/toast';

@Component({
 selector: "..."
})

export class ... {

...
constructor(private toast: Md2Toast) { }
toastMe() {
this.toast.show('Toast message...');

---  or  ---

this.toast.show('Toast message...', 1000);
}

...

}

```



## Md2Datepicker


Datepicker allow the user to select date and time.

```js
<md2-datepicker [(ngModel)]="date"></md2-datepicker>

```

see for more details [here](https://github.com/Promact/md2/tree/master/src/lib/datepicker)



## Md2Tooltip


Tooltip is a directive, it allows the user to show hint text while the user mouse hover over an element.

> 
A tooltip would have the following markup.


```js
<span tooltip-direction="left" tooltip="On the Left!">Left</span>
<button tooltip="some message"
    tooltip-position="below"
    tooltip-delay="1000">Hover Me
</button>

```

