---
metaTitle: "Angular 2 - Lifecycle Hooks"
description: "OnChanges, OnInit, OnDestroy, AfterContentInit, AfterContentChecked, AfterViewInit, AfterViewChecked, DoCheck"
---

# Lifecycle Hooks




## OnChanges


Fired when one or more of the component or directive properties have been changed.

```js
import { Component, OnChanges, Input } from '@angular/core';

@Component({
    selector: 'so-onchanges-component',
    templateUrl: 'onchanges-component.html',
    styleUrls: ['onchanges-component.']
})
class OnChangesComponent implements OnChanges {
    @Input() name: string;
    message: string;
    
    ngOnChanges(changes: SimpleChanges): void {
        console.log(changes);
    }
}

```

On change event will log

```js
name: {
    currentValue: 'new name value',
    previousValue: 'old name value'
},
message: {
    currentValue: 'new message value',
    previousValue: 'old message value'
}

```



## OnInit


Fired when component or directive properties have been initialized.

(Before those of the child directives)

```js
import { Component, OnInit } from '@angular/core';

@Component({
    selector: 'so-oninit-component',
    templateUrl: 'oninit-component.html',
    styleUrls: ['oninit-component.']
})
class OnInitComponent implements OnInit {

    ngOnInit(): void {
        console.log('Component is ready !');
    }
}

```



## OnDestroy


Fired when the component or directive instance is destroyed.

```js
import { Component, OnDestroy } from '@angular/core';

@Component({
    selector: 'so-ondestroy-component',
    templateUrl: 'ondestroy-component.html',
    styleUrls: ['ondestroy-component.']
})
class OnDestroyComponent implements OnDestroy {

    ngOnDestroy(): void {
        console.log('Component was destroyed !');
    }
}

```



## AfterContentInit


Fire after the initialization of the content of the
component or directive has finished.

(Right after OnInit)

```js
import { Component, AfterContentInit } from '@angular/core';

@Component({
    selector: 'so-aftercontentinit-component',
    templateUrl: 'aftercontentinit-component.html',
    styleUrls: ['aftercontentinit-component.']
})
class AfterContentInitComponent implements AfterContentInit {

    ngAfterContentInit(): void {
        console.log('Component content have been loaded!');
    }
}

```



## AfterContentChecked


Fire after the view has been fully initialized.

**(Only available for components)**

```js
import { Component, AfterContentChecked } from '@angular/core';

@Component({
    selector: 'so-aftercontentchecked-component',
    templateUrl: 'aftercontentchecked-component.html',
    styleUrls: ['aftercontentchecked-component.']
})
class AfterContentCheckedComponent implements AfterContentChecked {

    ngAfterContentChecked(): void {
        console.log('Component content have been checked!');
    }
}

```



## AfterViewInit


Fires after initializing both the component view and any of its child views. This is a useful lifecycle hook for plugins outside of the Angular 2 ecosystem. For example, you could use this method to initialize a jQuery date picker based on the markup that Angular 2 has rendered.

```js
import { Component, AfterViewInit } from '@angular/core';

@Component({
    selector: 'so-afterviewinit-component',
    templateUrl: 'afterviewinit-component.html',
    styleUrls: ['afterviewinit-component.']
})
class AfterViewInitComponent implements AfterViewInit {

    ngAfterViewInit(): void {
        console.log('This event fire after the content init have been loaded!');
    }
}

```



## AfterViewChecked


Fire after the check of the view, of the component, has finished.

**(Only available for components)**

```js
import { Component, AfterViewChecked } from '@angular/core';

@Component({
    selector: 'so-afterviewchecked-component',
    templateUrl: 'afterviewchecked-component.html',
    styleUrls: ['afterviewchecked-component.']
})
class AfterViewCheckedComponent implements AfterViewChecked {

    ngAfterViewChecked(): void {
        console.log('This event fire after the content have been checked!');
    }
}

```



## DoCheck


Allows to listen for changes only on specified properties

```js
import { Component, DoCheck, Input } from '@angular/core';

@Component({
    selector: 'so-docheck-component',
    templateUrl: 'docheck-component.html',
    styleUrls: ['docheck-component.']
})
class DoCheckComponent implements DoCheck {
    @Input() elements: string[];
    differ: any;
    ngDoCheck(): void {
        // get value for elements property
        const changes = this.differ.diff(this.elements);
        
        if (changes) {
            changes.forEachAddedItem(res => console.log('Added', r.item));
            changes.forEachRemovedItem(r => console.log('Removed', r.item));
        }
    }
}

```



#### Remarks


### Events availability

`AfterViewInit` and `AfterViewChecked` are only available **in Components**, and **not in Directives**.

### Events order

- `OnChanges` (multiple times)
- `OnInit` (once)
- `DoCheck` (multiple times)
- `AfterContentInit` (once)
- `AfterContentChecked` (multiple times)
- `AfterViewInit` (once) (Component only)
- `AfterViewChecked` (multiple times) (Component only)
- `OnDestroy` (once)

### Further Reading

- [Angular Documentation - Lifecycle Hooks](https://angular.io/docs/ts/latest/guide/lifecycle-hooks.html)

