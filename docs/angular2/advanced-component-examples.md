---
metaTitle: "Angular 2 - Advanced Component Examples"
description: "Image Picker with Preview, Filter out table values by the input"
---

# Advanced Component Examples




## Image Picker with Preview


In this example, we are going to create an image picker that previews your picture before uploading. The previewer also supports drag and dropping files into the input. In this example, I am only going to cover uploading single files, but you can tinker a bit to get multi file upload working.

**image-preview.html**

This is the html layout of our image preview

**image-preview.ts**

This is the main file for our `<image-preview>` component

**another.component.html**

And that's it. Way more easier than it was in AngularJS 1.x. I actually made this component based on an older version I made in AngularJS 1.5.5.



## Filter out table values by the input


Import `ReactiveFormsModule`, and then

```js
import { Component, OnInit, OnDestroy } from '@angular/core';
import { FormControl } from '@angular/forms';
import { Subscription } from 'rxjs';

@Component({
  selector: 'component',
  template: `
    <input [formControl]="control" />
    <div *ngFor="let item of content">
      {{item.id}} - {{item.name}}
    </div>
  `
})
export class MyComponent implements OnInit, OnDestroy {

  public control = new FormControl('');

  public content: { id: number; name: string; }[];
  
  private originalContent = [
    { id: 1, name: 'abc' },
    { id: 2, name: 'abce' },
    { id: 3, name: 'ced' }
  ];
  
  private subscription: Subscription;
  
  public ngOnInit() {
    this.subscription = this.control.valueChanges.subscribe(value => {
      this.content = this.originalContent.filter(item => item.name.startsWith(value));
    });
  }
  
  public ngOnDestroy() {
    this.subscription.unsubscribe();
  }
  
}

```



#### Remarks


Remember that Angular 2 is all about singular responsibility. No matter how small your component is, dedicate a separate logic for each and every component. Be it a button, a fancy anchor link, a dialog header or even a sidenav's sub item.

