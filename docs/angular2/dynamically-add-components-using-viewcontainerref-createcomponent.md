---
metaTitle: "Angular 2 - Dynamically add components using ViewContainerRef.createComponent"
description: "A wrapper component that adds dynamic components declaratively, Dynamically add component on specific event(click), Rendered dynamically created component array on template html in Angular2"
---

# Dynamically add components using ViewContainerRef.createComponent




## A wrapper component that adds dynamic components declaratively


A custom component that takes the type of a component as input and creates an instance of that component type inside itself. When the input is updated, the previously added dynamic component is removed and the new one added instead.

```js
@Component({
  selector: 'dcl-wrapper',
  template: `<div #target></div>`
})
export class DclWrapper {
  @ViewChild('target', {
    read: ViewContainerRef
  }) target;
  @Input() type;
  cmpRef: ComponentRef;
  private isViewInitialized: boolean = false;

  constructor(private resolver: ComponentResolver) {}

  updateComponent() {
    if (!this.isViewInitialized) {
      return;
    }
    if (this.cmpRef) {
      this.cmpRef.destroy();
    }
    this.resolver.resolveComponent(this.type).then((factory: ComponentFactory < any > ) => {
      this.cmpRef = this.target.createComponent(factory)
        // to access the created instance use
        // this.cmpRef.instance.someProperty = 'someValue';
        // this.cmpRef.instance.someOutput.subscribe(val => doSomething());
    });
  }

  ngOnChanges() {
    this.updateComponent();
  }

  ngAfterViewInit() {
    this.isViewInitialized = true;
    this.updateComponent();
  }

  ngOnDestroy() {
    if (this.cmpRef) {
      this.cmpRef.destroy();
    }
  }
}

```

This allows you to create dynamic components like

```js
<dcl-wrapper [type]="someComponentType"></dcl-wrapper>

```

[**Plunker example**](http://plnkr.co/edit/GJTLrnQdRDBvZenX59PZ?p=preview)



## Dynamically add component on specific event(click)


**Main Component File:**

```js
//our root app component
import {Component, NgModule, ViewChild, ViewContainerRef, ComponentFactoryResolver, ComponentRef} from '@angular/core'
import {BrowserModule} from '@angular/platform-browser'
import {ChildComponent} from './childComp.ts'

@Component({
  selector: 'my-app',
  template: `
    <div>
      <h2>Hello {{name}}</h2>
      <input type="button" value="Click me to add element" (click) = addElement()> // call the function on click of the button
      <div #parent> </div> // Dynamic component will be loaded here
    </div>
  `,
})
export class App {
  name:string;
  
  @ViewChild('parent', {read: ViewContainerRef}) target: ViewContainerRef;
  private componentRef: ComponentRef<any>;

  constructor(private componentFactoryResolver: ComponentFactoryResolver) {
    this.name = 'Angular2'
  }
  
  addElement(){
    let childComponent = this.componentFactoryResolver.resolveComponentFactory(ChildComponent);
    this.componentRef = this.target.createComponent(childComponent);
  }
}

```

**childComp.ts :**

```js
import{Component} from '@angular/core';

@Component({
  selector: 'child',
  template: `
    <p>This is Child</p>
  `,
})
export class ChildComponent {
  constructor(){
    
  }
}

```

**app.module.ts :**

```js
@NgModule({
  imports: [ BrowserModule ],
  declarations: [ App, ChildComponent ],
  bootstrap: [ App ],
  entryComponents: [ChildComponent] // define the dynamic component here in module.ts
})
export class AppModule {}

```

[**Plunker example**](https://plnkr.co/edit/ZihiORrHb7JPFMcCQ5b0?p=preview)



## Rendered dynamically created component array on template html in Angular2


We can create dynamic component and get the instances of component into an array and finally rendered it on template.

For example, we can can consider two widget component, ChartWidget and PatientWidget which extended the class WidgetComponent that I wanted to add in the container.

ChartWidget.ts

```js
@Component({
selector: 'chart-widget',
templateUrl: 'chart-widget.component.html',
providers: [{provide: WidgetComponent, useExisting: forwardRef(() => ChartWidget) }]
})

export class ChartWidget extends WidgetComponent implements OnInit {
       constructor(ngEl: ElementRef, renderer: Renderer) {
    super(ngEl, renderer);
    }
    ngOnInit() {}
     close(){
      console.log('close');
    }
    refresh(){
      console.log('refresh');
    }
    ...
}

```

chart-widget.compoment.html (using primeng Panel)

```js
<p-panel [style]="{'margin-bottom':'20px'}">
    <p-header>
        <div class="ui-helper-clearfix">
           <span class="ui-panel-title" style="font-size:14px;display:inline-block;margin-top:2px">Chart Widget</span>
            <div class="ui-toolbar-group-right">                
               <button pButton type="button" icon="fa-window-minimize" (click)="minimize()"</button>
              <button pButton type="button" icon="fa-refresh" (click)="refresh()"></button>
              <button pButton type="button"  icon="fa-expand" (click)="expand()" ></button>
             <button pButton type="button" (click)="close()" icon="fa-window-close"></button>
                    </div>
                </div>
    </p-header>
      some data
</p-panel>

```

DataWidget.ts

```js
@Component({
    selector: 'data-widget',
    templateUrl: 'data-widget.component.html',
    providers: [{provide: WidgetComponent, useExisting: forwardRef(() =>DataWidget) }]
    })

export class DataWidget extends WidgetComponent implements OnInit {
       constructor(ngEl: ElementRef, renderer: Renderer) {
    super(ngEl, renderer);
    }
    ngOnInit() {}
    close(){
      console.log('close');
    }
    refresh(){
      console.log('refresh');
    }
    ...
}

```

**data-widget.compoment.html (same as chart-widget using primeng Panel)**

WidgetComponent.ts

```js
@Component({
  selector: 'widget',
  template: '<ng-content></ng-content>'
})
export  class WidgetComponent{
}

```

we can creat dynamic component instances by selecting the pre-existing components. For example,

```js
@Component({

    selector: 'dynamic-component',
    template: `<div #container><ng-content></ng-content></div>`

})
export class DynamicComponent {
@ViewChild('container', {read: ViewContainerRef}) container: ViewContainerRef; 

    public addComponent(ngItem: Type<WidgetComponent>): WidgetComponent {
    let factory = this.compFactoryResolver.resolveComponentFactory(ngItem);
    const ref = this.container.createComponent(factory);
    const newItem: WidgetComponent = ref.instance;              
    this._elements.push(newItem);                 
    return newItem;
  }
}

```

Finally we use it in app component.
app.component.ts

```js
@Component({
  selector: 'app-root',
  templateUrl: './app/app.component.html',
  styleUrls: ['./app/app.component.css'],
  entryComponents: [ChartWidget,  DataWidget], 
})

export class AppComponent {
   private elements: Array<WidgetComponent>=[];
   private WidgetClasses = {
      'ChartWidget': ChartWidget,
      'DataWidget': DataWidget        
  }
  @ViewChild(DynamicComponent) dynamicComponent:DynamicComponent;  
  
   addComponent(widget: string ): void{                         
     let ref= this.dynamicComponent.addComponent(this.WidgetClasses[widget]);    
     this.elements.push(ref); 
     console.log(this.elements);
  
     this.dynamicComponent.resetContainer();                     
  }
}

```

app.component.html

```js
<button (click)="addComponent('ChartWidget')">Add ChartWidget</button>
<button (click)="addComponent('DataWidget')">Add DataWidget</button>

<dynamic-component [hidden]="true" ></dynamic-component>  

<hr>
Dynamic Components
<hr>
<widget *ngFor="let item of elements">
    <div>{{item}}</div>
   <div [innerHTML]="item._ngEl.nativeElement.innerHTML | sanitizeHtml">
   </div>
</widget>

```

[https://plnkr.co/edit/lugU2pPsSBd3XhPHiUP1?p=preview](https://plnkr.co/edit/lugU2pPsSBd3XhPHiUP1?p=preview)

Some modification by @yurzui to use mouse event on the widgets

view.directive.ts

import { ViewRef, Directive, Input, ViewContainerRef } from '@angular/core';

```js
@Directive({
    selector: '[view]'
})
export class ViewDirective {
  constructor(private vcRef: ViewContainerRef) {}

  @Input()
  set view(view: ViewRef) {
    this.vcRef.clear();
    this.vcRef.insert(view);
  }

  ngOnDestroy() {
    this.vcRef.clear()
  }
}

```

app.component.ts

```js
private elements: Array<{ view: ViewRef, component: WidgetComponent}> = [];

...
addComponent(widget: string ): void{
  let component = this.dynamicComponent.addComponent(this.WidgetClasses[widget]);
  let view: ViewRef = this.dynamicComponent.container.detach(0);
  this.elements.push({view,component});

  this.dynamicComponent.resetContainer();
}

```

app.component.html

```js
<widget *ngFor="let item of elements">
  <ng-container *view="item.view"></ng-container>
</widget>

```

[https://plnkr.co/edit/JHpIHR43SvJd0OxJVMfV?p=preview](https://plnkr.co/edit/JHpIHR43SvJd0OxJVMfV?p=preview)

