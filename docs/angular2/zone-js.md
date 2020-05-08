---
metaTitle: "Angular 2 - Zone.js"
description: "Getting reference to NgZone, Using NgZone to do multiple HTTP requests before showing the data"
---

# Zone.js



## Getting reference to NgZone


`NgZone` reference can be injected via the Dependency Injection (DI).

**my.component.ts**

```js
import { Component, NgOnInit, NgZone } from '@angular/core';
  
@Component({...})
export class Mycomponent implements NgOnInit {
  constructor(private _ngZone: NgZone) { }
  ngOnInit() {
    this._ngZone.runOutsideAngular(() => {
      // Do something outside Angular so it won't get noticed
    });
  }
}

```



## Using NgZone to do multiple HTTP requests before showing the data


`runOutsideAngular` can be used to run code outside Angular 2 so that it does not trigger change detection unnecessarily. This can be used to for example run multiple HTTP request to get all the data before rendering it. To execute code again inside Angular 2, `run` method of `NgZone` can be used.

**my.component.ts**

```js
import { Component, OnInit, NgZone } from '@angular/core';
import { Http } from '@angular/http';
  
@Component({...})
export class Mycomponent implements OnInit {
  private data: any[];
  constructor(private http: Http, private _ngZone: NgZone) { }
  ngOnInit() {
    this._ngZone.runOutsideAngular(() => {
      this.http.get('resource1').subscribe((data1:any) => {
        // First response came back, so its data can be used in consecutive request
        this.http.get(`resource2?id=${data1['id']}`).subscribe((data2:any) => {
          this.http.get(`resource3?id1=${data1['id']}&id2=${data2}`).subscribe((data3:any) => {
            this._ngZone.run(() => {
              this.data = [data1, data2, data3];
            });
          });
        });
      });
    });
  }
}

```

