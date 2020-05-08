---
metaTitle: "Angular 2 - Example for routes such as /route/subroute for static urls"
description: "Basic route example with sub routes tree"
---

# Example for routes such as /route/subroute for static urls




## Basic route example with sub routes tree


app.module.ts

```

import {routes} from "./app.routes";

 @NgModule({
    declarations: [AppComponent],
    imports: [BrowserModule, mainModule.forRoot(), RouterModule.forRoot(routes)],
    providers: [],
    bootstrap: [AppComponent]
 })

 export class AppModule { } 

```

app.routes.ts

```js
import { Routes } from '@angular/router';
import {SubTreeRoutes} from "./subTree/subTreeRoutes.routes";

export const routes: Routes = [
  ...SubTreeRoutes,
  { path: '',  redirectTo: 'home', pathMatch: 'full'}
];

```

subTreeRoutes.ts

```js
import {Route} from '@angular/router';
import {exampleComponent} from "./example.component";

export const SubTreeRoutes: Route[] = [
  {
    path: 'subTree',
    children: [
      {path: '',component: exampleComponent}
    ]
  }
];

```

