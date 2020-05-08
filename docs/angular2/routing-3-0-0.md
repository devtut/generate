---
metaTitle: "Angular 2 - Routing (3.0.0+)"
description: "Controlling Access to or from a Route, Add guard to route configuration, Using Resolvers and Guards, Use Guard in app bootstrap, Bootstrapping, Configuring router-outlet, Changing routes (using templates & directives), Setting the Routes"
---

# Routing (3.0.0+)



## Controlling Access to or from a Route


The default Angular router allows navigation to and from any route unconditionally. This is not always the desired behavior.

In a scenario where a user may conditionally be allowed to navigate to or from a route, a **Route Guard** may be used to restrict this behavior.

If your scenario fits one of the following, consider using a Route Guard,

- User is required to be authenticated to navigate to the target component.
- User is required to be authorized to navigate to the target component.
- Component requires asynchronous request before initialization.
- Component requires user input before navigated away from.

### How Route Guards work

Route Guards work by returning a boolean value to control the behavior of router navigation. If **true** is returned, the router will continue with navigation to the target component. If **false** is returned, the router will deny navigation to the target component.

### Route Guard Interfaces

The router supports multiple guard interfaces:

- **CanActivate**: occurs between route navigation.
- **CanActivateChild**: occurs between route navigation to a child route.
- **CanDeactivate**: occurs when navigating away from the current route.
- **CanLoad**: occurs between route navigation to a feature module loaded asynchronously.
- **Resolve**: used to perform data retrieval before route activation.

These interfaces can be implemented in your guard to grant or remove access to certain processes of the navigation.

### Synchronous vs. Asynchronous Route Guards

Route Guards allow synchronous and asynchronous operations to conditionally control navigation.
<br><br>

### Synchronous Route Guard

A synchronous route guard returns a boolean, such as by computing an immediate result, in order to conditionally control navigation.

```js
import { Injectable }     from '@angular/core';
import { CanActivate }    from '@angular/router';

@Injectable()
export class SynchronousGuard implements CanActivate {
  canActivate() {
    console.log('SynchronousGuard#canActivate called');
    return true;
  }
}

```

### Asynchronous Route Guard

For more complex behavior, a route guard can asynchronously block navigation. An asynchronous route guard can return an Observable or Promise.

This is useful for situations like waiting for user input to answer a question, waiting to successfully save changes to the server, or waiting to receive data fetched from a remote server.

```js
import { Injectable }     from '@angular/core';
import { CanActivate, Router, ActivatedRouteSnapshot, RouterStateSnapshot } from '@angular/router';
import { Observable }     from 'rxjs/Rx';
import { MockAuthenticationService } from './authentication/authentication.service';

@Injectable()
export class AsynchronousGuard implements CanActivate {
    constructor(private router: Router, private auth: MockAuthenticationService) {}

    canActivate(route:ActivatedRouteSnapshot, state:RouterStateSnapshot):Observable<boolean>|boolean {
        this.auth.subscribe((authenticated) => {
            if (authenticated) {
                return true;
            }
            this.router.navigateByUrl('/login');
            return false;
        });
    }
}

```



## Add guard to route configuration


File **app.routes**

Protected routes have `canActivate` binded to `Guard`

```js
import { provideRouter, Router, RouterConfig, CanActivate } from '@angular/router';

//components
import { LoginComponent } from './login/login.component';
import { DashboardComponent } from './dashboard/dashboard.component';

export const routes: RouterConfig = [
    { path: 'login', component: LoginComponent },
    { path: 'dashboard', component: DashboardComponent, canActivate: [AuthGuard] }    
}

```

Export the **APP_ROUTER_PROVIDERS** to be used in app bootstrap

```js
export const APP_ROUTER_PROVIDERS = [
    AuthGuard,
    provideRouter(routes)
];

```



## Using Resolvers and Guards


We're using a toplevel guard in our route config to catch the current user on first page load, and a resolver to store the value of the `currentUser`, which is our authenticated user from the backend.

A simplified version of our implementation looks as follows:

Here is our top level route:

```js
export const routes = [
{
  path: 'Dash',
  pathMatch : 'prefix',
  component: DashCmp,
  canActivate: [AuthGuard],
  resolve: {
      currentUser: CurrentUserResolver
  },
  children: [...[
    path: '',
    component: ProfileCmp,
    resolve: {
        currentUser: currentUser
    }
  ]]
  }
];

```

Here is our `AuthService`

```js
import { Injectable } from '@angular/core';
import { Http, Headers, RequestOptions } from '@angular/http';
import { Observable } from 'rxjs/Rx';
import 'rxjs/add/operator/do';

@Injectable()
export class AuthService {
  constructor(http: Http) {
    this.http = http;

    let headers = new Headers({ 'Content-Type': 'application/json' });
    this.options = new RequestOptions({ headers: headers });
  }
  fetchCurrentUser() {
    return this.http.get('/api/users/me')
      .map(res => res.json())
      .do(val => this.currentUser = val);
 }
}

```

Here is our `AuthGuard`:

```js
import { Injectable } from '@angular/core';
import { CanActivate } from "@angular/router";
import { Observable } from 'rxjs/Rx';

import { AuthService } from '../services/AuthService';

@Injectable()
export class AuthGuard implements CanActivate {
  constructor(auth: AuthService) {
    this.auth = auth;
  }
  canActivate(route, state) {
    return Observable
      .merge(this.auth.fetchCurrentUser(), Observable.of(true))
      .filter(x => x == true);
  }
}

```

Here is our `CurrentUserResolver`:

```js
import { Injectable } from '@angular/core';
import { Resolve } from "@angular/router";
import { Observable } from 'rxjs/Rx';

import { AuthService } from '../services/AuthService';

@Injectable()
export class CurrentUserResolver implements Resolve {
  constructor(auth: AuthService) {
    this.auth = auth;
  }
  resolve(route, state) {
    return this.auth.currentUser;
  }
}

```



## Use Guard in app bootstrap


File **main.ts** (or **boot.ts**)

Consider the examples above:

1. **Create the guard** (where the Guard is created) and
<li>**Add guard to route configuration**, (where the Guard is configured for route, then **APP_ROUTER_PROVIDERS** is exported),<br />
we can couple the bootstrap to Guard as follows</li>

```js
import { bootstrap } from '@angular/platform-browser-dynamic';
import { provide } from '@angular/core';

import { APP_ROUTER_PROVIDERS } from './app.routes';    
import { AppComponent } from './app.component';

bootstrap(AppComponent, [
    APP_ROUTER_PROVIDERS
])
.then(success => console.log(`Bootstrap success`))
.catch(error => console.log(error));

```



## Bootstrapping


Now that the routes are defined, we need to let our application know about the routes.
To do this, bootstrap the provider we exported in the previous example.

Find your bootstrap configuration (should be in `main.ts`, but **your mileage may vary**).

```js
//main.ts

import {bootstrap} from '@angular/platform-browser-dynamic';

//Import the App component (root component)
import { App } from './app/app';

//Also import the app routes
import { APP_ROUTES_PROVIDER } from './app/app.routes';

bootstrap(App, [
  APP_ROUTES_PROVIDER,
])
.catch(err => console.error(err));

```



## Configuring router-outlet


Now that the router is configured and our app knows how to handle the routes, we need to show the actual components that we configured.

To do so, configure your HTML template/file for your **top-level (app)** component like so:

```js
//app.ts

import {Component} from '@angular/core';
import {Router, ROUTER_DIRECTIVES} from '@angular/router';

@Component({
    selector: 'app',
    templateUrl: 'app.html',
    styleUrls: ['app.css'],
    directives: [
        ROUTER_DIRECTIVES,
    ]
})
export class App {
    constructor() {
    }
}

<!-- app.html -->

<!-- All of your 'views' will go here -->
<router-outlet></router-outlet>

```

The `<router-outlet></router-outlet>` element will switch the content given the route. Another good aspect about this element is that it **does not** have to be the only element in your HTML.

For example: Lets say you wanted a a toolbar on every page that stays constant between routes, similar to how Stack Overflow looks. You can nest the `<router-outlet>` under elements so that only certain parts of the page change.



## Changing routes (using templates & directives)


Now that the routes are set up, we need some way to actually change routes.

This example will show how to change routes using the template, but it is possible to change routes in TypeScript.

Here is one example (without binding):

```js
<a routerLink="/home">Home</a>

```

If the user clicks on that link, it will route to `/home`. The router knows how to handle `/home`, so it will display the `Home` Component.

Here is an example with data binding:

```js
<a *ngFor="let link of links" [routerLink]="link">{{link}}</a>

```

Which would require an array called `links` to exist, so add this to `app.ts`:

```js
public links[] = [
    'home',
    'login'
]

```

This will loop through the array and add an `<a>` element with the `routerLink` directive = the value of the current element in the array, creating this:

```

<a routerLink="home">home</a>
 <a routerLink="login">login</a>

```

This is particularly helpful if you have a lot of links, or maybe the links need to be constantly changed. We let Angular handle the busy work of adding links by just feeding it the info it requires.

Right now, `links[]` is static, but it is possible to feed it data from another source.



## Setting the Routes


**NOTE: This example is based on the 3.0.0.-beta.2 release of the @angular/router. At the time of writing, this is the latest version of the router.**

To use the router, define routes in a new TypeScript file like such

```js
//app.routes.ts

import {provideRouter} from '@angular/router';

import {Home} from './routes/home/home';
import {Profile} from './routes/profile/profile';

export const routes = [
    {path: '', redirectTo: 'home'},
    {path: 'home', component: Home},
    {path: 'login', component: Login},
];

export const APP_ROUTES_PROVIDER = provideRouter(routes);

```

In the first line, we import `provideRouter` so we can let our application know what the routes are during the bootstrap phase.

`Home` and `Profile` are just two components as an example. You will need to import each `Component` you need as a route.

Then, export the array of routes.

`path`: The path to the component. **YOU DO NOT NEED TO USE '/........'** Angular will do this automatically

`component`: The component to load when the route is accessed

`redirectTo`: **Optional**. If you need to automatically redirect a user when they access a particular route, supply this.

Finally, we export the configured router. `provideRouter` will return a provider that we can boostrap so our application knows how to handle each route.



#### Remarks


There are a few more tricks we can do with the router (such as restricting access), but those can be covered in a separate tutorial.

If you need a new route, simply modify `app.routes.ts` and follow the following steps:

1. Import the Component
1. Add to the `routes` array. Make sure to include a new `path` and `component`.

