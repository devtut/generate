---
metaTitle: "Angular 2 - Angular2 CanActivate"
description: "Angular2 CanActivate"
---

# Angular2 CanActivate




## Angular2 CanActivate


**Implemented in a router:**

```js
export const MainRoutes: Route[] = [{
   path: '',
   children: [ {
      path: 'main',
      component: MainComponent ,
      canActivate : [CanActivateRoute]
   }]
}];

```

**The `canActivateRoute` file:**

```js
@Injectable()
  export class  CanActivateRoute implements CanActivate{
  constructor(){}
  canActivate(next: ActivatedRouteSnapshot, state: RouterStateSnapshot): boolean {
     return true;
  }
}

```

