---
metaTitle: "Angular 2 - ngrx"
description: "Complete example : Login/logout a user"
---

# ngrx




## Complete example : Login/logout a user


**Prerequisites**

This topic is **not** about Redux and/or Ngrx :

- You need to be comfortable with Redux
- At least understand the basics of RxJs and Observable pattern

First, let's define an example from the very beginning and play with some code :

As a developer, I want to :

1. Have an `IUser` interface that defines the properties of a `User`
1. Declare the actions that we'll use later to manipulate the `User` in the `Store`
1. Define the initial state of the `UserReducer`
1. Create the reducer `UserReducer`
1. Import our `UserReducer` into our main module to build the `Store`
1. Use data from the `Store` to display information in our view

**Spoiler alert** : If you want to try the demo right away or read the code before we even get started, here's a Plunkr ([embed view](http://embed.plnkr.co/gS41jX/) or [run view](http://run.plnkr.co/ci8I1LviF0GCyugW/)).

### 1) Define `IUser` interface

I like to split my interfaces in two parts :

- The properties we'll get from a server
- The properties we define only for the UI (should a button be spinning for example)

And here's the interface `IUser` we'll be using :

`user.interface.ts`

```js
export interface IUser {
  // from server
  username: string;
  email: string;
  
  // for UI
  isConnecting: boolean;
  isConnected: boolean;
};

```

### 2) Declare the actions to manipulate the `User`

Now we've got to think about what kind of actions our ****reducers**** are supposed to handle.<br />
Let say here :

`user.actions.ts`

```js
export const UserActions = {
  // when the user clicks on login button, before we launch the HTTP request
  // this will allow us to disable the login button during the request
  USR_IS_CONNECTING: 'USR_IS_CONNECTING',
  // this allows us to save the username and email of the user
  // we assume those data were fetched in the previous request
  USR_IS_CONNECTED: 'USR_IS_CONNECTED',

  // same pattern for disconnecting the user
  USR_IS_DISCONNECTING: 'USR_IS_DISCONNECTING',
  USR_IS_DISCONNECTED: 'USR_IS_DISCONNECTED'
};

```

But before we use those actions, let me explain why we're going to need a service to dispatch **some** of those actions for us :

Let say that we want to connect a user. So we'll be clicking on a login button and here's what's going to happen :

- Click on the button
- The component catch the event and call `userService.login`
- `userService.login` method `dispatch` an event to update our store property : `user.isConnecting`
- An HTTP call is fired (we'll use a `setTimeout` in the demo to simulate the **async behaviour**)
- Once the `HTTP` call is finished, we'll dispatch another action to warn our store that a user is logged

`user.service.ts`

```js
@Injectable()
export class UserService {
  constructor(public store$: Store<AppState>) { }

  login(username: string) {
    // first, dispatch an action saying that the user's tyring to connect
    // so we can lock the button until the HTTP request finish
    this.store$.dispatch({ type: UserActions.USR_IS_CONNECTING });

    // simulate some delay like we would have with an HTTP request
    // by using a timeout
    setTimeout(() => {
      // some email (or data) that you'd have get as HTTP response
      let email = `${username}@email.com`;

      this.store$.dispatch({ type: UserActions.USR_IS_CONNECTED, payload: { username, email } });
    }, 2000);
  }

  logout() {
    // first, dispatch an action saying that the user's tyring to connect
    // so we can lock the button until the HTTP request finish
    this.store$.dispatch({ type: UserActions.USR_IS_DISCONNECTING });

    // simulate some delay like we would have with an HTTP request
    // by using a timeout
    setTimeout(() => {
      this.store$.dispatch({ type: UserActions.USR_IS_DISCONNECTED });
    }, 2000);
  }
}

```

### 3) Define the initial state of the `UserReducer`

`user.state.ts`

```js
export const UserFactory: IUser = () => {
  return {
    // from server
    username: null,
    email: null,

    // for UI
    isConnecting: false,
    isConnected: false,
    isDisconnecting: false
  };
};

```

### 4) Create the reducer `UserReducer`

A reducer takes 2 arguments :

- The current state
- An `Action` of type `Action<{type: string, payload: any}>`

### **Reminder :** A reducer needs to be initialized at some point

As we defined the default state of our reducer in part 3), we'll be able to use it like that :

`user.reducer.ts`

```js
export const UserReducer: ActionReducer<IUser> = (user: IUser, action: Action) => {
  if (user === null) {
    return userFactory();
  }
  
  // ...
}

```

Hopefully, there's an easier way to write that by using our `factory` function to return an object and within the reducer use an (ES6) [default parameters value](http://es6-features.org/#DefaultParameterValues) :

```js
export const UserReducer: ActionReducer<IUser> = (user: IUser = UserFactory(), action: Action) => {
  // ...
}

```

Then, we need to handle every actions in our reducer :
****TIP****: Use [ES6 `Object.assign`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/assign) function to keep our state immutable

```js
export const UserReducer: ActionReducer<IUser> = (user: IUser = UserFactory(), action: Action) => {
  switch (action.type) {
    case UserActions.USR_IS_CONNECTING:
      return Object.assign({}, user, { isConnecting: true });

    case UserActions.USR_IS_CONNECTED:
      return Object.assign({}, user, { isConnecting: false, isConnected: true, username: action.payload.username });

    case UserActions.USR_IS_DISCONNECTING:
      return Object.assign({}, user, { isDisconnecting: true });

    case UserActions.USR_IS_DISCONNECTED:
      return Object.assign({}, user, { isDisconnecting: false, isConnected: false });

    default:
      return user;
  }
};

```

### 5) Import our `UserReducer` into our main module to build the `Store`

`app.module.ts`

```js
@NgModule({
    declarations: [
    AppComponent
    ],
    imports: [
    // angular modules
    // ...

    // declare your store by providing your reducers
    // (every reducer should return a default state)
    StoreModule.provideStore({
        user: UserReducer,
        // of course, you can put as many reducers here as you want
        // ...
    }),

    // other modules to import
    // ...
    ]
});

```

### 6) Use data from the `Store` to display information in our view

Everything is now ready on logic side and we just have to display what we want in two components :

- `UserComponent`: **[Dumb component]** We'll just pass the user object from the store using `@Input` property and `async` pipe. This way, the component will receive the user only once it's available (and the `user` will be of type `IUser` and not of type `Observable<IUser>` !)
- `LoginComponent` **[Smart component]** We'll directly inject the `Store` into this component and work only on `user` as an `Observable`.

`user.component.ts`

```js
@Component({
  selector: 'user',
  styles: [
    '.table { max-width: 250px; }',
    '.truthy { color: green; font-weight: bold; }',
    '.falsy { color: red; }'
  ],
  template: `
    <h2>User information :</h2>

    <table class="table">
      <tr>
        <th>Property</th>
        <th>Value</th>
      </tr>

      <tr>
        <td>username</td>
        <td [class.truthy]="user.username" [class.falsy]="!user.username">
          {{ user.username ? user.username : 'null' }}
        </td>
      </tr>

      <tr>
        <td>email</td>
        <td [class.truthy]="user.email" [class.falsy]="!user.email">
          {{ user.email ? user.email : 'null' }}
        </td>
      </tr>

      <tr>
        <td>isConnecting</td>
        <td [class.truthy]="user.isConnecting" [class.falsy]="!user.isConnecting">
          {{ user.isConnecting }}
        </td>
      </tr>

      <tr>
        <td>isConnected</td>
        <td [class.truthy]="user.isConnected" [class.falsy]="!user.isConnected">
          {{ user.isConnected }}
        </td>
      </tr>

      <tr>
        <td>isDisconnecting</td>
        <td [class.truthy]="user.isDisconnecting" [class.falsy]="!user.isDisconnecting">
          {{ user.isDisconnecting }}
        </td>
      </tr>
    </table>
  `
})
export class UserComponent {
  @Input() user;

  constructor() { }
}

```

`login.component.ts`

```js
@Component({
  selector: 'login',
  template: `
    <form
      *ngIf="!(user | async).isConnected"
      #loginForm="ngForm"
      (ngSubmit)="login(loginForm.value.username)"
    >
      <input
        type="text"
        name="username"
        placeholder="Username"
        [disabled]="(user | async).isConnecting"
        ngModel
      >
 
      <button
        type="submit"
        [disabled]="(user | async).isConnecting || (user | async).isConnected"
      >Log me in</button>
    </form>
 
    <button
      *ngIf="(user | async).isConnected"
      (click)="logout()"
      [disabled]="(user | async).isDisconnecting"
    >Log me out</button>
  `
})
export class LoginComponent {
  public user: Observable<IUser>;
 
  constructor(public store$: Store<AppState>, private userService: UserService) {
      this.user = store$.select('user');
  }
 
  login(username: string) {
    this.userService.login(username);
  }
 
  logout() {
    this.userService.logout();
  }
}

```

As `Ngrx` is a merge of `Redux` and `RxJs` concepts, it can be quite hard to understand the ins an outs at the beginning. But this is a powerful pattern that allows you as we've seen in this example to have a **reactive app** and were you can easily share your data. Don't forget that there's a [Plunkr](http://plnkr.co/edit/gS41jX?p=preview) available and you can fork it to make your own tests !

I hope it was helpful even tho the topic is quite long, cheers !

