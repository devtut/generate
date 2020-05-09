---
metaTitle: "AngularJS - Constants"
description: "Create your first constant, Use cases"
---

# Constants



## Create your first constant


```js
angular
  .module('MyApp', [])
  .constant('VERSION', 1.0);

```

Your constant is now declared and can be injected in a controller, a service, a factory, a provider, and even in a config method:

```js
angular
  .module('MyApp')
  .controller('FooterController', function(VERSION) {
    this.version = VERSION;
  });

```

```js
<footer ng-controller="FooterController as Footer">{ { Footer.version } }</footer>

```



## Use cases


There is no revolution here, but angular constant can be useful specially when your application and/or team starts to grow ... or if you simply love writing beautiful code!

<li>
<p>**Refactor code.**
Example with event's names. If you use a lot of events in your application, you have event's names a little every where. A when a new developper join your team, he names his events with a different syntax, ... You can easily prevent this by grouping your event's names in a constant:</p>

```js
angular
  .module('MyApp')
  .constant('EVENTS', {
    LOGIN_VALIDATE_FORM: 'login::click-validate',
    LOGIN_FORGOT_PASSWORD: 'login::click-forgot',
    LOGIN_ERROR: 'login::notify-error',
    ...
  });

```



```js
angular
  .module('MyApp')
  .controller('LoginController', function($scope, EVENT) {
    $scope.$on(EVENT.LOGIN_VALIDATE_FORM, function() {
      ...
    });
  })

```


</li>

... and now, your event's names can take benefits from autocompletion !

<li>
<p>**Define configuration.**
Locate all your configuration in a same place:</p>

```js
angular
  .module('MyApp')
  .constant('CONFIG', {
    BASE_URL: {
      APP: 'http://localhost:3000',
      API: 'http://localhost:3001'
    },
    STORAGE: 'S3',
    ...
  });

```


</li>

<li>
<p>**Isolate parts.**
Sometimes, there are some things you are not very proud of ... like hardcoded value for example. Instead of let them in your main code, you can create an angular constant</p>

```js
angular
  .module('MyApp')
  .constant('HARDCODED', {
    KEY: 'KEY',
    RELATION: 'has_many',
    VAT: 19.6
  });

```


</li>

... and refactor something like

```js
$scope.settings = {
  username: Profile.username,
  relation: 'has_many',
  vat: 19.6
}

```

to

```js
$scope.settings = {
  username: Profile.username,
  relation: HARDCODED.RELATION,
  vat: HARDCODED.VAT
}

```



#### Remarks


**UPPERCASE your constant**: Writing constant in capital is a common best practice used in many languages. It's also useful to clearly identify the nature of injected elements:

When you see `.controller('MyController', function($scope, Profile, EVENT))`, you instantly know that:

- `$scope` is an angular element
- `Profile` is a custom Service or Factory
- `EVENT` is an angular constant

