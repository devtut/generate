---
metaTitle: "AngularJS - Session storage"
description: "Handling session storage through service using angularjs"
---

# Session storage




## Handling session storage through service using angularjs


### Session storage service :

Common factory service that will save and return the saved session data based on the key.

```

'use strict';

/**
 * @ngdoc factory
 * @name app.factory:storageService
 * @description This function will communicate with HTML5 sessionStorage via Factory Service.
 */

app.factory('storageService', ['$rootScope', function($rootScope) {

    return {
        get: function(key) {
            return sessionStorage.getItem(key);
        },
        save: function(key, data) {
            sessionStorage.setItem(key, data);
        }
    };
}]);

```

### In controller :

Inject the storageService dependency in the controller to set and get the data from the session storage.

```js
app.controller('myCtrl',['storageService',function(storageService) {

  // Save session data to storageService
  storageService.save('key', 'value');

  // Get saved session data from storageService
  var sessionData = storageService.get('key');

});

```

