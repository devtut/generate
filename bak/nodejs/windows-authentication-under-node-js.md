---
metaTitle: "Node.js - Windows authentication under node.js"
description: "Using activedirectory"
---

# Windows authentication under node.js




## Using activedirectory


The example below is taken from the full docs, available [here (GitHub)](https://github.com/gheeres/node-activedirectory) or [here (NPM)](https://npmdoc.github.io/node-npmdoc-activedirectory/build/apidoc.html).

### Installation

```js
npm install --save activedirectory

```

### Usage

```js
// Initialize
var ActiveDirectory = require('activedirectory');
var config = {
    url: 'ldap://dc.domain.com',
    baseDN: 'dc=domain,dc=com'
};
var ad = new ActiveDirectory(config);
var username = 'john.smith@domain.com';
var password = 'password';
// Authenticate
ad.authenticate(username, password, function(err, auth) {
    if (err) {
        console.log('ERROR: '+JSON.stringify(err));
        return;
    }
    if (auth) {
        console.log('Authenticated!');
    }
    else {
        console.log('Authentication failed!');
    }
});

```



#### Remarks


There are several other Active Directory APIS, such as [`activedirectory2`](https://www.npmjs.com/package/activedirectory2) and [`adldap`](https://www.npmjs.com/package/adldap).

