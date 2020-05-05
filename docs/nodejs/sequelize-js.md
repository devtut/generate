---
metaTitle: "Node.js - Sequelize.js"
description: "Installation, Defining Models"
---

# Sequelize.js




## Installation


Make sure that you first have Node.js and npm installed. Then install sequelize.js with npm

```js
npm install --save sequelize

```

You will also need to install supported database Node.js modules. You only need to install the one you are using<br><br>
For `MYSQL` and `Mariadb`

```js
npm install --save mysql

```

For `PostgreSQL`

```js
npm install --save pg pg-hstore

```

For `SQLite`

```

npm install --save sqlite

```

For `MSSQL`

```js
npm install --save tedious

```

Once you have you set up installed you can include and create a new Sequalize instance like so.

ES5 syntax

```js
var Sequelize = require('sequelize');
var sequelize = new Sequelize('database', 'username', 'password');

```

ES6 stage-0 Babel syntax

```js
import Sequelize from 'sequelize';
const sequelize = new Sequelize('database', 'username', 'password');

```

You now have an instance of sequelize available. You could if you so feel inclined call it a different name such as

```js
var db = new Sequelize('database', 'username', 'password');

```

or

```js
var database = new Sequelize('database', 'username', 'password');

```

that part is your prerogative. Once you have this installed you can use it inside of your application as per the API documentation [http://docs.sequelizejs.com/en/v3/api/sequelize/](http://docs.sequelizejs.com/en/v3/api/sequelize/)

Your next step after install would be to [set up your own model](http://docs.sequelizejs.com/en/v3/docs/getting-started/#your-first-model)



## Defining Models


There are two ways to define models in sequelize; with `sequelize.define(...)`, or `sequelize.import(...)`. Both functions return a sequelize model object.

### 1. sequelize.define(modelName, attributes, [options])

This is the way to go if you'd like to define all your models in one file, or if you want to have extra control of your model definition.

For the documentation and more examples, check out the [doclets documentation](https://doclets.io/sequelize/sequelize/master#dl-Sequelize-define), or [sequelize.com's documentation](http://docs.sequelizejs.com/en/v3/docs/models-definition/).

### 2. sequelize.import(path)

If your model definitions are broken into a file for each, then `import` is your friend. In the file where you initialize Sequelize, you need to call import like so:

Then in your model definition files, your code will look something like this:

For more information on how to use `import`, check out sequelize's [express example on GitHub](https://github.com/sequelize/express-example/tree/master/models).

