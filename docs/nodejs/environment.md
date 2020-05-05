---
metaTitle: "Node.js - Environment"
description: "Accessing environment variables, process.argv command line arguments, Using different Properties/Configuration for different environments like dev, qa, staging etc., Loading environment properties from a property file"
---

# Environment



## Accessing environment variables


The `process.env` property returns an object containing the user environment.

It returns an object like this one :

```js
{
  TERM: 'xterm-256color',
  SHELL: '/usr/local/bin/bash',
  USER: 'maciej',
  PATH: '~/.bin/:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin',
  PWD: '/Users/maciej',
  EDITOR: 'vim',
  SHLVL: '1',
  HOME: '/Users/maciej',
  LOGNAME: 'maciej',
  _: '/usr/local/bin/node'
}

```

```js
process.env.HOME // '/Users/maciej'

```

If you set environment variable `FOO` to `foobar`, it will be accessible with:

```js
process.env.FOO // 'foobar' 

```



## process.argv command line arguments


[process.argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv) is an array containing the command line arguments. The first element will be `node`, the second element will be the name of the JavaScript file. The next elements will be any additional command line arguments.

**Code Example:**

Output sum of all command line arguments

`index.js`

```js
var sum = 0;
for (i = 2; i < process.argv.length; i++) {
    sum += Number(process.argv[i]);
}

console.log(sum);

```

**Usage Exaple:**

```js
node index.js 2 5 6 7

```

Output will be `20`

**A brief explanation of the code:**

Here in for loop `for (i = 2; i < process.argv.length; i++)` loop begins with 2 because first two elements in process.argv array **always** is `['path/to/node.exe', 'path/to/js/file', ...]`

Converting to number `Number(process.argv[i])` because elements in process.argv array **always** is string



## Using different Properties/Configuration for different environments like dev, qa, staging etc.


Large scale applications often need different properties when running on different environments. we can achieve this by passing arguments to NodeJs application and using same argument in node process to load specific environment property file.

Suppose we have two property files for different environment.

<li>
dev.json

```js
  {
      PORT : 3000,
      DB : {
          host : "localhost",
          user : "bob",
          password : "12345"
      }
  }

```


</li>
<li>
qa.json

```js
  {
          PORT : 3001,
          DB : {
              host : "where_db_is_hosted",
              user : "bob",
              password : "54321"
          }
  }

```


</li>

Following code in application will export respective property file which we want to use.

Suppose the code is in environment.js

```js
process.argv.forEach(function (val, index, array) {
    var arg = val.split("=");
    if (arg.length > 0) {
        if (arg[0] === 'env') {
            var env = require('./' + arg[1] + '.json');
            module.exports = env;
        }
    }
});

```

We give arguments to the application like following

```js
node app.js env=dev

```

if we are using process manager like **forever** than it as simple as

```js
forever start app.js env=dev

```

**How to use the configuration file**

```

var env= require("environment.js");

```



## Loading environment properties from a "property file"


- Install properties reader:

```js
npm install properties-reader --save

```


- Create a **directory env** to store your properties files:

```js
mkdir env

```


- Create **environments.js**:

```js
process.argv.forEach(function (val, index, array) {
    var arg = val.split("=");
    if (arg.length > 0) {
        if (arg[0] === 'env') {
            var env = require('./env/' + arg[1] + '.properties');
            module.exports = env;
        }
    }
});

```


- Sample **development.properties** properties file:

```js
# Dev properties
[main]
# Application port to run the node server
app.port=8080

[database]
# Database connection to mysql
mysql.host=localhost
mysql.port=2500
...

```


- Sample usage of the loaded properties:

```js
var enviorment = require('./environments');
var PropertiesReader = require('properties-reader');
var properties = new PropertiesReader(enviorment);
   
var someVal = properties.get('main.app.port');

```


- Starting the express server

```js
npm start env=development

```

or

```js
npm start env=production

```

