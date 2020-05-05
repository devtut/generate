---
metaTitle: "Node.js - Node.JS with ES6"
description: "Node ES6 Support and creating a project with Babel, Use JS es6 on your NodeJS app"
---

# Node.JS with ES6


ES6, ECMAScript 6 or ES2015 is the latest [specification](http://www.ecma-international.org/ecma-262/6.0/) for JavaScript which introduces some syntactic sugar to the language. It's a big update to the language and introduces a lot of new [features](https://github.com/lukehoban/es6features)

More details on Node and ES6 can be found on their site [https://nodejs.org/en/docs/es6/](https://nodejs.org/en/docs/es6/)



## Node ES6 Support and creating a project with Babel


The whole ES6 spec is not yet implemented in its entirety so you will only be able to use some of the new features. You can see a list of the current supported ES6 features at [http://node.green/](http://node.green/)

Since NodeJS v6 there has been pretty good support. So if you using NodeJS v6 or above you can enjoy using ES6. However, you may also want to use some of the unreleased features and some from beyond. For this you will need to use a transpiler

It is possible to run a transpiler at run time and build, to use all of the ES6 features and more. The most popular transpiler for JavaScript is called [Babel](https://babeljs.io/)

Babel allows you to use all of the features from the ES6 specification and some additional not-in-spec features with 'stage-0' such as `import thing from 'thing` instead of `var thing = require('thing')`

If we wanted to create a project where we use 'stage-0' features such as import we would need to add Babel as a transpiler. You'll see projects using react and Vue and other commonJS based patterns implement stage-0 quite often.

create a new node project

```js
mkdir my-es6-app
cd my-es6-app
npm init

```

Install babel the ES6 preset and stage-0

`npm install --save-dev babel-preset-es2015 babel-preset-stage-2 babel-cli babel-register`

Create a new file called `server.js` and add a basic HTTP server.

```js
import http from 'http'

http.createServer((req, res) => {
  res.writeHead(200, {'Content-Type': 'text/plain'})
  res.end('Hello World\n')
}).listen(3000, '127.0.0.1')

console.log('Server running at http://127.0.0.1:3000/')

```

Note that we use an `import http from 'http'` this is a stage-0 feature and if it works it means we've got the transpiler working correctly.

If you run `node server.js` it will fail not knowing how to handle the import.

Creating a .babelrc file in the root of your directory and add the following settings

```js
{
  "presets": ["es2015", "stage-2"],
  "plugins": []
}

```

you can now run the server with `node src/index.js --exec babel-node`

Finishing off it is not a good idea to run a transpiler at runtime on a production app. We can however implement some scripts in our package.json to make it easier to work with.

```js
"scripts": {
    "start": "node dist/index.js",
    "dev": "babel-node src/index.js",
    "build": "babel src -d dist",
    "postinstall": "npm run build"
  },

```

The above will on `npm install` build the transpiled code to the dist directory allow `npm start` to use the transpiled code for our production app.

`npm run dev` will boot the server and babel runtime which is fine and preferred when working on a project locally.

Going one further you could then install nodemon `npm install nodemon --save-dev` to watch for changes and then reboot the node app.

This really speeds up working with babel and NodeJS. In you package.json just update the "dev" script to use nodemon

`"dev": "nodemon src/index.js --exec babel-node",`



## Use JS es6 on your NodeJS app


JS es6 (also known as es2015) is a set of new features to JS language aim to make it more intuitive when using OOP or while facing modern development tasks.

### Prerequisites:

<li>
Check out the new es6 features at [http://es6-features.org](http://es6-features.org) - it may clarify to you if you really intend to use it on your next NodeJS app
</li>
<li>
Check the compatibility level of your node version at [http://node.green](http://node.green)
</li>
<li>
If all is ok - let's code on!
</li>

Here is a very short sample of a simple `hello world` app with JS es6

```js
'use strict'

class Program
{
    constructor()
    {
        this.message = 'hello es6 :)';
    }

    print()
    {
        setTimeout(() =>
        {
            console.log(this.message);
            
            this.print();

        }, Math.random() * 1000);
    }
}

new Program().print();

```

You can run this program and observe how it print the same message over and over again.

Now.. let break it down line by line:

```js
'use strict'

```

This line is actually required if you intend to use js es6. `strict` mode, intentionally, has different semantics from normal code (please read more about it on MDN - [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Strict_mode)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Strict_mode))

```js
class Program

```

Unbelievable - a `class` keyword! Just for a quick reference - before es6 the only way do define a class in js was with the... `function` keyword!

```js
function MyClass() // class definition
{

}

var myClassObject = new MyClass(); // generating a new object with a type of MyClass

```

When using OOP, a class is a very fundamental ability which assist the developer to represent a specific part of a system (breaking down code is crucial when the code is getting larger.. for instance: when writing server-side code)

```js
constructor()
{
    this.message = 'hello es6 :)';
}

```

You got to admit - this is pretty intuitive! This is the c'tor of my class - this unique "function" will occur every time an object is created from this particular class (in our program - only once)

```js
print()
{
    setTimeout(() => // this is an 'arrow' function
    {
        console.log(this.message);
        
        this.print(); // here we call the 'print' method from the class template itself (a recursion in this particular case)

    }, Math.random() * 1000);
}

```

Because print is defined in the class scope - it is actually a method - which can be invoked from either the object of the class or from within the class itself!

So.. till now we defined our class.. time to use it:

```js
new Program().print();

```

Which is truly equals to:

```js
var prog = new Program(); // define a new object of type 'Program'

prog.print(); // use the program to print itself

```

**In conclusion:** JS es6 can simplify your code - make it more intuitive and easy to understand (comparing with the previous version of JS).. you may try to re-write an existing code of yours and see the difference for yourself

ENJOY :)

