---
metaTitle: "Node.js (express.js) with angular.js Sample code"
description: "Creating our project."
---

# Node.js (express.js) with angular.js Sample code


This example shows how to create a basic express app and then serve AngularJS.



## Creating our project.


We're good to go so, we run, again from console:

```js
mkdir our_project
cd our_project

```

Now we're in the place where our code will live. To create the main archive of our project you can run <br>

### Ok, but how we create the express skeleton project?

It's simple:

```js
npm install -g express express-generator

```

Linux distros and Mac should use **sudo** to install this because they're installed in the nodejs directory which is only accessible by the **root** user. If everything went fine we can, finally, create the express-app skeleton, just run

```js
express

```

This command will create inside our folder an express example app. The structure is as follow:

```js
bin/
public/
routes/
views/
app.js
package.json

```

Now if we run **npm start** an go to [http://localhost:3000](http://localhost:3000) we'll see the express app up and running, fair enough we've generated an express app without too much trouble, but how can we mix this with AngularJS?. <br>

### How express works, briefly?

**Express** is a framework built on top of **Nodejs**, you can see the official documentation at the [Express Site](http://expressjs.com). But for our purpose we need to know that **Express** is the responsible when we type, for example, [http://localhost:3000/home](http://localhost:3000/home) of rendering the home page of our application. From the recently created app created we can check:

```js
FILE: routes/index.js
var express = require('express');
var router = express.Router();

/* GET home page. */
router.get('/', function(req, res, next) {
  res.render('index', { title: 'Express' });
});

module.exports = router;

```

What this code is telling us is that when the user goes to [http://localhost:3000](http://localhost:3000) it must render the **index** view and pass a **JSON** with a title property and value Express. But when we check the views directory and open index.jade we can see this:

```js
extends layout
block content
    h1= title
    p Welcome to #{title}

```

This is another powerful Express feature, **template engines**, they allow you to render content in the page by passing variables to it or inherit another template so your pages are more compact and better understandable by others. The file extension is **.jade** as far as I know **Jade**  changed the name for **Pug**, basically is the same template engine but with some updates and core modifications. <br>

### Installing Pug and updating Express template engine.

Ok, to start using Pug as the template engine of our project we need to run:

```js
npm install --save pug

```

This will install Pug as a dependency of our project and save it to **package.json**. To use it we need to modify the file **app.js**:

```js
var app = express();
// view engine setup
app.set('views', path.join(__dirname, 'views'));
app.set('view engine', 'pug');

```

And replace the line of view engine with pug and that's all. We can run again our project with **npm start** and we'll see that everything is working fine.

### How AngularJS fits in all of this?

AngularJS is an Javascript **MVW**(Model-View-Whatever) Framework mainly used to create **SPA**(Simple Page Application) installing is fairly simple, you can go to [AngularJS website](https://angularjs.org) and download the latest version which is **v1.6.4**.<br>

After we downloaded AngularJS when should copy the file to our **public/javascripts** folder inside our project, a little explanation, this is the folder that serves the static assets of our site, images, css, javacript files and so on. Of course this is configurable through the **app.js** file, but we'll keep it simple. Now we create a file named **ng-app.js**, the file where our application will live, inside our javascripts public folder, just where AngularJS lives. To bring AngularJS up we need to modify the content of **views/layout.pug** as follow:

```js
doctype html
html(ng-app='first-app')
  head
    title= title
    link(rel='stylesheet', href='/stylesheets/style.css')
  body(ng-controller='indexController')
    block content

  script(type='text-javascript', src='javascripts/angular.min.js')
  script(type='text-javascript', src='javascripts/ng-app.js')

```

What are we doing here?, well, we're including AngularJS core and our recently created file **ng-app.js** so when the template is rendered it will bring AngularJS up, notice the use of the **ng-app** directive, this is telling AngularJS that this is our application name and it should stick to it.<br>
So, the content of our **ng-app.js** will be:

```js
angular.module('first-app', [])
       .controller('indexController', ['$scope', indexController]);

function indexController($scope) {
    $scope.name = 'sigfried';
}

```

We're using the most basic AngularJS feature here, **two-way data binding**, this allows us to refresh the content of our view and controller instantly, this is a very simple explanation, but you can make a research in Google or StackOverflow to see how it really works. <br>

So, we have the basic blocks of our AngularJS application, but there is something we got to do, we need to update our index.pug page to see the changes of our angular app, let's do it:

```js
extends layout
block content
  div(ng-controller='indexController')
    h1= title
    p Welcome {{name}}
    input(type='text' ng-model='name')

```

Here we're just binding the input to our defined property name in the AngularJS scope inside our controller:

```js
$scope.name = 'sigfried';

```

The purpose of this is that whenever we change the text in the input the paragraph above will update it content inside the {{name}}, this is called **interpolation**, again another AngularJS feature to render our content in the template. <br>

So, all is setup, we can now run **npm start** go to [http://localhost:3000](http://localhost:3000) and see our express application serving the page and AngularJS managing the application frontend.

