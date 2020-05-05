---
metaTitle: "Node.js - Project Structure"
description: "A simple nodejs application with MVC and API"
---

# Project Structure


The structure of nodejs project is influenced by the personal preferences, project's architecture and module injection strategy being used.Also on event based arc' which uses dynamic module instantiation mechanism.
To have a MVC structure it is imperative to separate out the server side and client side source code as the client side code will probably be minimized and sent to browser and is public in its basic nature. And the server side or back-end will provide API to perform CRUD operations



## A simple nodejs application with MVC and API


- The first major distinction is between the dynamically generated directories which will be used for hosting and source directories.
- The source directories will have a config file or folder depending on the amount of configuration you may have . This includes the environment configuration and business logic configuration which you may choose to put inside config directory.

```

|-- Config
        |-- config.json
        |-- appConfig
            |-- pets.config
            |-- payment.config

```


<li>
Now the most vital directories where we distinguish between the server side/backend and the frontend modules . The 2 directories **server** and **webapp** represent the backend and frontend respectively which we can choose to put inside a source directory viz. **src**.
<blockquote>
You can go with different names as per personal choice for server or webapp depending on what makes sense for you. Make sure you dont want to make it too long or to complex as it is in the end internal project structure.
</blockquote>
</li>
<li>
Inside the **server** directory you can have the controller ,the App.js/index.js which will be you main nodejs file and start point .The server dir. can also have the **dto** dir which holds all the data transfer objects which will be usd by API controllers.

```js
 |-- server
      |-- dto
          |-- pet.js
          |-- payment.js
      |-- controller
          |-- PetsController.js
          |-- PaymentController.js
      |-- App.js

```


</li>
<li>
The webapp directory can be divided into two major parts **public** and **mvc** , this is again influenced by what build strategy  you want to use. We are using [browserfiy](http://browserify.org/) the build the MVC part of webapp and minimize the contents from **mvc** directory simply put.
<p>|-- webapp
|-- public
|-- mvc</p>
</li>
<li>
Now the public directory can contain all the static resources,images,css(you can have saas files as well) and most importantly the HTML files .
</li>

```js
|-- public 
    |-- build  // will contianed minified scripts(mvc)
    |-- images
        |-- mouse.jpg
        |-- cat.jpg
    |-- styles
        |-- style.css
    |-- views
        |-- petStore.html
        |-- paymentGateway.html
        |-- header.html
        |-- footer.html
    |-- index.html

```


<li>
The **mvc** directory will contain the front-end logic including the **models**,the **view controllers** and any other **utils** modules you may need as part of UI. Also the index.js or shell.js whichever may suite you is part of this directory as well.

```js
 |-- mvc
     |-- controllers
         |-- Dashborad.js
         |-- Help.js
         |-- Login.js
     |-- utils
     |-- index.js

```


</li>

So in conclusion the entire project structure will look like below.And a simple build task like **gulp browserify** will minify the mvc scripts and publish in **public** directory. We can then provide this public directory as static resource via ****express.use(satic('public' ))**** api.

```

   |-- node_modules
    |-- src
        |-- server
            |-- controller
            |-- App.js   // node app
        |-- webapp
            |-- public
                |-- styles
                |-- images
                |-- index.html
            |-- mvc
                |-- controller
                |-- shell.js  // mvc shell
    |-- config
    |-- Readme.md
    |-- .gitignore
    |-- package.json

```



#### Remarks


The project above uses browserify and vue.js modules as application base view and minification libs. So the project structure may changes minutely based on which mvc framework you use e.g. The build directory in public will need to contain all the mvc code.
You can have a task which does this for you .

