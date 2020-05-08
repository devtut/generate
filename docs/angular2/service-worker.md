---
metaTitle: "Angular 2 - Service Worker"
description: "Add Service Worker to our app"
---

# Service Worker


We will see how to set up a service working on angular, to allow our web app to have offline capabilities.

A Service worker is a special script which runs in the background in the browser and manages network requests to a given origin. It's originally installed by an app and stays resident on the user's machine/device. It's activated by the browser when a page from its origin is loaded and has the option to respond to HTTP requests during the page loading



## Add Service Worker to our app


First in case you are consulting mobile.angular.io the flag --mobile doesn't work anymore.

So to start , we can create a normal project with angular cli.

```js
ng new serviceWorking-example
cd serviceWorking-example

```

Now the important thing, to said to angular cli that we want to use service worker we need to do:

ng set apps.0.serviceWorker=true

If for  some reason you don’t have @angular/service-worker installed, you will see a message:

> 
<p>Your project is configured with serviceWorker = true, but
@angular/service-worker is not installed. Run `npm install --save-dev @angular/service-worker` and try again, or run `ng set apps.0.serviceWorker=false` in your .angular-cli.json.</p>


Check the .angular-cli.json and you now should see this:
"serviceWorker": true

When this flag is true, production builds will be set up with a service worker.

A ngsw-manifest.json file will be generated (or augmented in case we have create a ngsw-manifest.json in the root of the project, usually this is done to specify the routing ,in a future this will probably be done automatic) in the dist/ root, and the service worker script will be copied there. A short script will be added to index.html to register the service worker.

Now if we build the app in production mode
ng build --prod

And check dist/ folder.

You will see three new files there :

- worker-basic.min.js
- sw-register.HASH.bundle.js
- ngsw-manifest.json

Also, index.html now includes this sw-register script, which registers a Angular Service Worker (ASW) for us.

Refresh the page in your browser (served by the Web Server for Chrome)

Open Developer Tools. Go to the Application -> Service Workers

[<img src="https://i.stack.imgur.com/kVKmq.png" alt="enter image description here" />](https://i.stack.imgur.com/kVKmq.png)

Good now the Service Worker is up and running!

Now our application, should load faster and we should be able to use the app offline.

Now if you enable the offline mode in the chrome console , you should see that our app in [http://localhost:4200/index.html](http://localhost:4200/index.html) is working without connection to internet.

But in [http://localhost:4200/](http://localhost:4200/) we have a problem and it doesn't load, this is due to
the static content cache only serves files listed in the manifest.

For example, if the manifest declares a URL of /index.html, requests to /index.html will be answered by the cache, but a request to / or /some/route will go to the network.

That's where the route redirection plugin comes in. It reads a routing config from the manifest and redirects configured routes to a specified index route.

Currently, this section of configuration must be hand-written (19-7-2017). Eventually, it will be generated from the route configuration present in the application source.

So if now we create or ngsw-manifest.json in the root of the project

```js
{
  "routing": {
    "routes": {
      "/": {
        "prefix": false
      }
    },
    "index": "/index.html"
  }
}

```

And we build again our app, now when we go to [http://localhost:4200/](http://localhost:4200/), we should be redirected to [http://localhost:4200/index.html](http://localhost:4200/index.html).

For further information about routing read the [official documentation here](https://github.com/angular/mobile-toolkit/tree/master/service-worker/worker)

Here you can find more documentation about service workers:

[https://developers.google.com/web/fundamentals/getting-started/primers/service-workers](https://developers.google.com/web/fundamentals/getting-started/primers/service-workers)

[https://docs.google.com/document/d/19S5ozevWighny788nI99worpcIMDnwWVmaJDGf_RoDY/edit#](https://docs.google.com/document/d/19S5ozevWighny788nI99worpcIMDnwWVmaJDGf_RoDY/edit#)

And here you can see an alternative way to implement the service working using SW precache library :

[https://coryrylan.com/blog/fast-offline-angular-apps-with-service-workers](https://coryrylan.com/blog/fast-offline-angular-apps-with-service-workers)

