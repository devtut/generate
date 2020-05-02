---
metaTitle: "Node JS Localization"
description: "using i18n module to maintains localization in  node js app"
---

# Node JS Localization


Its very easy to maintain localization nodejs express



## using i18n module to maintains localization in  node js app


Lightweight simple translation module with dynamic json storage. Supports plain vanilla node.js apps and should work with any framework (like express, restify and probably more) that exposes an app.use() method passing in res and req objects. Uses common __('...') syntax in app and templates. Stores language files in json files compatible to webtranslateit json format. Adds new strings on-the-fly when first used in your app. No extra parsing needed.

express + i18n-node + cookieParser and avoid concurrency issues

```js
// usual requirements
var express = require('express'),
    i18n = require('i18n'),
    app = module.exports = express();

i18n.configure({
  // setup some locales - other locales default to en silently
  locales: ['en', 'ru', 'de'],

  // sets a custom cookie name to parse locale settings from
  cookie: 'yourcookiename',

  // where to store json files - defaults to './locales'
  directory: __dirname + '/locales'
});

app.configure(function () {
  // you will need to use cookieParser to expose cookies to req.cookies
  app.use(express.cookieParser());

  // i18n init parses req for language headers, cookies, etc.
  app.use(i18n.init);

});

// serving homepage
app.get('/', function (req, res) {
  res.send(res.__('Hello World'));
});

// starting server
if (!module.parent) {
  app.listen(3000);
}

```

