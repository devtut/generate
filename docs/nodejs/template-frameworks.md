---
metaTitle: "Template frameworks"
description: "Nunjucks"
---

# Template frameworks



## Nunjucks


Server-side engine with block inheritance, autoescaping, macros, asynchronous control, and more. Heavily inspired by jinja2, very similar to Twig (php).

Docs - [http://mozilla.github.io/nunjucks/](http://mozilla.github.io/nunjucks/) <br/>
Install - `npm i nunjucks`<br/>

Basic usage with [**Express**](http://expressjs.com/) below.<br/><br/>
**app.js**

```js
var express = require ('express');
var nunjucks  = require('nunjucks');

var app = express();
app.use(express.static('/public'));

// Apply nunjucks and add custom filter and function (for example). 
var env = nunjucks.configure(['views/'], { // set folders with templates
    autoescape: true, 
    express: app
});
env.addFilter('myFilter', function(obj, arg1, arg2) {
    console.log('myFilter', obj, arg1, arg2);
    // Do smth with obj
    return obj;  
});
env.addGlobal('myFunc', function(obj, arg1) { 
    console.log('myFunc', obj, arg1);
    // Do smth with obj
    return obj;
});

app.get('/', function(req, res){
    res.render('index.html', {title: 'Main page'});    
});

app.get('/foo', function(req, res){
    res.locals.smthVar = 'This is Sparta!';
    res.render('foo.html', {title: 'Foo page'});    
});

app.listen(3000, function() {
    console.log('Example app listening on port 3000...');
});

```

**/views/index.html**

```js
<html>
<head>
    <title>Nunjucks example</title>
</head>
<body>
{% block content %} 
{{title}}
{% endblock %}
</body>
</html>

```

**/views/foo.html**

```js
{% extends "index.html" %}

{# This is comment #}
{% block content %}
    <h1>{{title}}</h1>
    {# apply custom function and next build-in and custom filters #}
    {{ myFunc(smthVar) | lower | myFilter(5, 'abc') }} 
{% endblock %}

```

