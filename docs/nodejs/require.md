---
metaTitle: "Require()"
description: "Beginning require() use with a function and file, Beginning require() use with an NPM package"
---

# Require()


This documentation focuses on explaining the uses and of the `require()` statement that [NodeJS](https://nodejs.org/en/) includes in their language.

Require is an import of certain files or packages used with NodeJS's modules. It is used to improve code structure and uses. `require()` is used on files that are installed locally, with a direct route from the file that is `require`'ing.



## Beginning require() use with a function and file


Require is a statement that Node interprets as, in some sense, a `getter` function. For example, say you have a file named `analysis.js`, and the inside of your file looks like this,

```js
function analyzeWeather(weather_data) {
  console.log('Weather information for ' + weather_data.time + ': ');
  console.log('Rainfall: ' + weather_data.precip);
  console.log('Temperature: ' + weather_data.temp);
  //More weather_data analysis/printing...
}

```

This file contains only the method, `analyzeWeather(weather_data)`. If we want to use this function, it must be either used inside of this file, or copied to the file it wants to be used by. However, Node has included a very useful tool to help with code and file organization, which is [modules](https://nodejs.org/api/modules.html).

In order to utilize our function, we must first `export` the function through a statement at the beginning. Our new file looks like this,

```js
module.exports = {
  analyzeWeather: analyzeWeather
}
function analyzeWeather(weather_data) {
  console.log('Weather information for ' + weather_data.time + ': ');
  console.log('Rainfall: ' + weather_data.precip);
  console.log('Temperature: ' + weather_data.temp);
  //More weather_data analysis/printing...
}

```

With this small `module.exports` statement, our function is now ready for use outside of the file. All that is left to do is to use `require()`.

When `require`'ing a function or file, the syntax is very similar. It is usually done at the beginning of the file and set to `var`'s or [`const`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/const)'s for use throughout the file. For example, we have another file (on the same level as `analyze.js` named `handleWeather.js` that looks like this,

```js
const analysis = require('./analysis.js');

weather_data = {
  time: '01/01/2001',
  precip: 0.75,
  temp: 78,
  //More weather data...
};
analysis.analyzeWeather(weather_data);

```

In this file, we are using `require()` to grab our `analysis.js` file. When used, we just call the variable or constant assigned to this `require` and use whatever function inside that is exported.



## Beginning require() use with an NPM package


Node's `require` is also very helpful when used in tandem with an [NPM package](https://docs.npmjs.com/how-npm-works/packages). Say, for example, you would like to use the NPM package [`require`](https://www.npmjs.com/package/request) in a file named `getWeather.js`. After [NPM installing](https://docs.npmjs.com/cli/install) your package through your command line (`git install request`), you are ready to use it. Your `getWeather.js` file might like look this,

```js
var https = require('request');

//Construct your url variable...
https.get(url, function(error, response, body) {
  if (error) {
    console.log(error);
  } else {
    console.log('Response => ' + response);
    console.log('Body => ' + body);
  }
});

```

When this file is run, it first `require`'s (imports) the package you just installed called `request`. Inside of the `request` file, there are many functions you now have access to, one of which is called `get`. In the next couple lines, the function is used in order to make an [HTTP GET request](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol#Request_methods).



#### Syntax


- module.exports = {testFunction: testFunction};
- var test_file = require('./testFile.js'); //Let us have a file named `testFile`
- test_file.testFunction(our_data); //Let `testFile` have function `testFunction`



#### Remarks


Using `require()` allows code to be structured in a way similar to Java's use of [classes](https://docs.oracle.com/javase/tutorial/java/javaOO/classes.html) and public methods. If a function is `.export`'ed, it can be `require`'ed in another file to be used. If a file is not `.export`'ed, it cannot be used in another file.

