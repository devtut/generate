---
metaTitle: "ReactJS - Setting Up React Environment"
description: "Simple React Component, Install all dependencies, Configure webpack, Configure babel, HTML file to use react component, Transpile and bundle your component"
---

# Setting Up React Environment



## Simple React Component


We want to be able to compile below component and render it in our webpage

**Filename**: src/index.jsx



## Install all dependencies




## Configure webpack


Create a file `webpack.config.js` in the root of your working directory

**Filename**: webpack.config.js



## Configure babel


Create a file `.babelrc` in the root of our working directory

**Filename**: .babelrc

```js
{
    "presets": ["es2015","react"]
}

```



## HTML file to use react component


Setup a simple html file in the root of the project directory

**Filename**: index.html



## Transpile and bundle your component


Using webpack, you can bundle your component:

This will create our output file in `build` directory.

Open the HTML page in a browser to see component in action

