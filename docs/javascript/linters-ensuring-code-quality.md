---
metaTitle: "JavsScript - Linters - Ensuring code quality"
description: "JSHint, ESLint / JSCS, JSLint"
---

# Linters - Ensuring code quality



## JSHint


[JSHint](http://jshint.com/) is an open source tool which detects errors and potential problems in JavaScript code.

To lint your JavaScript you have two options.

<li>Go to [JSHint.com](http://jshint.com/) and paste your code in there
on line text editor.</li>
<li>Install [JSHint in your IDE](http://jshint.com/install/).
<ul>
1. Atom: [linter-jshint](https://github.com/AtomLinter/linter-jshint) (must have [Linter](https://github.com/steelbrain/linter) plugin installed)
1. Sublime Text: [JSHint Gutter](https://github.com/victorporof/Sublime-JSHint) and/or [Sublime Linter](https://github.com/SublimeLinter/SublimeLinter-for-ST2)
1. Vim: [jshint.vim](https://github.com/walm/jshint.vim) or [jshint2.vim](https://github.com/Shutnik/jshint2.vim)
1. Visual Studio: [VSCode JSHint](https://github.com/Microsoft/vscode-jshint)
</ul>
</li>

A benefit of adding it to your IDE is that you can create a JSON configuration file named `.jshintrc` that will be used when linting your program. This is convent if you want to share configurations between projects.

Example `.jshintrc` file

```js
{
    "-W097": false, // Allow "use strict" at document level
    "browser": true, // defines globals exposed by modern browsers http://jshint.com/docs/options/#browser
    "curly": true, // requires you to always put curly braces around blocks in loops and conditionals http://jshint.com/docs/options/#curly
    "devel": true, // defines globals that are usually used for logging poor-man's debugging: console, alert, etc. http://jshint.com/docs/options/#devel
    // List global variables (false means read only)
    "globals": {
        "globalVar": true
    },
    "jquery": true, // This option defines globals exposed by the jQuery JavaScript library.
    "newcap": false,
    // List any global functions or const vars
    "predef": [
        "GlobalFunction",
        "GlobalFunction2"
    ],
    "undef": true, // warn about undefined vars
    "unused": true // warn about unused vars
}

```

JSHint also allows configurations for specific lines/blocks of code

```js
switch(operation)
{
   case '+'
   {
      result = a + b;
      break;
   }

   // JSHint W086 Expected a 'break' statement
   // JSHint flag to allow cases to not need a break
   /* falls through */
   case '*':
   case 'x':
   {
      result = a * b;
      break;
   }
}

// JSHint disable error for variable not defined, because it is defined in another file
/* jshint -W117 */
globalVariable = 'in-another-file.js';
/* jshint +W117 */

```

More configuration options are documented at [http://jshint.com/docs/options/](http://jshint.com/docs/options/)



## ESLint / JSCS


[ESLint](http://eslint.org/) is a code style linter and formatter for your style guide [much like JSHint](http://www.slant.co/versus/8627/8628/%7Ejshint_vs_eslint). ESLint merged with [JSCS](https://medium.com/@markelog/jscs-end-of-the-line-bc9bf0b3fdb2#.h2cktyall) in April of 2016. ESLint does take more effort to set up than JSHint, but there are clear instructions on their [website](http://eslint.org/docs/user-guide/getting-started) for getting started.

A sample configuration for ESLint is as follows:

```js
{
    "rules": {
        "semi": ["error", "always"], // throw an error when semicolons are detected 
        "quotes": ["error", "double"] // throw an error when double quotes are detected
    }
}

```

A sample configuration file where ALL rules are set to off, with descriptions for what they do can be found [here](https://gist.github.com/cletusw/e01a85e399ab563b1236).



## JSLint


[JSLint](http://www.jslint.com/) is the trunk from which JSHint branched. JSLint takes a much more opinionated stance on how to write JavaScript code, pushing you towards only using the parts [Douglas Crockford](http://crockford.com/) deems to be its "good parts", and away from any code that Crockford believes to have a better solution. The following StackOverflow thread may help you decide [which linter is right for you](http://stackoverflow.com/a/6803574/6194193). While there are differences (here are some brief comparisons between it and [JSHint](http://www.slant.co/versus/8627/8626/%7Ejshint_vs_jslint) / [ESLint](http://www.slant.co/versus/8628/8626/%7Eeslint_vs_jslint)), each option is extremely customizable.

For a more information about configuring JSLint check out [NPM](https://www.npmjs.com/package/jslint) or [github](https://gist.github.com/bretdavidson/3189814#file-jslint-options-descriptions).



#### Remarks


No matter what linter you choose every JavaScript Project should use one. They can help find error and make code more consistent. For more comparisions check out [comparison JavaScript linting tools](https://www.sitepoint.com/comparison-javascript-linting-tools/)

