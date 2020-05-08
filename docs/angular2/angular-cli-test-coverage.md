---
metaTitle: "Angular 2 - angular-cli test coverage"
description: "A simple angular-cli command base test coverage, Detailed individual component base graphical test coverage reporting"
---

# angular-cli test coverage


test coverage is defined as a technique which determines whether our test cases are actually covering the application code and how much code is exercised when we run those test cases.

Angular CLI has built in code coverage feature with just a simple command `ng test --cc`



## A simple angular-cli command base test coverage


If you want to see overall test coverage statistics than of course in Angular CLI you can just type below command, and see the bottom of your command prompt window for results.

```js
ng test --cc // or --code-coverage

```

[<img src="https://i.stack.imgur.com/omjN2.png" alt="enter image description here" />](https://i.stack.imgur.com/omjN2.png)



## Detailed individual component base graphical test coverage reporting


if you want to see component's individual coverage of tests follow these steps.

<li>
`npm install --save-dev karma-teamcity-reporter`
</li>
<li>

```js
Add `require('karma-teamcity-reporter')` to list of plugins in karma.conf.js

```


</li>
<li>
`ng test --code-coverage --reporters=teamcity,coverage-istanbul`
</li>

note that list of reporters is comma-separated, as we have added a new reporter, teamcity.

after running this command you can see the folder `coverage` in your dir and open `index.html` for a graphical view of test coverage.

[<img src="https://i.stack.imgur.com/luaq2.png" alt="enter image description here" />](https://i.stack.imgur.com/luaq2.png)

You can also set the coverage threshold that you want to achieve, in `karma.conf.js`, like this.

```js
coverageIstanbulReporter: {
      reports: ['html', 'lcovonly'],
      fixWebpackSourcePaths: true,
      thresholds: {
        statements: 90,
        lines: 90,
        branches: 90,
        functions: 90
      }
    },

```

