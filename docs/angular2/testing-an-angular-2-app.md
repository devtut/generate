---
metaTitle: "Angular 2 - Testing an Angular 2 App"
description: "Installing the Jasmine testing framework, Setting up testing with Gulp, Webpack, Karma and Jasmine, Testing Http Service, Testing Angular Components - Basic"
---

# Testing an Angular 2 App



## Installing the Jasmine testing framework


The most common way to test Angular 2 apps is with the Jasmine test framework. Jasmine allows you to test your code in the browser.

### Install

To get started, all you need is the `jasmine-core` package (not `jasmine`).

```js
npm install jasmine-core --save-dev --save-exact

```

### Verify

To verify that Jasmine is set up properly, create the file `./src/unit-tests.html` with the following content and open it in the browser.



## Setting up testing with Gulp, Webpack, Karma and Jasmine


The first thing we need is to tell karma to use Webpack to read our tests, under a configuration we set for the webpack engine. Here, I am using babel because I write my code in ES6, you can change that for other flavors, such as Typescript. Or I use Pug (formerly Jade) templates, you don't have to.

Still, the strategy remains the same.

So, this is a webpack config:

```js
const webpack = require("webpack");
let packConfig = {
    entry: {},
    output: {},
    plugins:[
        new webpack.DefinePlugin({
            ENVIRONMENT: JSON.stringify('test')
        })
    ],
    module: {
       loaders: [
        {
            test: /\.js$/,
            exclude:/(node_modules|bower_components)/,
            loader: "babel",
            query:{
                presets:["es2015", "angular2"]
            }
        },
        {
            test: /\.woff2?$|\.ttf$|\.eot$|\.svg$/,
            loader: "file"
        },
        {
            test: /\.scss$/,
            loaders: ["style", "css", "sass"]
        },
        {
            test: /\.pug$/,
            loader: 'pug-html-loader'
        },
        ]
    },
    devtool : 'inline-cheap-source-map'
};
module.exports = packConfig;

```

And then, we need a karma.config.js file to use that webpack config:

```js
const packConfig = require("./webpack.config.js");
module.exports = function (config) {
    config.set({
    basePath: '',
    frameworks: ['jasmine'],
    exclude:[],
    files: [
        {pattern: './karma.shim.js', watched: false}
    ],

    preprocessors: {
        "./karma.shim.js":["webpack"]
    },
    webpack: packConfig,

    webpackServer: {noInfo: true},

    port: 9876,

    colors: true,

    logLevel: config.LOG_INFO,

    browsers: ['PhantomJS'],

    concurrency: Infinity,

    autoWatch: false,
    singleRun: true
});
};

```

So far, we have told Karma to use webpack, and we have told it to start at a file called **karma.shim.js**. this file will have the job of acting as the starting point for webpack. webpack will read this file and use the **import** and **require** statements to gather all our dependencies and run our tests.

So now, let's look at the karma.shim.js file:

```js
// Start of ES6 Specific stuff
import "es6-shim";
import "es6-promise";
import "reflect-metadata";
// End of ES6 Specific stuff

import "zone.js/dist/zone";
import "zone.js/dist/long-stack-trace-zone";
import "zone.js/dist/jasmine-patch";
import "zone.js/dist/async-test";
import "zone.js/dist/fake-async-test";
import "zone.js/dist/sync-test";
import "zone.js/dist/proxy-zone";

import 'rxjs/add/operator/map';
import 'rxjs/add/observable/of';

Error.stackTraceLimit = Infinity;

import {TestBed} from "@angular/core/testing";
import { BrowserDynamicTestingModule, platformBrowserDynamicTesting} from "@angular/platform-browser-dynamic/testing";

TestBed.initTestEnvironment(
     BrowserDynamicTestingModule,
     platformBrowserDynamicTesting());

let testContext = require.context('../src/app', true, /\.spec\.js/);
testContext.keys().forEach(testContext);

```

In essence, we are importing **TestBed** from angular core testing, and initiating the environment, as it needs to be initiated only once for all of our tests. Then, we are going through the **src/app** directory recursively and reading every file that ends with **.spec.js** and feed them to testContext, so they will run.

I usually try to put my tests the same place as the class. Personat taste, it makes it easier for me to import dependencies and refactor tests with classes. But if you want to put your tests somewhere else, like under **src/test** directory for example, here is you chance. change the line before last in the karma.shim.js file.

Perfect. what is left? ah, the gulp task that uses the karma.config.js file we made above:

```js
gulp.task("karmaTests",function(done){
    var Server = require("karma").Server;
    new Server({
        configFile : "./karma.config.js",
        singleRun: true,
        autoWatch: false
    }, function(result){
        return result ? done(new Error(`Karma failed with error code ${result}`)):done();
    }).start();
}); 

```

I am now starting the server with the config file we created, telling it to run once and don't watch for changes. I find this to suite me better as the tests will run only if I am ready for them to run, but of course if you want different you know where to change.

And as my final code sample, here is a set of tests for the Angular 2 tutorial, "Tour of Heroes".

```js
import {
    TestBed,
    ComponentFixture,
    async
} from "@angular/core/testing";

import {AppComponent} from "./app.component";
import {AppModule} from "./app.module";
import Hero from "./hero/hero";

describe("App Component", function () {

    beforeEach(()=> {
        TestBed.configureTestingModule({
            imports: [AppModule]
        });
    
        this.fixture = TestBed.createComponent(AppComponent);
        this.fixture.detectChanges();
    });

    it("Should have a title", async(()=> {
        this.fixture.whenStable().then(()=> {
            expect(this.fixture.componentInstance.title).toEqual("Tour of Heros");
        });
    }));

    it("Should have a hero", async(()=> {
        this.fixture.whenStable().then(()=> {
            expect(this.fixture.componentInstance.selectedHero).toBeNull();
        });
    }));

    it("Should have an array of heros", async(()=>
        this.fixture.whenStable().then(()=> {
            const cmp = this.fixture.componentInstance;
            expect(cmp.heroes).toBeDefined("component should have a list of heroes");
            expect(cmp.heroes.length).toEqual(10, "heroes list should have 10 members");
            cmp.heroes.map((h, i)=> {
                expect(h instanceof Hero).toBeTruthy(`member ${i} is not a Hero instance. ${h}`)
            });
        })));

        it("Should have one list item per hero", async(()=>
        this.fixture.whenStable().then(()=> {
            const ul = this.fixture.nativeElement.querySelector("ul.heroes");
            const li = Array.prototype.slice.call(
                this.fixture.nativeElement.querySelectorAll("ul.heroes>li"));
            const cmp = this.fixture.componentInstance;
            expect(ul).toBeTruthy("There should be an unnumbered list for heroes");
            expect(li.length).toEqual(cmp.heroes.length, "there should be one li for each hero");
            li.forEach((li, i)=> {
                expect(li.querySelector("span.badge"))
                    .toBeTruthy(`hero ${i} has to have a span for id`);
                expect(li.querySelector("span.badge").textContent.trim())
                    .toEqual(cmp.heroes[i].id.toString(), `hero ${i} had wrong id displayed`);
                expect(li.textContent)
                    .toMatch(cmp.heroes[i].name, `hero ${i} has wrong name displayed`);
            });
        })));

    it("should have correct styling of hero items", async(()=>
        this.fixture.whenStable().then(()=> {
            const hero = this.fixture.nativeElement.querySelector("ul.heroes>li");
            const win = hero.ownerDocument.defaultView ||hero.ownerDocument.parentWindow;
            const styles = win.getComputedStyle(hero);
            expect(styles["cursor"]).toEqual("pointer", "cursor should be pointer on hero");
            expect(styles["borderRadius"]).toEqual("4px", "borderRadius should be 4px");
        })));

    it("should have a click handler for hero items",async(()=>
        this.fixture.whenStable().then(()=>{
            const cmp = this.fixture.componentInstance;
            expect(cmp.onSelect)
                .toBeDefined("should have a click handler for heros");
            expect(this.fixture.nativeElement.querySelector("input.heroName"))
                .toBeNull("should not show the hero details when no hero has been selected");
            expect(this.fixture.nativeElement.querySelector("ul.heroes li.selected"))
                .toBeNull("Should not have any selected heroes at start");

            spyOn(cmp,"onSelect").and.callThrough();
            this.fixture.nativeElement.querySelectorAll("ul.heroes li")[5].click();

            expect(cmp.onSelect)
                .toHaveBeenCalledWith(cmp.heroes[5]);
            expect(cmp.selectedHero)
                .toEqual(cmp.heroes[5], "click on hero should change hero");
        })
    ));
});

```

Noteworthy in this is how we have **beforeEach()** configure a test module and create the component in test, and how we call **detectChanges()** so that angular actually goes through the double-binding and all.

Notice that each test is a call to **async()** and it always waits for **whenStable** promise to resolve before examining the fixture. It then has access to the component through **componentInstance** and to the element through **nativeElement**.

There is one test which is checking the correct styling. as part of the Tutorial, Angular team demonstrates use of styles inside components. In our test, we use **getComputedStyle()** to check that styles are coming from where we specified, however we need the Window object for that, and we are getting it from the element as you can see in the test.



## Testing Http Service


Usually, services call remote Api to retrieve/send data. But unit tests shouldn't do network calls. Angular internally uses `XHRBackend` class to do http requests. User can override this to change behavior. Angular testing module provides `MockBackend` and `MockConnection` classes which can be used to test and assert http requests.

`posts.service.ts`
This service hits an api endpoint to fetch list of posts.

`posts.service.spec.ts`
Here, we will test above service by mocking http api calls.



## Testing Angular Components - Basic


The component code is given as below.

```js
import { Component } from '@angular/core';

@Component({
  selector: 'my-app',
  template: '<h1>{{title}}</h1>'
})
export class MyAppComponent{
  title = 'welcome';
}

```

For angular testing, angular provide its testing utilities along with the testing framework which helps in writing the good test case in angular. Angular utilities can be imported from `@angular/core/testing`

```js
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { MyAppComponent } from './banner-inline.component';

describe('Tests for MyAppComponent', () => {
  
  let fixture: ComponentFixture<MyAppComponent>;
  let comp: MyAppComponent;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [
        MyAppComponent
      ]
    });
  });

  beforeEach(() => {

    fixture = TestBed.createComponent(MyAppComponent);
    comp = fixture.componentInstance;

  });

  it('should create the MyAppComponent', () => {
    
      expect(comp).toBeTruthy();  

  });

});

```

In the above example, there is only one test case which explain the test case for component existence. In the above example angular testing utilities like `TestBed` and `ComponentFixture` are used.

`TestBed` is used to create the angular testing module and we configure this module with the `configureTestingModule` method to produce the module environment for the class we want to test.
Testing module to be configured before the execution of every test case that's why we configure the testing module in the `beforeEach` function.

`createComponent` method of `TestBed` is used to create the instance of the component under test. `createComponent` return the `ComponentFixture`. The fixture provides access to the component instance itself.

