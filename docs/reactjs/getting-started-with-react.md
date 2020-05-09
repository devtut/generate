---
metaTitle: "ReactJS - Getting started with React"
description: "What is ReactJS?, Installation or Setup, Hello World with Stateless Functions, Absolute Basics of Creating Reusable Components, Create React App, Hello World, Hello World Component"
---

# Getting started with React



## What is ReactJS?


ReactJS is an open-source, component based front end library responsible only for the **view layer** of the application. It is maintained by Facebook.

ReactJS uses virtual DOM based mechanism to fill in data (views) in HTML DOM. The virtual DOM works fast owning to the fact that it only changes individual DOM elements instead of reloading complete DOM every time

> 
A React application is made up of multiple **components**, each responsible for outputting a small, reusable piece of HTML. Components can be nested within other components to allow complex applications to be built out of simple building blocks.  A component may also maintain internal state - for example, a TabList component may store a variable corresponding to the currently open tab.


React allows us to write components using a domain-specific language called JSX. JSX allows us to write our components using HTML, whilst mixing in JavaScript events. React will internally convert this into a virtual DOM, and will ultimately output our HTML for us.

React "**reacts**" to state changes in your components quickly and automatically to rerender the components in the HTML DOM by utilizing the virtual DOM. The virtual DOM is an in-memory representation of an actual DOM. By doing most of the processing inside the virtual DOM rather than directly in the browser's DOM, React can act quickly and only add, update, and remove components which have changed since the last render cycle occurred.



## Installation or Setup


ReactJS is a JavaScript library contained in a single file `react-<version>.js` that can be included in any HTML page. People also commonly install the React DOM library `react-dom-<version>.js` along with the main React file:

**Basic Inclusion**

```js
<!DOCTYPE html>
<html>
    <head></head>
    <body>
    <script type="text/javascript" src="/path/to/react.js"></script>
    <script type="text/javascript" src="/path/to/react-dom.js"></script>
    <script type="text/javascript">
        // Use react JavaScript code here or in a separate file
    </script>
    </body>
</html>

```

To get the JavaScript files, go to [the installation page](https://facebook.github.io/react/docs/installation.html) of the official React documentation.

React also supports [JSX syntax](https://facebook.github.io/react/docs/jsx-in-depth.html). JSX is an extension created by Facebook that adds XML syntax to JavaScript. In order to use JSX you need to include the Babel library and change `<script type="text/javascript">` to `<script type="text/babel">` in order to translate JSX to Javascript code.

```js
<!DOCTYPE html>
<html>
    <head></head>
    <body>
    <script type="text/javascript" src="/path/to/react.js"></script>
    <script type="text/javascript" src="/path/to/react-dom.js"></script>
    <script src="https://npmcdn.com/babel-core@5.8.38/browser.min.js"></script>
    <script type="text/babel">
        // Use react JSX code here or in a separate file
    </script>
    </body>
</html>

```

**Installing via npm**

You can also install React using [npm](https://www.npmjs.com/) by doing the following:

`npm install --save react react-dom`

To use React in your JavaScript project, you can do the following:

```js
var React = require('react');
var ReactDOM = require('react-dom');
ReactDOM.render(<App />, ...);

```

**Installing via Yarn**

Facebook released its own package manager named [Yarn](https://yarnpkg.com/), which can also be used to install React. After installing Yarn you just need to run this command:

You can then use React in your project in exactly the same way as if you had installed React via npm.



## Hello World with Stateless Functions


Stateless components are getting their philosophy from functional programming. Which implies that: A function returns all time the same thing exactly on what is given to it.

### For example:

```js
const statelessSum = (a, b) => a + b;

let a = 0;
const statefulSum = () => a++;

```

As you can see from the above example that, statelessSum is always will return the same values given a and b. However, statefulSum function will not return the same values given even no parameters. This type of function's behaviour is also called as a **side-effect**. Since, the component affects somethings beyond.

So, it is advised to use stateless components more often, since they are **side-effect free** and will create the same behaviour always. That is what you want to be after in your apps because fluctuating state is the worst case scenario for a maintainable program.

The most basic type of react component is one without state. React components that are pure functions of their props and do not require any internal state management can be written as simple JavaScript functions. These are said to be `Stateless Functional Components` because they are a function only of `props`, without having any `state` to keep track of.

Here is a simple example to illustrate the concept of a `Stateless Functional Component`:

```js
// In HTML
<div id="element"></div>

// In React
const MyComponent = props => {
    return <h1>Hello, {props.name}!</h1>;
};

ReactDOM.render(<MyComponent name="Arun" />, element);
// Will render <h1>Hello, Arun!</h1>

```

Note that all that this component does is render an `h1` element containing the `name` prop. This component doesn't keep track of any state. Here's an ES6 example as well:

```js
import React from 'react'

const HelloWorld = props => (
    <h1>Hello, {props.name}!</h1>
)

HelloWorld.propTypes = {
    name: React.PropTypes.string.isRequired
}

export default HelloWorld

```

Since these components do not require a backing instance to manage the state, React has more room for optimizations. The implementation is clean, but as of yet [no such optimizations for stateless components have been implemented](https://github.com/facebook/react/issues/5677#issuecomment-165125151).



## Absolute Basics of Creating Reusable Components


### Components and Props

As React concerns itself only with an application's view, the bulk of development in React will be the creation of components. A component represents a portion of the view of your application. "Props" are simply the attributes used on a JSX node (e.g. `<SomeComponent someProp="some prop's value" />`), and are the primary way our application interacts with our components. In the snippet above, inside of SomeComponent, we would have access to `this.props`, whose value would be the object `{someProp: "some prop's value"}`.

It can be useful to think of React components as simple functions - they take input in the form of "props", and produce output as markup. Many simple components take this a step further, making themselves "Pure Functions", meaning they do not issue side effects, and are idempotent (given a set of inputs, the component will always produce the same output). This goal can be formally enforced by actually creating components as functions, rather than "classes". There are three ways of creating a React component:

- **Functional ("Stateless") Components**

```js
const FirstComponent = props => (
    <div>{props.content}</div>
);

```


- **React.createClass()**

```js
const SecondComponent = React.createClass({
    render: function () {
        return (
            <div>{this.props.content}</div>
        );
    }
});

```


- **ES2015 Classes**

```js
class ThirdComponent extends React.Component {
    render() {
        return (
            <div>{this.props.content}</div>
        );
    }
}

```

These components are used in exactly the same way:

```js
const ParentComponent = function (props) {
    const someText = "FooBar";
    return (
        <FirstComponent content={someText} />
        <SecondComponent content={someText} />
        <ThirdComponent content={someText} />
    );
}

```

The above examples will all produce identical markup.

Functional components cannot  have "state" within them. So if your component needs to have a state, then go for class based components. Refer [Creating Components](http://stackoverflow.com/documentation/reactjs/1185/components/4649/creating-components#t=201703241332591992903) for more information.

As a final note, react props are immutable once they have been passed in, meaning they cannot be modified from within a component. If the parent of a component changes the value of a prop, React handles replacing the old props with the new, the component will rerender itself using the new values.

See [Thinking In React](https://facebook.github.io/react/docs/thinking-in-react.html) and [Reusable Components](https://facebook.github.io/react/docs/reusable-components.html) for deeper dives into the relationship of props to components.



## Create React App


[create-react-app](https://github.com/facebookincubator/create-react-app) is a React app boilerplate generator created by Facebook.  It provides a development environment configured for ease-of-use with minimal setup, including:

- ES6 and JSX transpilation
- Dev server with hot module reloading
- Code linting
- CSS auto-prefixing
- Build script with JS, CSS and image bundling, and sourcemaps
- Jest testing framework

### Installation

First, install create-react-app globally with node package manager (npm).

```js
npm install -g create-react-app

```

Then run the generator in your chosen directory.

```js
create-react-app my-app

```

Navigate to the newly created directory and run the start script.

```js
cd my-app/
npm start

```

### Configuration

create-react-app is intentionally non-configurable by default.  If non-default usage is required, for example, to use a compiled CSS language such as Sass, then the eject command can be used.

```js
npm run eject

```

This allows editing of all configuration files. N.B. this is an irreversible process.

### Alternatives

Alternative React boilerplates include:

- [enclave](https://github.com/eanplatter/enclave)
- [nwb](https://github.com/insin/nwb)
- [motion](https://github.com/motion/motion)
- [rackt-cli](https://github.com/mzabriskie/rackt-cli)
- [budō](https://github.com/mattdesl/budo)
- [rwb](https://github.com/petehunt/rwb)
- [quik](https://github.com/satya164/quik)
- [sagui](https://github.com/saguijs/sagui)
- [roc](https://github.com/rocjs/roc)

**Build React App**

To build your app for production ready, run following command

```js
npm run build

```



## Hello World


**Without JSX**

Here's a basic example that uses React's main API to create a React element and the React DOM API to render the React element in the browser.

```js
<!DOCTYPE html>
<html>
  <head>
    <meta charset="UTF-8" />
    <title>Hello React!</title>

    <!-- Include the React and ReactDOM libraries -->
    <script src="https://fb.me/react-15.2.1.js"></script>
    <script src="https://fb.me/react-dom-15.2.1.js"></script>

  </head>
  <body>
    <div id="example"></div>

    <script type="text/javascript">

      // create a React element rElement
      var rElement = React.createElement('h1', null, 'Hello, world!');

      // dElement is a DOM container
      var dElement = document.getElementById('example');

      // render the React element in the DOM container
      ReactDOM.render(rElement, dElement);

    </script>

  </body>
</html>

```

**With JSX**

Instead of creating a React element from strings one can use JSX (a Javascript extension created by Facebook for adding XML syntax to JavaScript), which allows to write

```js
var rElement = React.createElement('h1', null, 'Hello, world!');

```

as the equivalent (and easier to read for someone familiar with HTML)

```js
var rElement = <h1>Hello, world!</h1>;

```

The code containing JSX needs to be enclosed in a `<script type="text/babel">` tag. Everything within this tag will be transformed to plain Javascript using the Babel library (that needs to be included in addition to the React libraries).

So finally the above example becomes:

```js
<!DOCTYPE html>
<html>
  <head>
    <meta charset="UTF-8" />
    <title>Hello React!</title>

    <!-- Include the React and ReactDOM libraries -->
    <script src="https://fb.me/react-15.2.1.js"></script>
    <script src="https://fb.me/react-dom-15.2.1.js"></script>
    <!-- Include the Babel library -->
    <script src="https://npmcdn.com/babel-core@5.8.38/browser.min.js"></script>

  </head>
  <body>
   <div id="example"></div>

    <script type="text/babel">

      // create a React element rElement using JSX
      var rElement = <h1>Hello, world!</h1>;

      // dElement is a DOM container
      var dElement = document.getElementById('example');

      // render the React element in the DOM container
      ReactDOM.render(rElement, dElement);

    </script>

  </body>
</html>

```



## Hello World Component


A React component can be defined as an ES6 class that extends the base `React.Component` class. In its minimal form, a component **must** define a `render` method that specifies how the component renders to the DOM. The `render` method returns React nodes, which can be defined using JSX syntax as HTML-like tags. The following example shows how to define a minimal Component:

```js
import React from 'react'

class HelloWorld extends React.Component {
    render() {
        return <h1>Hello, World!</h1>
    }
}

export default HelloWorld

```

A Component can also receive `props`. These are properties passed by its parent in order to specify some values the component cannot know by itself; a property can also contain a function that can be called by the component after certain events occur - for example, a button could receive a function for its `onClick` property and call it whenever it is clicked. When writing a component, its `props` can be accessed through the `props` object on the Component itself:

```js
import React from 'react'

class Hello extends React.Component {
    render() {
        return <h1>Hello, {this.props.name}!</h1>
    }
}

export default Hello

```

The example above shows how the component can render an arbitrary string passed into the `name` prop by its parent. Note that a component cannot modify the props it receives.

A component can be rendered within any other component, or directly into the DOM if it's the topmost component, using `ReactDOM.render` and providing it with both the component and the DOM Node where you want the React tree to be rendered:

```js
import React from 'react'
import ReactDOM from 'react-dom'
import Hello from './Hello'

ReactDOM.render(<Hello name="Billy James" />, document.getElementById('main'))

```

By now you know how to make a basic component and accept `props`. Lets take this a step further and introduce `state`.

For demo sake, let's make our Hello World app, display only the first name if a full name is given.

```js
import React from 'react'

class Hello extends React.Component {

    constructor(props){

        //Since we are extending the default constructor,
        //handle default activities first.
        super(props);

        //Extract the first-name from the prop
        let firstName = this.props.name.split(" ")[0];
        
        //In the constructor, feel free to modify the
        //state property on the current context.
        this.state = {
            name: firstName
        }

    } //Look maa, no comma required in JSX based class defs!

    render() {
        return <h1>Hello, {this.state.name}!</h1>
    }
}

export default Hello

```

**Note:** Each component can have it's own state or accept it's parent's state as a prop.

[Codepen Link to Example.](https://codepen.io/sunnykgupta/pen/mRPxdo?editors=1010)



#### Remarks


[React](https://facebook.github.io/react/index.html) is a declarative, component-based JavaScript library used for creating user interfaces.

To achieve MVC framework like functionalities in React, developers use it in conjunction with a [Flux](https://facebook.github.io/flux/) flavour of choice, e.g. [Redux](http://redux.js.org/).

