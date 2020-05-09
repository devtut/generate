---
metaTitle: "ReactJS - Using ReactJS with Typescript"
description: "ReactJS component written in Typescript, Installation and Setup, Stateless React Components in Typescript, Stateless and property-less Components"
---

# Using ReactJS with Typescript



## ReactJS component written in Typescript


Actually you can use ReactJS's components in Typescript as in facebook's example. Just replace 'jsx' file's extension to 'tsx':

```js
//helloMessage.tsx:
var HelloMessage = React.createClass({
  render: function() {
    return <div>Hello {this.props.name}</div>;
  }
});
ReactDOM.render(<HelloMessage name="John" />, mountNode);

```

But in order to make full use of Typescript's main feature (static type checking) should be done couple things:

**1) convert React.createClass example to ES6 Class:**

```js
//helloMessage.tsx:
class HelloMessage extends React.Component {
  render() {
    return <div>Hello {this.props.name}</div>;
  }
}
ReactDOM.render(<HelloMessage name="John" />, mountNode);

```

**2) next add Props and State interfaces:**

```js
interface IHelloMessageProps {
    name:string;
}

interface IHelloMessageState {
  //empty in our case
}

class HelloMessage extends React.Component<IHelloMessageProps, IHelloMessageState> {
  constructor(){
    super();
  }  
  render() {
    return <div>Hello {this.props.name}</div>;
  }
}
ReactDOM.render(<HelloMessage name="Sebastian" />, mountNode);

```

Now Typescript will display an error if the programmer forgets to pass props. Or if they added props that are not defined in the interface.



## Installation and Setup


To use typescript with react in a node project, you must first have a project directory initialized with npm. To initialize the directory with `npm init`

**Installing via npm or yarn**

You can install React using [npm](https://www.npmjs.com/) by doing the following:

```js
npm install --save react react-dom

```

Facebook released its own package manager named [Yarn](https://yarnpkg.com/), which can also be used to install React. After installing Yarn you just need to run this command:

```js
yarn add react react-dom

```

You can then use React in your project in exactly the same way as if you had installed React via npm.

**Installing react type definitions in Typescript 2.0+**

To compile your code using typescript, add/install type definition files using npm or yarn.

```js
npm install --save-dev @types/react @types/react-dom

```

or, using yarn

```js
yarn add --dev @types/react @types/react-dom

```

**Installing react type definitions in older versions of Typescript**

You have to use a separate package called [tsd](https://www.npmjs.com/package/tsd)

```js
tsd install react react-dom --save

```

**Adding or Changing the Typescript configuration**

To use [JSX](https://www.typescriptlang.org/docs/handbook/jsx.html), a language mixing javascript with html/xml, you have to change the typescript compiler configuration. In the project's typescript configuration file (usually named `tsconfig.json`), you will need to add the JSX option as:

```js
"compilerOptions": {
    "jsx": "preserve"
},

```



## Stateless React Components in Typescript


React components that are pure functions of their props and do not require any internal state can be written as JavaScript functions instead of using the standard class syntax, as:

The same can be achieved in Typescript using the `React.SFC` class:

Note that, the name `React.SFC` is an alias for `React.StatelessComponent` So, either can be used.



## Stateless and property-less Components


The simplest react component without a state and no properties can be written as:

```js
import * as React from 'react';

const Greeter = () => <span>Hello, World!</span>

```

That component, however, can't access `this.props` since typescript can't tell if it is a react component. To access its props, use:

```js
import * as React from 'react';

const Greeter: React.SFC<{}> = props => () => <span>Hello, World!</span>

```

Even if the component doesn't have explicitly defined properties, it can now access **`props.children`** since all components inherently have children.

Another similar good use of stateless and property-less components is in simple page templating. The following is an examplinary simple `Page` component, assuming there are hypothetical `Container`, `NavTop` and `NavBottom` components already in the project:

```js
import * as React from 'react';

const Page: React.SFC<{}> = props => () => 
    <Container>
        <NavTop />
        {props.children}
        <NavBottom />
    </Container>

const LoginPage: React.SFC<{}> = props => () =>
    <Page>
        Login Pass: <input type="password" />
    </Page>

```

In this example, the `Page` component can later be used by any other actual page as a base template.

