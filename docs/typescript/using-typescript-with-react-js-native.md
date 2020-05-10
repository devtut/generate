---
metaTitle: "TypeScript - Using Typescript with React (JS & native)"
description: "ReactJS component written in Typescript, Typescript & react & webpack"
---

# Using Typescript with React (JS & native)



## ReactJS component written in Typescript


You can use ReactJS's components easily in TypeScript. Just rename the 'jsx' file extension to 'tsx':

```ts
//helloMessage.tsx:
var HelloMessage = React.createClass({
  render: function() {
    return <div>Hello {this.props.name}</div>;
  }
});

ReactDOM.render(<HelloMessage name="John" />, mountNode);

```

But in order to make full use of TypeScript's main feature (static type checking) you must do a couple things:

**1) convert React.createClass to an ES6 Class:**

```ts
//helloMessage.tsx:
class HelloMessage extends React.Component {
  render() {
    return <div>Hello {this.props.name}</div>;
  }
}

ReactDOM.render(<HelloMessage name="John" />, mountNode);

```

For more info on converting to ES6 look [here](http://www.newmediacampaigns.com/blog/refactoring-react-components-to-es6-classes)

**2) Add Props and State interfaces:**

```ts
interface Props {
    name:string;
    optionalParam?:number;
}

interface State {
  //empty in our case
}

class HelloMessage extends React.Component<Props, State> {
  render() {
    return <div>Hello {this.props.name}</div>;
  }
}
// TypeScript will allow you to create without the optional parameter
ReactDOM.render(<HelloMessage name="Sebastian" />, mountNode);
// But it does check if you pass in an optional parameter of the wrong type
ReactDOM.render(<HelloMessage name="Sebastian" optionalParam='foo' />, mountNode);

```

Now TypeScript will display an error if the programmer forgets to pass props. Or if trying to pass in props that are not defined in the interface.



## Typescript & react & webpack


Installing typescript, typings and webpack globally

`npm install -g typescript typings webpack`

Installing loaders and linking typescript

`npm install --save-dev ts-loader source-map-loader npm link typescript`

> 
Linking TypeScript allows ts-loader to use your global installation of TypeScript instead of needing a separate local copy [typescript doc](https://www.typescriptlang.org/docs/handbook/react-&-webpack.html)


installing `.d.ts` files with typescript 2.x

```ts
npm i @types/react --save-dev
npm i @types/react-dom --save-dev

```

installing `.d.ts` files with typescript 1.x

```ts
typings install --global --save dt~react
typings install --global --save dt~react-dom

```

`tsconfig.json` configuration file

```ts
{
  "compilerOptions": {
    "sourceMap": true,
    "noImplicitAny": true,
    "module": "commonjs",
    "target": "es5",
    "jsx": "react"
  }
}

```

`webpack.config.js` configuration file

```ts
module.exports = {
    entry: "<path to entry point>",// for example ./src/helloMessage.tsx
    output: {
        filename: "<path to bundle file>", // for example ./dist/bundle.js
    },

    // Enable sourcemaps for debugging webpack's output.
    devtool: "source-map",

    resolve: {
        // Add '.ts' and '.tsx' as resolvable extensions.
        extensions: ["", ".webpack.js", ".web.js", ".ts", ".tsx", ".js"]
    },

    module: {
        loaders: [
            // All files with a '.ts' or '.tsx' extension will be handled by 'ts-loader'.
            {test: /\.tsx?$/, loader: "ts-loader"}
        ],

        preLoaders: [
            // All output '.js' files will have any sourcemaps re-processed by 'source-map-loader'.
            {test: /\.js$/, loader: "source-map-loader"}
        ]
    },

    // When importing a module whose path matches one of the following, just
    // assume a corresponding global variable exists and use that instead.
    // This is important because it allows us to avoid bundling all of our
    // dependencies, which allows browsers to cache those libraries between builds.
    externals: {
        "react": "React",
        "react-dom": "ReactDOM"
    },
};

```

finally run `webpack` or `webpack -w` (for watch mode)

**Note**:
React and ReactDOM are marked as external

