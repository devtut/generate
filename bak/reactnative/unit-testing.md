---
metaTitle: "React Native - Unit Testing"
description: "Unit Test In React Native Using Jest, Unit testing with jest"
---

# Unit Testing




## Unit Test In React Native Using Jest


Starting from react-native version 0.38, a Jest setup is included by default when running react-native init. The following configuration should be automatically added to your package.json file:

```

   "scripts": {
    "start": "node node_modules/react-native/local-cli/cli.js start",
    "test": "jest"
    },
    "jest": {
     "preset": "react-native"
    }

```

You can run `run npm test or jest` to test in react native.
For code example: [Link](https://github.com/facebook/jest/tree/master/examples/react-native)



## Unit testing with jest


`Jest` is a javascript testing framework widely used for testing react applications. Its supported by facebook

Here's a test

```js
import 'react-native';
import React from 'react';
import Index from '../index.android.js';

import renderer from 'react-test-renderer';

it('renders correctly', () => {
  const tree = renderer.create(
    <Index />
  );
});

```

Here's some code to make it pass

```js
import React, { Component } from 'react';
import {
  AppRegistry,
  StyleSheet,
  Text,
  View
} from 'react-native';

export default class gol extends Component {
  render() {
    return (
      <View>
        <Text>
          Welcome to React Native!
        </Text>
        <Text>
          To get started, edit index.android.js
        </Text>
        <Text>
          Double tap R on your keyboard to reload,{'\n'}
          Shake or press menu button for dev menu
        </Text>
      </View>
    );
  }
}

AppRegistry.registerComponent('gol', () => gol);

```

