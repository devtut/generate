---
metaTitle: "React Native - Routing"
description: "Navigator component"
---

# Routing




## Navigator component


Navigator works for both IOS and android.

```js
import React, { Component } from 'react';
import { Text, Navigator, TouchableHighlight } from 'react-native';

export default class NavAllDay extends Component {
  render() {
    return (
      <Navigator
        initialRoute={{ title: 'Awesome Scene', index: 0 }}
        renderScene={(route, navigator) =>
          <Text>Hello {route.title}!</Text>
        }
        style={{padding: 100}}
      />
    );
  }
}

```

Routes to `Navigator` are provided as objects. You also provide a `renderScene` function that renders the scene for each route object. `initialRoute` is used to specify the first route.

