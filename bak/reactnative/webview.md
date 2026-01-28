---
metaTitle: "React Native - WebView"
description: "Simple component using webview"
---

# WebView


Webview can be used to load external webpages or html content. This component is there by default.



## Simple component using webview


```js
import React, { Component } from 'react';
import { WebView } from 'react-native';

class MyWeb extends Component {
  render() {
    return (
      <WebView
        source={{uri: 'https://github.com/facebook/react-native'}}
        style={{marginTop: 20}}
       />
    );
  }
}

```

