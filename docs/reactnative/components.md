---
metaTitle: "React Native - Components"
description: "Basic Component, Stateful Component, Stateless Component"
---

# Components




## Basic Component


```js
import React, { Component } from 'react'
import { View, Text, AppRegistry } from 'react-native'

class Example extends Component {
  render () {
    return (
      <View> 
        <Text> I'm a basic Component </Text>
      </View>
    )
  }
}

AppRegistry.registerComponent('Example', () => Example)

```



## Stateful Component


These components will have changing States.

```js
import React, { Component } from 'react'
import { View, Text, AppRegistry } from 'react-native'

class Example extends Component {
  constructor (props) {
    super(props)
    this.state = {
      name: "Sriraman"
    }  
  }
  render () {
    return (
      <View>
        <Text> Hi, {this.state.name}</Text>
      </View>
    )
  }
}

AppRegistry.registerComponent('Example', () => Example)

```



## Stateless Component


As the name implies, Stateless Components do not have any local state. They are also known as **Dumb Components**. Without any local state, these components do not need lifecycle methods or much of the boilerplate that comes with a stateful component.

Class syntax is not required, you can simply do `const name = ({props}) => ( ... )`. Generally stateless components are more concise as a result.

Beneath is an example of two stateless components `App` and `Title`, with a demonstration of passing props between components:

```js
import React from 'react'
import { View, Text, AppRegistry } from 'react-native'

const Title = ({Message}) => (
  <Text>{Message}</Text>
)    

const App = () => (
  <View>
    <Title title='Example Stateless Component' />
  </View>
)

AppRegistry.registerComponent('App', () => App)

```

This is the recommended pattern for components, when possible. As in the future optimisations can be made for these components, reducing memory allocations and unnecessary checks.

