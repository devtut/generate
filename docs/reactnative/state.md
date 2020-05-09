---
metaTitle: "React Native - State"
description: "setState, Initialize State"
---

# State




## setState


To change view in your application you can use `setState` - this will re-render your component and any of its child components.
setState performs a shallow merge between the new and previous state, and triggers a re-render of the component.

`setState` takes either a key-value object or a function that returns a key-value object

**Key-Value Object**

```js
this.setState({myKey: 'myValue'});

```

**Function**

Using a function is useful for updating a value based off the existing state or props.

```js
this.setState((previousState, currentProps) => {
    return {
        myInteger: previousState.myInteger+1
    }
})

```

You can also pass an optional callback to `setState` that will be fired when the component has re-rendered with the new state.

```js
this.setState({myKey: 'myValue'}, () => {
    // Component has re-rendered... do something amazing!
));

```

### Full Example

```js
import React, { Component } from 'react';
import { AppRegistry, StyleSheet, Text, View, TouchableOpacity } from 'react-native';

export default class MyParentComponent extends Component {
  constructor(props) {
    super(props);

    this.state = {
      myInteger: 0
    }

  }
  getRandomInteger() {
    const randomInt = Math.floor(Math.random()*100);

    this.setState({
      myInteger: randomInt
    });

  }
  incrementInteger() {
    
    this.setState((previousState, currentProps) => {
      return {
        myInteger: previousState.myInteger+1
      }
    });

  }
  render() {

    return <View style={styles.container}>
      
      <Text>Parent Component Integer: {this.state.myInteger}</Text>

      <MyChildComponent myInteger={this.state.myInteger} />

      <Button label="Get Random Integer" onPress={this.getRandomInteger.bind(this)} />
      <Button label="Increment Integer" onPress={this.incrementInteger.bind(this)} />

    </View>

  }
}

export default class MyChildComponent extends Component {
  constructor(props) {
    super(props);
  }
  render() {
    
    // this will get updated when "MyParentComponent" state changes
    return <View>
      <Text>Child Component Integer: {this.props.myInteger}</Text>
    </View>
    
  }
}

export default class Button extends Component {
  constructor(props) {
    super(props);
  }
  render() {

    return <TouchableOpacity onPress={this.props.onPress}>
        <View style={styles.button}>
          <Text style={styles.buttonText}>{this.props.label}</Text>
        </View>
      </TouchableOpacity>
    
  }
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
    backgroundColor: '#F5FCFF',
  },
  button: {
    backgroundColor: '#444',
    padding: 10, 
    marginTop: 10
  },
  buttonText: {
    color: '#fff'
  }
});

AppRegistry.registerComponent('MyApp', () => MyParentComponent);

```



## Initialize State


You should initialize state inside the constructor function of your component like this:

```js
export default class MyComponent extends Component {
  constructor(props) {
    super(props);
    
    this.state = {
      myInteger: 0
    }
  }
  render() {
    return  (
      <View>
        <Text>Integer: {this.state.myInteger}</Text>
      </View>
    )
  }
}

```

Using setState one can update the view.



#### Syntax


<li>void setState(
function|object nextState,
[function callback]
)</li>

