---
metaTitle: "React Native - Navigator Best Practices"
description: "Navigator, Use react-navigation for navigation in react native apps, react-native Navigation with react-native-router-flux"
---

# Navigator Best Practices



## Navigator


`Navigator` is React Native's default navigator.  A `Navigator` component manages a **stack** of route objects, and provides methods for managing that stack.

**Managing the Route Stack**

First of all, notice the `initialRoute` prop.  A route is simply a javascript object, and can take whatever shape you want, and have whatever values you want.  It's the primary way you'll pass values and methods between components in your navigation stack.

The `Navigator` knows what to render based on the value returned from its `renderScene` prop.

Let's imagine an implementation of `ExampleScene` in this example:

**Configuring the Navigator**

You can configure the `Navigator`'s transitions with the `configureScene` prop.  This is a function that's passed the `route` object, and needs to return a configuration object. These are the available configuration objects:

- Navigator.SceneConfigs.PushFromRight (default)
- Navigator.SceneConfigs.FloatFromRight
- Navigator.SceneConfigs.FloatFromLeft
- Navigator.SceneConfigs.FloatFromBottom
- Navigator.SceneConfigs.FloatFromBottomAndroid
- Navigator.SceneConfigs.FadeAndroid
- Navigator.SceneConfigs.HorizontalSwipeJump
- Navigator.SceneConfigs.HorizontalSwipeJumpFromRight
- Navigator.SceneConfigs.VerticalUpSwipeJump
- Navigator.SceneConfigs.VerticalDownSwipeJump

You can return one of these objects without modification, or you can modify the configuration object to customize the navigation transitions.  For example, to modify the edge hit width to more closely emulate the iOS `UINavigationController`'s `interactivePopGestureRecognizer`:

**Managing the NavigationBar**

The `Navigator` component comes with a `navigationBar` prop, which can theoretically take any properly configured React component.  But the most common implementation uses the default `Navigator.NavigationBar`.  This takes a `routeMapper` prop that you can use to configure the appearance of the navigation bar based on the route.

A `routeMapper` is a regular javascript object with three functions: `Title`, `RightButton`, and `LeftButton`.  For example:

**See more**

For more detailed documentation of each prop, see the [the official React Native Documentation for `Navigator`](https://facebook.github.io/react-native/docs/navigator.html), and the React Native guide on [Using Navigators](https://facebook.github.io/react-native/docs/using-navigators.html).



## Use react-navigation for navigation in react native apps


With the help of [react-navigation](https://reactnavigation.org/), you can add navigation to your app really easy.

Install react-navigation

```js
npm install --save react-navigation
```

Example:

```js
import { Button, View, Text, AppRegistry } from 'react-native';
import { StackNavigator } from 'react-navigation';

const App = StackNavigator({
  FirstPage: {screen: FirstPage},
  SecondPage: {screen: SecondPage},
});

class FirstPage extends React.Component {
  static navigationOptions = {
    title: 'Welcome',
  };
  render() {
    const { navigate } = this.props.navigation;

    return (
      <Button
        title='Go to Second Page'
        onPress={() =>
          navigate('SecondPage', { name: 'Awesomepankaj' })
        }
      />
    );
  }
}

class SecondPage extends React.Component {
  static navigationOptions = ({navigation}) => ({
    title: navigation.state.params.name,
  });

  render() {
    const { goBack } = this.props.navigation;
    return (
      <View>
        <Text>Welcome to Second Page</Text>
        <Button
          title="Go back to First Page"
          onPress={() => goBack()}
        />
      </View>
    );
  }
}

```



## react-native Navigation with react-native-router-flux


Install by using `npm install --save react-native-router-flux`

In react-native-router-flux, each route is called a `<Scene>`

> 
`<Scene key="home" component={LogIn} title="Home" initial />`


`key` A unique string that can be used to refer to the particular scene.

`component` Which component to show, here it's 

`title` make a NavBar and give it a title 'Home'

`initial` Is this the first screen of the App

> 
Example:


```js
import React from 'react';
import { Scene, Router } from 'react-native-router-flux';
import LogIn from './components/LogIn';
import SecondPage from './components/SecondPage';

const RouterComponent = () => {
  return (
    <Router>
      <Scene key="login" component={LogIn} title="Login Form" initial />
      <Scene key="secondPage" component={SecondPage} title="Home" />
    </Router>
  );
};

export default RouterComponent;

```

Import this file in the main App.js(index file) and render it.
For more information can visit this [link](https://github.com/aksonov/react-native-router-flux/blob/master/docs/MINI_TUTORIAL.md).

