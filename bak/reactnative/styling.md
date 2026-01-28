---
metaTitle: "React Native - Styling"
description: "Styling using inline styles, Styling using a stylesheet, Adding multiple styles, Conditional Styling"
---

# Styling




## Styling using inline styles


Each React Native component can take a `style` prop. You can pass it a JavaScript object with CSS-style style properties:

```js
<Text style={{color:'red'}}>Red text</Text>

```

This can be inefficient as it has to recreate the object each time the component is rendered. Using a stylesheet is preferred.



## Styling using a stylesheet


```js
import React, { Component } from 'react';
import { View, Text, StyleSheet } from 'react-native';

const styles = StyleSheet.create({
    red: {
        color: 'red'
    },
    big: {
        fontSize: 30
    }
});

class Example extends Component {
    render() {
        return (
            <View>
                <Text style={styles.red}>Red</Text>
                <Text style={styles.big}>Big</Text>
            </View>
        );
    }
}

```

`StyleSheet.create()` returns an object where the values are numbers. React Native knows to convert these numeric IDs into the correct style object.



## Adding multiple styles


You can pass an array to the `style` prop to apply multiple styles. When there is a conflict, the last one in the list takes precedence.

```js
import React, { Component } from 'react';
import { View, Text, StyleSheet } from 'react-native';

const styles = StyleSheet.create({
    red: {
        color: 'red'
    },
    greenUnderline: {
        color: 'green',
        textDecoration: 'underline'
    },
    big: {
        fontSize: 30
    }
});

class Example extends Component {
    render() {
        return (
            <View>
                <Text style={[styles.red, styles.big]}>Big red</Text>
                <Text style={[styles.red, styles.greenUnderline]}>Green underline</Text>
                <Text style={[styles.greenUnderline, styles.red]}>Red underline</Text>
                <Text style={[styles.greenUnderline, styles.red, styles.big]}>Big red underline</Text>
                <Text style={[styles.big, {color:'yellow'}]}>Big yellow</Text>
            </View>
        );
    }
}

```



## Conditional Styling


```js
<View style={[(this.props.isTrue) ? styles.bgcolorBlack : styles.bgColorWhite]}>

```

If the value of `isTrue` is `true` then it will have black background color otherwise white.



#### Syntax


- `<Component style={styleFromStyleSheet} />`
- `<Component style={styleObject} />`
- `<Component style={[style1,style2]} />`



#### Remarks


Most React Native styles are their CSS forms, but in camel case. So, `text-decoration` becomes `textDecoration`.

Unlike in CSS, styles do not get inherited. If you want child components to inherit a certain style, you must explicitly provide it to the child. This means that you cannot set a font family for an entire `View`.<br />
The one exception to this is the `Text` component: nested `Text`s inherit their parent styles.

