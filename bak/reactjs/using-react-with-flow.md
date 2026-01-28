---
metaTitle: "ReactJS - Using React with Flow"
description: "Using Flow to check prop types of stateless functional components, Using Flow to check prop types"
---

# Using React with Flow


How to use the [Flow type checker](https://flow.org/) to check types in React components.



## Using Flow to check prop types of stateless functional components


```js
type Props = {
  posts: Array<Article>,
  dispatch: Function,
  children: ReactElement
}

const AppContainer =
  ({ posts, dispatch, children }: Props) => (
    <div className="main-app">
      <Header {...{ posts, dispatch }} />
      {children}
    </div>
  )

```



## Using Flow to check prop types


```js
import React, { Component } from 'react';

type Props = {
  posts: Array<Article>,
  dispatch: Function,
  children: ReactElement
}

class Posts extends Component {
  props: Props;

  render () {
    // rest of the code goes here
  }
}

```



#### Remarks


[Flow | React](https://flowtype.org/docs/react.html)

