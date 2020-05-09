---
metaTitle: "ReactJS - React with Redux"
description: "Using Connect"
---

# React with Redux


Redux has come to be the status quo for managing application-level state on the front-end these days, and those who work on "large-scale applications" often swear by it. This topic covers why and how you should use the state management library, Redux, in your React applications.



## Using Connect


Create a Redux store with **createStore**.

```js
import { createStore } from 'redux'
import todoApp from './reducers'
let store = createStore(todoApp, { inistialStateVariable: "derp"})

```

Use **connect** to connect component to Redux store and pull props from store to component.

```js
import { connect } from 'react-redux'

const VisibleTodoList = connect(
  mapStateToProps,
  mapDispatchToProps
)(TodoList)

export default VisibleTodoList

```

Define actions that allow your components to send messages to the Redux store.

```js
/*
 * action types
 */

export const ADD_TODO = 'ADD_TODO'

export function addTodo(text) {
  return { type: ADD_TODO, text }
}

```

Handle these messages and create a new state for the store in reducer functions.

```js
function todoApp(state = initialState, action) {
  switch (action.type) {
    case SET_VISIBILITY_FILTER:
      return Object.assign({}, state, {
        visibilityFilter: action.filter
      })
    default:
      return state
  }
}

```



#### Remarks


While React's component driven architecture is fantastic for breaking down the application into modular, encapsulated little pieces, it introduces some challenges for managing the state of the application as a whole. The time to use Redux is when you need to display the same data across more than one component or page (aka route). At that point you can no longer store the data in variables local to one component or the other, and sending messages between components quickly becomes a mess. With Redux your components are all subscribing to the same shared data in the store and thus the state can be easily reflected consistently across the entire application.

