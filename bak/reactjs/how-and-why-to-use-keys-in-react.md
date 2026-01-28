---
metaTitle: "ReactJS - How and why to use keys in React"
description: "Basic Example"
---

# How and why to use keys in React


Whenever you are rendering a list of React components, each component needs to have a `key` attribute. The key can be any value, but it does need to be unique to that list.

When React has to render changes on a list of items, React just iterates over both lists of children at the same time and generates a mutation whenever there's a difference. If there are no keys set for the children, React scans each child. Otherwise, React compares the keys to know which were added or removed from the list



## Basic Example


For a class-less React component:

```js
function SomeComponent(props){

    const ITEMS = ['cat', 'dog', 'rat']
    function getItemsList(){
        return ITEMS.map(item => <li key={item}>{item}</i>);
    }
    
    return (
        <ul>
            {getItemsList()}
        </ul>
    );
}

```

For this example, the above component resolves to:

```js
<ul>
    <li key='cat'>cat</li>
    <li key='dog'>dog</li>
    <li key='rat'>rat</li>
<ul>

```



#### Remarks


For more information, visit this link to read how to use keys:
[https://facebook.github.io/react/docs/lists-and-keys.html](https://facebook.github.io/react/docs/lists-and-keys.html)

And visit this link to read why it is recommended to use keys: [https://facebook.github.io/react/docs/reconciliation.html#recursing-on-children](https://facebook.github.io/react/docs/reconciliation.html#recursing-on-children)

