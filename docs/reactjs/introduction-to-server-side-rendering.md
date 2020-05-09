---
metaTitle: "ReactJS - Introduction to Server-Side Rendering"
description: "Rendering components"
---

# Introduction to Server-Side Rendering



## Rendering components


There are two options to render components on server: `renderToString` and `renderToStaticMarkup`.

### renderToString

This will render React components to HTML on server. This function will also add `data-react-` properties to HTML elements so React on client won't have to render elements again.

```js
import { renderToString } from "react-dom/server";
renderToString(<App />);

```

### renderToStaticMarkup

This will render React components to HTML, but without `data-react-` properties, it is not recommended to use components that will be rendered on client, because components will rerender.

```js
import { renderToStaticMarkup } from "react-dom/server";
renderToStaticMarkup(<App />);

```

