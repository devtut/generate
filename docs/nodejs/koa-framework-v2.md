---
metaTitle: "Koa Framework v2"
description: "Hello World example, Handling errors using middleware"
---

# Koa Framework v2



## Hello World example


```js
const Koa = require('koa')

const app = new Koa()

app.use(async ctx => {
   ctx.body = 'Hello World'
})

app.listen(8080)

```



## Handling errors using middleware


```js
app.use(async (ctx, next) => {
  try {
    await next() // attempt to invoke the next middleware downstream
  } catch (err) {
    handleError(err, ctx) // define your own error handling function
  }
})

```

