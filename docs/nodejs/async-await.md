---
metaTitle: "Async/Await"
description: "Async Functions with Try-Catch Error Handling, Comparison between Promises and Async/Await, Progression from Callbacks, Stops execution at await"
---

# Async/Await


Async/await is a set of keywords that allows writing of asynchronous code in a procedural manner without having to rely on callbacks (**callback hell**) or promise-chaining (`.then().then().then()`).

This works by using the `await` keyword to suspend the state of an async function, until the resolution of a promise, and using the `async` keyword to declare such async functions, which return a promise.

Async/await is available from node.js 8 by default or 7 using the flag `--harmony-async-await`.



## Async Functions with Try-Catch Error Handling


One of the best features of async/await syntax is that standard try-catch coding style is possible, just like you were writing synchronous code.

```js
const myFunc = async (req, res) => {
  try {
    const result = await somePromise();
  } catch (err) {
    // handle errors here
  }
});

```

Here's an example with Express and promise-mysql:

```js
router.get('/flags/:id', async (req, res) => {

  try {

    const connection = await pool.createConnection();

    try {
      const sql = `SELECT f.id, f.width, f.height, f.code, f.filename
                   FROM flags f
                   WHERE f.id = ?
                   LIMIT 1`;
      const flags = await connection.query(sql, req.params.id);
      if (flags.length === 0)
        return res.status(404).send({ message: 'flag not found' });

      return res.send({ flags[0] });

    } finally {
      pool.releaseConnection(connection);
    }

  } catch (err) {
    // handle errors here
  }
});

```



## Comparison between Promises and Async/Await


Function using promises:

```js
function myAsyncFunction() {
    return aFunctionThatReturnsAPromise()
           // doSomething is a sync function
           .then(result => doSomething(result))
           .catch(handleError);
}

```

So here is when Async/Await enter in action in order to get cleaner our function:

```js
async function myAsyncFunction() {
  let result;

  try {
      result = await aFunctionThatReturnsAPromise();
  } catch (error) {
      handleError(error);
  }

  // doSomething is a sync function
  return doSomething(result);
}

```

So the keyword `async` would be similar to write `return new Promise((resolve, reject) => {...}`.

And `await` similar to get your result in `then` callback.

Here I leave a pretty brief gif that will not left any doubt in mind after seeing it:

[GIF](https://twitter.com/manekinekko/status/855824609299636230)



## Progression from Callbacks


In the beginning there were callbacks, and callbacks were ok:

```js
const getTemperature = (callback) => {
  http.get('www.temperature.com/current', (res) => {
    callback(res.data.temperature)
  })
}

const getAirPollution = (callback) => {
  http.get('www.pollution.com/current', (res) => {
    callback(res.data.pollution)
  });
}

getTemperature(function(temp) {
  getAirPollution(function(pollution) {
    console.log(`the temp is ${temp} and the pollution is ${pollution}.`)
    // The temp is 27 and the pollution is 0.5.
  })
})

```

But there were a few [really frustrating](http://callbackhell.com) issues with callbacks so we all started using promises.

```js
const getTemperature = () => {
  return new Promise((resolve, reject) => {
    http.get('www.temperature.com/current', (res) => {
      resolve(res.data.temperature)
    })
  })
}

const getAirPollution = () => {
  return new Promise((resolve, reject) => {
    http.get('www.pollution.com/current', (res) => {
      resolve(res.data.pollution)
    })
  })
}

getTemperature()
.then(temp => console.log(`the temp is ${temp}`))
.then(() => getAirPollution())
.then(pollution => console.log(`and the pollution is ${pollution}`))
// the temp is 32
// and the pollution is 0.5

```

This was a bit better. Finally, we found async/await. Which still uses promises under the hood.

```js
const temp = await getTemperature()
const pollution = await getAirPollution()

```



## Stops execution at await


If the promise doesn't return anything, the async task can be completed using `await`.

```js
try{
    await User.findByIdAndUpdate(user._id, {
        $push: {
            tokens: token
        }
    }).exec()
}catch(e){
    handleError(e)
}

```

