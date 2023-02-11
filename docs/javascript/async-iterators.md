---
metaTitle: "JavaScript - Async Iterators"
description: "Basics"
---

# Async Iterators


An `async` function is one that returns a promise. `await` yields to the caller until the promise resolves and then continues with the result.

An iterator allows the collection to be looped through with a `for-of` loop.

An async iterator is a collection where each iteration is a promise which can be awaited using a `for-await-of` loop.

Async iterators are a [stage 3 proposal](https://github.com/tc39/proposal-async-iteration). They are in Chrome Canary 60 with `--harmony-async-iteration`



## Basics


A JavaScript `Iterator` is an object with a `.next()` method, which returns an `IteratorItem`, which is an object with `value : <any>` and `done : <boolean>`.

A JavaScript `AsyncIterator` is an object with a `.next()` method, which returns a `Promise<IteratorItem>`, a **promise** for the next value.

To create an AsyncIterator, we can use the **async generator** syntax:

```js
/**
 * Returns a promise which resolves after time had passed.
 */
const delay = time => new Promise(resolve => setTimeout(resolve, time));

async function* delayedRange(max) {
  for (let i = 0; i < max; i++) {
    await delay(1000);
    yield i;
  }
}

```

The `delayedRange` function will take a maximum number, and returns an `AsyncIterator`, which yields numbers from 0 to that number, in 1 second intervals.

Usage:

```js
for await (let number of delayedRange(10)) {
  console.log(number);
}

```

The `for await of` loop is another piece of new syntax, available only inside of async functions, as well as async generators. Inside the loop, the values yielded (which, remember, are Promises) are unwrapped, so the Promise is hidden away. Within the loop, you can deal with the direct values (the yielded numbers), the `for await of` loop will wait for the Promises on your behalf.

The above example will wait 1 second, log 0, wait another second, log 1, and so on, until it logs 9. At which point the `AsyncIterator` will be `done`, and the `for await of` loop will exit.



#### Syntax


- async function* asyncGenerator() {}
- yield await asyncOperationWhichReturnsAPromise();
- for await (let result of asyncGenerator()) { /* result is the resolved value from the promise */ }



#### Remarks


An async iterator is a **declarative pull stream** as opposed to an Observable's declarative **push** stream.

### Useful Links

- [Async Iteration spec proposal](https://github.com/tc39/proposal-async-iteration)
- [Introduction to their use](https://jakearchibald.com/2017/async-iterators-and-generators/)
- [Event subscription proof of concept](https://github.com/KeithHenry/event-generator)

