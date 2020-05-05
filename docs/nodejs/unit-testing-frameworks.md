---
metaTitle: "Node.js - Unit testing frameworks"
description: "Mocha Asynchronous (async/await), Mocha synchronous, Mocha asynchronous (callback), Mocha asynchronous (Promise)"
---

# Unit testing frameworks



## Mocha Asynchronous (async/await)


```js
const { expect } = require('chai')

describe('Suite Name', function() {
  describe('#method()', function() {
    it('should run without an error', async function() {
      const result = await answerToTheUltimateQuestion()
      expect(result).to.be.equal(42)
    })
  })
})

```



## Mocha synchronous


```js
describe('Suite Name', function() {
  describe('#method()', function() {
    it('should run without an error', function() {
      expect([ 1, 2, 3 ].length).to.be.equal(3)
    })
  })
})

```



## Mocha asynchronous (callback)


```js
var expect = require("chai").expect;
describe('Suite Name', function() {
  describe('#method()', function() {
    it('should run without an error', function(done) {
      testSomething(err => {
        expect(err).to.not.be.equal(null)
        done()
      })
    })
  })
})

```



## Mocha asynchronous (Promise)


```js
describe('Suite Name', function() {
  describe('#method()', function() {
    it('should run without an error', function() {
      return doSomething().then(result => {
         expect(result).to.be.equal('hello world')
      })
    })
  })
})

```

