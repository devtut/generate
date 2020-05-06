---
metaTitle: "TypeScript - Unit Testing"
description: "tape, jest (ts-jest), Alsatian, chai-immutable plugin"
---

# Unit Testing




## tape


[tape](https://github.com/substack/tape) is minimalistic JavaScript testing framework, it outputs [TAP-compliant](https://testanything.org/) markup.

To install `tape` using `npm` run command

```js
npm install --save-dev tape @types/tape

```

To use `tape` with Typescript you need to install `ts-node` as global package, to do this run command

```js
npm install -g ts-node

```

Now you are ready to write your first test

```js
//math.test.ts
import * as test from "tape";

test("Math test", (t) => {
    t.equal(4, 2 + 2);
    t.true(5 > 2 + 2);

    t.end();
});

```

To execute test run command

```js
ts-node node_modules/tape/bin/tape math.test.ts

```

In output you should see

```js
TAP version 13
# Math test
ok 1 should be equal
ok 2 should be truthy

1..2
# tests 2
# pass  2

# ok

```

Good job, you just ran your TypeScript test.

**Run multiple test files**

You can run multiple test files at once using path wildcards.
To execute all Typescript tests in `tests` directory run command

```js
ts-node node_modules/tape/bin/tape tests/**/*.ts

```



## jest (ts-jest)


[jest](https://facebook.github.io/jest/) is painless JavaScript testing framework by Facebook, with [ts-jest](https://www.npmjs.com/package/ts-jest) can be used to test TypeScript code.

To install jest using npm run command

```js
npm install --save-dev jest @types/jest ts-jest typescript

```

For ease of use install `jest` as global package

```js
npm install -g jest

```

To make `jest` work with TypeScript you need to add configuration to `package.json`

```js
//package.json
{
...
"jest": {
    "transform": {
      ".(ts|tsx)": "<rootDir>/node_modules/ts-jest/preprocessor.js"
    },
    "testRegex": "(/__tests__/.*|\\.(test|spec))\\.(ts|tsx|js)$",
    "moduleFileExtensions": ["ts", "tsx", "js"]
  }
}

```

Now `jest` is ready.
Assume we have sample fizz buz to test

```js
//fizzBuzz.ts
export function fizzBuzz(n: number): string {
    let output = "";
    for (let i = 1; i <= n; i++) {
        if (i % 5 && i % 3) {
            output += i + ' ';
        }
        if (i % 3 === 0) {
            output += 'Fizz ';
        }
        if (i % 5 === 0) {
            output += 'Buzz ';
        }
    }
    return output;
}

```

Example test could look like

```js
//FizzBuzz.test.ts
/// <reference types="jest" />

import {fizzBuzz} from "./fizzBuzz";
test("FizzBuzz test", () =>{
    expect(fizzBuzz(2)).toBe("1 2 ");
    expect(fizzBuzz(3)).toBe("1 2 Fizz ");
});

```

To execute test run

```js
jest

```

In output you should see

```

PASS  ./fizzBuzz.test.ts
  ✓ FizzBuzz test (3ms)

Test Suites: 1 passed, 1 total
Tests:       1 passed, 1 total
Snapshots:   0 total
Time:        1.46s, estimated 2s
Ran all test suites.

```

### Code coverage

`jest` supports generation of code coverage reports.

To use code coverage with TypeScript you need to add another configuration line to `package.json`.

```js
{
...
  "jest": {
  ...
    "testResultsProcessor": "<rootDir>/node_modules/ts-jest/coverageprocessor.js"
  }
}

```

To run tests with generation of coverage report run

```js
jest --coverage

```

If used with our sample fizz buzz you should see

```

PASS  ./fizzBuzz.test.ts
  ✓ FizzBuzz test (3ms)

-------------|---|---|---|---|---|---|---|---|---|-------|---|---|---|---|---|---|---|---|---|-------|---|---|---|---|---|---|---|---|---|-------|---|---|---|---|---|---|---|---|---|-------|---|---|---|---|---|---|---|---|---|-------------|
File         |  % Stmts | % Branch |  % Funcs |  % Lines |Uncovered Lines |
-------------|---|---|---|---|---|---|---|---|---|-------|---|---|---|---|---|---|---|---|---|-------|---|---|---|---|---|---|---|---|---|-------|---|---|---|---|---|---|---|---|---|-------|---|---|---|---|---|---|---|---|---|-------------|
All files    |    92.31 |     87.5 |      100 |    91.67 |                |
 fizzBuzz.ts |    92.31 |     87.5 |      100 |    91.67 |             13 |
-------------|---|---|---|---|---|---|---|---|---|-------|---|---|---|---|---|---|---|---|---|-------|---|---|---|---|---|---|---|---|---|-------|---|---|---|---|---|---|---|---|---|-------|---|---|---|---|---|---|---|---|---|-------------|
Test Suites: 1 passed, 1 total
Tests:       1 passed, 1 total
Snapshots:   0 total
Time:        1.857s
Ran all test suites.

```

`jest` also created folder `coverage` which contains coverage report in various formats, including user friendly html report in `coverage/lcov-report/index.html`

[<img src="https://i.stack.imgur.com/PtNG7.png" alt="jest html report" />](https://i.stack.imgur.com/PtNG7.png)



## Alsatian


[Alsatian](https://github.com/alsatian-test/alsatian) is a unit testing framework written in TypeScript. It allows for usage of Test Cases, and outputs [TAP-compliant](https://testanything.org/) markup.

To use it, install it from `npm`:

```js
npm install alsatian --save-dev

```

Then set up a test file:

```js
import { Expect, Test, TestCase } from "alsatian";
import { SomeModule } from "../src/some-module";    

export SomeModuleTests {

    @Test()
    public statusShouldBeTrueByDefault() {
        let instance = new SomeModule();
        
        Expect(instance.status).toBe(true);
    }
    
    @Test("Name should be null by default")
    public nameShouldBeNullByDefault() {
        let instance = new SomeModule();
        
        Expect(instance.name).toBe(null);
    }
    
    @TestCase("first name")
    @TestCase("apples")
    public shouldSetNameCorrectly(name: string) {
        let instance = new SomeModule();
        
        instance.setName(name);
        
        Expect(instance.name).toBe(name);
    }
    
}

```

For a full documentation, see [alsatian's GitHub repo](https://github.com/alsatian-test/alsatian).



## chai-immutable plugin


<li>
Install from npm chai, chai-immutable, and ts-node

```js
npm install --save-dev chai chai-immutable ts-node

```


</li>

<li>
Install types for mocha and chai

```js
npm install --save-dev @types/mocha @types/chai

```


</li>
<li>
Write simple test file:

```js
  import {List, Set} from 'immutable';
  import * as chai from 'chai';
  import * as chaiImmutable from 'chai-immutable';

  chai.use(chaiImmutable);

  describe('chai immutable example', () => {
    it('example', () => {
      expect(Set.of(1,2,3)).to.not.be.empty;

      expect(Set.of(1,2,3)).to.include(2);
      expect(Set.of(1,2,3)).to.include(5);
    })
  })

```


</li>
<li>
Run it in the console:

```js
mocha --compilers ts:ts-node/register,tsx:ts-node/register 'test/**/*.spec.@(ts|tsx)'

```


</li>

