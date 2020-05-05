---
metaTitle: "JavsScript - Loops"
description: "Standard for loops, for ... of loop, for ... in loop, while Loops, continue a loop, do ... while loop, Break specific nested loops, Break and continue labels, Break out of a loop"
---

# Loops



## Standard "for" loops


### Standard usage

```js
for (var i = 0; i < 100; i++) {
    console.log(i);
}

```

Expected output:

> 
<p>0<br />
1<br />
...<br />
99</p>


### Multiple declarations

Commonly used to cache the length of an array.

```js
var array = ['a', 'b', 'c'];
for (var i = 0; i < array.length; i++) {
    console.log(array[i]);
}

```

Expected output:

> 
<p>'a'<br />
'b'<br />
'c'</p>


### Changing the increment

```js
for (var i = 0; i < 100; i += 2 /* Can also be: i = i + 2 */) {
    console.log(i);
}

```

Expected output:

> 
<p>0<br />
2<br />
4<br />
...<br />
98</p>


### Decremented loop

```js
for (var i = 100; i >=0; i--) {
    console.log(i);
}

```

Expected output:

> 
<p>100<br />
99<br />
98<br />
...<br />
0</p>




## "for ... of" loop


```js
const iterable = [0, 1, 2];
for (let i of iterable) {
    console.log(i);
}

```

Expected output:

> 
<p>0<br />
1<br />
2</p>


The advantages from the for...of loop are:

- This is the most concise, direct syntax yet for looping through array elements
- It avoids all the pitfalls of for...in
- Unlike `forEach()`, it works with break, continue, and return

### Support of for...of in other collections

### Strings

for...of will treat a string as a sequence of Unicode characters:

```js
const string = "abc";
for (let chr of string) {
  console.log(chr);
}

```

Expected output:

> 
<p>a
b
c</p>


### Sets

for...of works on [Set objects](http://stackoverflow.com/documentation/javascript/2854/set#t=201704141052476866786).

**Note**:

- A Set object will eliminate duplicates.
- Please [check this reference](https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Global_Objects/Set#Browser_compatibility) for `Set()` browser support.

```js
const names = ['bob', 'alejandro', 'zandra', 'anna', 'bob'];

const uniqueNames = new Set(names);

for (let name of uniqueNames) {
  console.log(name);
}

```

Expected output:

> 
<p>bob<br />
alejandro<br />
zandra<br />
anna</p>


### Maps

You can also use for...of loops to iterate over [Map](https://stackoverflow.com/documentation/javascript/1648/map)s. This works similarly to arrays and sets, except the iteration variable stores both a key and a value.

```js
const map = new Map()
  .set('abc', 1)
  .set('def', 2)

for (const iteration of map) {
  console.log(iteration) //will log ['abc', 1] and then ['def', 2]
}

```

You can use [destructuring assignment](https://stackoverflow.com/documentation/javascript/616/destructuring-assignment/4969/destructuring-arrays) to capture the key and the value separately:

```js
const map = new Map()
  .set('abc', 1)
  .set('def', 2)

for (const [key, value] of map) {
  console.log(key + ' is mapped to ' + value)
}
/*Logs:
  abc is mapped to 1
  def is mapped to 2
*/

```

### Objects

for...of loops **do not** work directly on plain Objects; but, it is possible to iterate over an object’s properties by switching to a for...in loop, or using [`Object.keys()`](http://stackoverflow.com/documentation/javascript/188/objects/736/object-keys#t=201608201852433521361):

```js
const someObject = { name: 'Mike' };

for (let key of Object.keys(someObject)) {
  console.log(key + ": " + someObject[key]);
}

```

Expected output:

> 
name: Mike




## "for ... in" loop


> 
<p>**Warning**<br/>
for...in is intended for iterating over object keys, not array indexes. [Using it to loop through an array is generally discouraged](http://stackoverflow.com/questions/500504/why-is-using-for-in-with-array-iteration-such-a-bad-idea). It also includes properties from the prototype, so it may be necessary to check if the key is within the object using `hasOwnProperty`. If any attributes in the object are defined by the `defineProperty/defineProperties` method and set the param `enumerable: false`, those attributes will be inaccessible.</p>


```js
var object = {"a":"foo", "b":"bar", "c":"baz"};
// `a` is inaccessible
Object.defineProperty(object , 'a', {
        enumerable: false,
});
for (var key in object) {
    if (object.hasOwnProperty(key)) {
      console.log('object.' + key + ', ' + object[key]);
    }
}

```

Expected output:

> 
<p>object.b, bar<br />
object.c, baz</p>




## "while" Loops


### **Standard While Loop**

A standard while loop will execute until the condition given is false:

```js
var i = 0;
while (i < 100) {
    console.log(i);
    i++;
}

```

Expected output:

> 
<p>0<br />
1<br />
...<br />
99</p>


### Decremented loop

```js
var i = 100;
while (i > 0) {
    console.log(i);
    i--; /* equivalent to i=i-1 */
}

```

Expected output:

> 
<p>100<br />
99<br />
98<br />
...<br />
1</p>


### **Do...while Loop**

A do...while loop will always execute at least once, regardless of whether the condition is true or false:

```js
var i = 101;
do {
    console.log(i);
} while (i < 100);

```

Expected output:

> 
101




## "continue" a loop


### Continuing a "for" Loop

When you put the `continue` keyword in a for loop, execution jumps to the update expression (`i++` in the example):

```js
for (var i = 0; i < 3; i++) {
    if (i === 1) {
        continue;
    }
    console.log(i);
}

```

Expected output:

> 
<p>0<br />
2</p>


### Continuing a While Loop

When you `continue` in a while loop, execution jumps to the condition (`i < 3` in the example):

```js
var i = 0;
while (i < 3) {
    if (i === 1) {
        i = 2;
        continue;
    }
    console.log(i);
    i++;
}

```

Expected output:

> 
<p>0<br />
2</p>




## "do ... while" loop


```js
var availableName;
do {
    availableName = getRandomName();
} while (isNameUsed(name));

```

A `do while` loop is guaranteed to run at least once as it's condition is only checked at the end of an iteration. A traditional `while` loop may run zero or more times as its condition is checked at the beginning of an iteration.



## Break specific nested loops


We can name our loops and break the specific one when necessary.

```js
outerloop:
for (var i = 0;i<3;i++){
    innerloup:
    for (var j = 0;j <3; j++){
        console.log(i);
        console.log(j);
        if (j == 1){
            break outerloop;    
        }
    }
}

```

Output:

```js
0
0
0
1

```



## Break and continue labels


Break and continue statements can be followed by an optional label which works like some kind of a goto statement, resumes execution from the label referenced position

```js
for(var i = 0; i < 5; i++){
  nextLoop2Iteration:
  for(var j = 0; j < 5; j++){
    if(i == j) break nextLoop2Iteration;
    console.log(i, j);
  }
}

```

> 
<p>****i=0 j=0 skips rest of j values****<br />
1 0<br />
****i=1 j=1 skips rest of j values****<br />
2 0<br />
2 1
****i=2 j=2 skips rest of j values****<br />
3 0<br />
3 1<br />
3 2<br />
****i=3 j=3 skips rest of j values****<br />
4 0<br />
4 1<br />
4 2<br />
4 3<br />
****i=4 j=4 does not log and loops are done****</p>




## "Break" out of a loop


### Breaking out of a while loop

```js
var i = 0;
while(true) {
    i++;
    if(i === 42) {
        break;
    }
}
console.log(i);

```

Expected output:

> 
42


### Breaking out of a for loop

```js
var i;
for(i = 0; i < 100; i++) {
    if(i === 42) {
        break;
    }
}
console.log(i);

```

Expected output:

> 
42




#### Syntax


- for (**initialization**; **condition**; **final_expression**) { }
- for (**key** in **object**) { }
- for (**variable** of **iterable**) { }
- while (**condition**) { }
- do { } while (**condition**)
- for each (**variable** in **object**) { } // ECMAScript for XML



#### Remarks


Loops in JavaScript typically help solve problems which involve repeating specific code **x** amount of times. Say you need to log a message 5 times. You could do this:

```js
console.log("a message");
console.log("a message");
console.log("a message");
console.log("a message");
console.log("a message");

```

But that's just time-consuming and kind of ridiculous. Plus, what if you needed to log over 300 messages? You should replace the code with a traditional "for" loop:

```js
for(var i = 0; i < 5; i++){
    console.log("a message");
}

```

