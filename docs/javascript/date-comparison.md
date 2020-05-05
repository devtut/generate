---
metaTitle: "JavsScript - Date Comparison"
description: "Comparing Date values, Date Difference Calculation"
---

# Date Comparison




## Comparing Date values


To check the equality of `Date` values:

```js
var date1 = new Date();
var date2 = new Date(date1.valueOf() + 10);
console.log(date1.valueOf() === date2.valueOf());

```

> 
Sample output: `false`


Note that you must use `valueOf()` or `getTime()` to compare the values of `Date` objects because the equality operator will compare if two object references are the same. For example:

```js
var date1 = new Date();
var date2 = new Date();
console.log(date1 === date2);

```

> 
Sample output: `false`


Whereas if the variables point to the same object:

```js
var date1 = new Date();
var date2 = date1;
console.log(date1 === date2);

```

> 
Sample output: `true`


However, the other comparison operators will work as usual and you can use `<` and `>` to compare that one date is earlier or later than the other. For example:

```js
var date1 = new Date();
var date2 = new Date(date1.valueOf() + 10);
console.log(date1 < date2);

```

> 
Sample output: `true`


It works even if the operator includes equality:

```js
var date1 = new Date();
var date2 = new Date(date1.valueOf());
console.log(date1 <= date2);

```

> 
Sample output: `true`




## Date Difference Calculation


To compare the difference of two dates, we can do the comparison based on the timestamp.

```js
var date1 = new Date();
var date2 = new Date(date1.valueOf() + 5000);

var dateDiff = date1.valueOf() - date2.valueOf();
var dateDiffInYears = dateDiff/1000/60/60/24/365; //convert milliseconds into years

console.log("Date difference in years : " + dateDiffInYears);

```

