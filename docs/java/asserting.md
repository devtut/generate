---
metaTitle: "Asserting"
description: "Checking arithmetic with assert"
---

# Asserting



## Checking arithmetic with assert


```java
a = 1 - Math.abs(1 - a % 2);

// This will throw an error if my arithmetic above is wrong.
assert a >= 0 && a <= 1 : "Calculated value of " + a + " is outside of expected bounds";

return a;

```



#### Syntax


- assert **expression1**;
- assert **expression1** : **expression2**;



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|expression1|The assertion statement throws an `AssertionError` if this expression evaluates to `false`.
|expression2|Optional. When used, `AssertionError`s thrown by the assert statement have this message.



#### Remarks


By default, assertions are disabled at runtime.

To enable assertions, you must run java with `-ea` flag.

```java
java -ea com.example.AssertionExample

```

Assertions are statements that will throw an error if their expression evaluates to `false`. Assertions should only be used to **test** code; they should never be used in production.

