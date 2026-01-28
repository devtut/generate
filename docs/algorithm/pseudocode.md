---
title: "Algorithm - Pseudocode"
description: "Variable affectations, Functions"
date: 2026-01-27
tags: ["algorithm"]
---

# Pseudocode

## Variable affectations

You could describe variable affectation in different ways.

### Typed

```text
int a = 1
int a := 1
let int a = 1
int a <- 1
```

### No type

```text
a = 1
a := 1
let a = 1
a <- 1
```

## Functions

As long as the function name, return statement and parameters are clear, you're fine.

```text
def incr n
    return n + 1
```

or

```text
let incr(n) = n + 1
```

or

```text
function incr (n)
    return n + 1
```

are all quite clear, so you may use them. Try not to be ambiguous with a variable affectation

#### Remarks

Pseudocode is by definition informal. This topic is meant to describe ways to translate language-specific code into something everyone with a programming background can understand.

Pseudocode is an important way to describe an algorithm and is more neutral than giving a langugage-specific implementation. Wikipedia often uses some form of pseudocode when describing an algorithm

Some things, like if-else type conditions are quite easy to write down informally. But other things, js-style callbacks for instance, may be hard to turn into pseudocode for some people.

This is why these examples may prove useful

