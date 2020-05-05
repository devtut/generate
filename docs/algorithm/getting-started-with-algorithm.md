---
metaTitle: "Algorithm - Getting started with algorithm"
description: "A sample algorithmic problem, Getting Started with Simple Fizz Buzz Algorithm in Swift"
---

# Getting started with algorithm



## A sample algorithmic problem


An algorithmic problem is specified by describing the complete set of **instances** it must work on and of its output after running on one of these instances. This distinction, between a problem and an instance of a problem, is fundamental. The algorithmic **problem** known as **sorting** is defined as follows: [Skiena:2008:ADM:1410219]

- Problem: Sorting
- Input: A sequence of **n** keys, `a_1, a_2, ..., a_n`.
- Output: The reordering of the input sequence such that `a'_1 <= a'_2 <= ... <= a'_{n-1} <= a'_n`

An **instance** of sorting might be an array of strings, such as `{ Haskell, Emacs }` or a sequence of numbers such as `{ 154, 245, 1337 }`.



## Getting Started with Simple Fizz Buzz Algorithm in Swift


For those of you that are new to programming in Swift and those of you coming from different programming bases, such as Python or Java, this article should be quite helpful. In this post, we will discuss a simple solution for implementing swift algorithms.

**Fizz Buzz**

You may have seen Fizz Buzz written as Fizz Buzz, FizzBuzz, or Fizz-Buzz; they're all referring to the same thing. That "thing" is the main topic of discussion today. First, what is FizzBuzz?

This is a common question that comes up in job interviews.

Imagine a series of a number from 1 to 10.

```cpp
1 2 3 4 5 6 7 8 9 10

```

Fizz and Buzz refer to any number that's a multiple of 3 and 5 respectively. In other words, if a number is divisible by 3, it is substituted with fizz; if a number is divisible by 5, it is substituted with buzz. If a number is simultaneously a multiple of 3 AND 5, the number is replaced with "fizz buzz." In essence, it emulates the famous children game "fizz buzz".

To work on this problem, open up Xcode to create a new playground and initialize an array like below:

```cpp
// for example 
let number  = [1,2,3,4,5]
// here 3 is fizz and 5 is buzz

```

To find all the fizz and buzz, we must iterate through the array and check which numbers are fizz and which are buzz. To do this, create a for loop to iterate through the array we have initialised:

```cpp
for num in number {
  // Body and calculation goes here
}

```

After this, we can simply use the "if else" condition and module operator in swift ie - % to locate the fizz and buzz

```cpp
for num in number {
  if num % 3 == 0 {
    print("\(num) fizz")
  } else {
    print(num)
  }
}

```

Great! You can go to the debug console in Xcode playground to see the output. You will find that the "fizzes" have been sorted out in your array.

For the Buzz part, we will use the same technique. Let's give it a try before scrolling through the article — you can check your results against this article once you've finished doing this.

```cpp
for num in number {
  if num % 3 == 0 {
    print("\(num) fizz")
  } else if num % 5 == 0 {
    print("\(num) buzz")
  } else {
    print(num)
  }
}

```

Check the output!

It's rather straight forward — you divided the number by 3, fizz and divided the number by 5, buzz. Now, increase the numbers in the array

```cpp
let number = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]

```

We increased the range of numbers from 1-10 to 1-15 in order to demonstrate the concept of a "fizz buzz." Since 15 is a multiple of both 3 and 5, the number should be replaced with "fizz buzz." Try for yourself and check the answer!

Here is the solution:

```cpp
for num in number {
  if num % 3 == 0 && num % 5 == 0 {
    print("\(num) fizz buzz")
  } else if num % 3 == 0 {
    print("\(num) fizz")
  } else if num % 5 == 0 {
    print("\(num) buzz")
  } else {
    print(num)
  }
}

```

Wait...it's not over though! The whole purpose of the algorithm is to customize the runtime correctly. Imagine if the range increases from 1-15 to 1-100. The compiler will check each number to determine whether it is divisible by 3 or 5. It would then run through the numbers again to check if the numbers are divisible by 3 and 5. The code would essentially have to run through each number in the array twice — it would have to runs the numbers by 3 first and then run it by 5. To speed up the process, we can simply tell our code to divide the numbers by 15 directly.

Here is the final code:

```cpp
for num in number {
  if num % 15 == 0 {
    print("\(num) fizz buzz")
  } else if num % 3 == 0 {
    print("\(num) fizz")
  } else if num % 5 == 0 {
    print("\(num) buzz")
  } else {
    print(num)
  }
}

```

As Simple as that, you can use any language of your choice and get started

Enjoy Coding



#### Remarks


### Introduction to Algorithms

Algorithms are ubiquitous in Computer Science and Software Engineering. Selection of appropriate algorithms and data structures improves our program efficiency in cost and time.

What is an algorithm? Informally, an algorithm is a procedure to accomplish a specific task. [ Skiena:2008:ADM:1410219] Specifically, an algorithm is a **well-defined** computational procedure, which takes some value (or set of values) as **input** and produces some value, or a set of values, as **output**. An algorithm is thus a sequence of computational steps that transform the input into the output. Cormen et. al. does not explicitly remark that an algorithm does not necessarily require an input. [Cormen:2001:IA:580470]

Formally, an algorithm must satisfy five features: [Knuth:1997:ACP:260999]

1. **Finiteness**. An algorithm must always terminate after a finite number of steps.
1. **Definiteness**. Each step of an algorithm must be precisely defined; the actions to be carried out must be rigorously specified. It is this quality that [Cormen:2001:IA:580470] refers to with the term "well-defined".
1. **Input**. An algorithm has zero or more **inputs**. These are quantities that are given to the algorithm initially before it begins or dynamically as it runs.
1. **Output**. An algorithm has one or more **outputs**. These are quantities that have a specified relation to the inputs. We expect that an algorithm produces the same output when given the same input over and over again.
1. **Effectiveness**. An algorithm is also generally expected to be **effective**. Its operations must be sufficiently basic that they can be done exactly in principle and in a finite length of time by someone using pencil and paper.

A procedure that lacks finiteness but satisfies all other characteristics of an algorithm may be called a **computational method**. [Knuth:1997:ACP:260999]

