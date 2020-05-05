---
metaTitle: "Algorithm - Catalan Number Algorithm"
description: "Catalan Number Algorithm Basic Information, C# Implementation"
---

# Catalan Number Algorithm



## Catalan Number Algorithm Basic Information


Catalan numbers algorithm is Dynamic Programming algorithm.

In combinatorial mathematics, the [Catalan numbers](https://en.wikipedia.org/wiki/Catalan_number) form a sequence of natural numbers that occur in various counting problems, often involving recursively-defined objects. The Catalan numbers on nonnegative integers n are a set of numbers that arise in tree enumeration problems of the type, 'In how many ways can a regular n-gon be divided into n-2 triangles if different orientations are counted separately?'

**Application of Catalan Number Algorithm:**

1. The number of ways to stack coins on a bottom row that consists of n consecutive coins in a plane, such that no coins are allowed to be put on the two sides of the bottom coins and every additional coin must be above two other coins, is the nth Catalan number.
1. The number of ways to group a string of n pairs of parentheses, such that each open parenthesis has a matching closed parenthesis, is the nth Catalan number.
1. The number of ways to cut an n+2-sided convex polygon in a plane into triangles by connecting vertices with straight, non-intersecting lines is the nth Catalan number. This is the application in which Euler was interested.

Using zero-based numbering, the **n**th Catalan number is given directly in terms of binomial coefficients by the following equation.

[<img src="https://i.stack.imgur.com/UP8N4.png" alt="Catalan Number Equation" />](https://i.stack.imgur.com/UP8N4.png)

**Example of Catalan Number:**

Here value of n = 4.(Best Example - From Wikipedia)

[<img src="https://i.stack.imgur.com/VBGLB.png" alt="Catalan Number Example" />](https://i.stack.imgur.com/VBGLB.png)

**Auxiliary Space:** `O(n)`<br>
**Time Complexity:** `O(n^2)`



## C# Implementation


```cpp
public class CatalanNumber
{
    public static int Main(int number)
    {
        int result = 0;
        if (number <= 1) return 1;
        for (int i = 0; i < number; i++)
        {
            result += Main(i)*Main(number - i - 1);
        }
        return result;
    }
}

```

