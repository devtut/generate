---
metaTitle: "Python - Python speed of program"
description: "Deque operations, Algorithmic Notations..., Notation, List operations, Set operations"
---

# Python speed of program




## Deque operations


A deque is a double-ended queue.

```py
class Deque:
def __init__(self):
    self.items = []

def isEmpty(self):
    return self.items == []

def addFront(self, item):
    self.items.append(item)

def addRear(self, item):
    self.items.insert(0,item)

def removeFront(self):
    return self.items.pop()

def removeRear(self):
    return self.items.pop(0)

def size(self):
    return len(self.items)

```

**Operations : Average Case (assumes parameters are randomly generated)**

Append : O(1)

Appendleft : O(1)

Copy : O(n)

Extend : O(k)

Extendleft : O(k)

Pop : O(1)

Popleft : O(1)

Remove : O(n)

Rotate : O(k)



## Algorithmic Notations...


There are certain principles that apply to optimization in any computer language, and Python is no exception.
**Don't optimize as you go**:
Write your program without regard to possible optimizations, concentrating instead on making sure that the code is clean, correct, and understandable. If it's too big or too slow when you've finished, then you can consider optimizing it.

**Remember the 80/20 rule**:
In many fields you can get 80% of the result with 20% of the effort (also called the 90/10 rule - it depends on who you talk to). Whenever you're about to optimize code, use profiling to find out where that 80% of execution time is going, so you know where to concentrate your effort.

**Always run "before" and "after" benchmarks**:
How else will you know that your optimizations actually made a difference? If your optimized code turns out to be only slightly faster or smaller than the original version, undo your changes and go back to the original, clear code.

Use the right algorithms and data structures:
Don't use an O(n2) bubble sort algorithm to sort a thousand elements when there's an O(n log n) quicksort available. Similarly, don't store a thousand items in an array that requires an O(n) search when you could use an O(log n) binary tree, or an O(1) Python hash table.

For more visit the link below...
[Python Speed Up](https://wiki.python.org/moin/PythonSpeed/PerformanceTips)

The following 3 asymptotic notations are mostly used to represent time complexity of algorithms.

<li>
<p>**Θ Notation**:
The theta notation bounds a functions from above and below, so it defines exact asymptotic behavior.
A simple way to get Theta notation of an expression is to drop low order terms and ignore leading constants. For example, consider the following expression.
3n3 + 6n2 + 6000 = Θ(n3)
Dropping lower order terms is always fine because there will always be a n0 after which Θ(n3) has higher values than Θn2) irrespective of the constants involved.
For a given function g(n), we denote Θ(g(n)) is following set of functions.
Θ(g(n)) = {f(n): there exist positive constants c1, c2 and n0 such
that 0 <= c1**g(n) <= f(n) <= c2**g(n) for all n >= n0}
The above definition means, if f(n) is theta of g(n), then the value f(n) is always between c1**g(n) and c2**g(n) for large values of n (n >= n0). The definition of theta also requires that f(n) must be non-negative for values of n greater than n0.</p>
</li>
<li>
<p>**Big O Notation**: The Big O notation defines an upper bound of an algorithm, it bounds a function only from above. For example, consider the case of Insertion Sort. It takes linear time in best case and quadratic time in worst case. We can safely say that the time complexity of Insertion sort is O(n^2). Note that O(n^2) also covers linear time.
If we use Θ notation to represent time complexity of Insertion sort, we have to use two statements for best and worst cases:</p>
</li>

1. The worst case time complexity of Insertion Sort is Θ(n^2).
1. The best case time complexity of Insertion Sort is Θ(n).

The Big O notation is useful when we only have upper bound on time complexity of an algorithm. Many times we easily find an upper bound by simply looking at the algorithm.
O(g(n)) = { f(n): there exist positive constants c and
n0 such that 0 <= f(n) <= cg(n) for
all n >= n0}

<li>**Ω Notation**: Just as Big O notation provides an asymptotic upper bound on a function, Ω notation provides an asymptotic lower bound.
Ω Notation< can be useful when we have lower bound on time complexity of an algorithm. As discussed in the previous post, the best case performance of an algorithm is generally not useful, the Omega notation is the least used notation among all three.
For a given function g(n), we denote by Ω(g(n)) the set of functions.
Ω (g(n)) = {f(n): there exist positive constants c and
n0 such that 0 <= cg(n) <= f(n) for
all n >= n0}.
Let us consider the same Insertion sort example here. The time complexity of Insertion Sort can be written as Ω(n), but it is not a very useful information about insertion sort, as we are generally interested in worst case and sometimes in average case.</li>



## Notation


**Basic Idea**

The notation used when describing the speed of your Python program is called Big-O notation. Let's say you have a function:

```py
def list_check(to_check, the_list):
    for item in the_list:
        if to_check == item:
          return True
    return False

```

This is a simple function to check if an item is in a list. To describe the complexity of this function, you will say O(n). This means "Order of n" as the O function is known as the Order function.

O(n) - generally n is the number of items in container

O(k) - generally k is the value of the parameter or the number of elements in the parameter



## List operations


**Operations : Average Case (assumes parameters are randomly generated)**

Append : O(1)

Copy : O(n)

Del slice : O(n)

Delete item : O(n)

Insert : O(n)

Get item : O(1)

Set item : O(1)

Iteration : O(n)

Get slice : O(k)

Set slice : O(n + k)

Extend : O(k)

Sort : O(n log n)

Multiply : O(nk)

x in s : O(n)

min(s), max(s) :O(n)

Get length : O(1)



## Set operations


**Operation : Average Case (assumes parameters generated randomly) : Worst case**

x in s : O(1)

Difference s - t : O(len(s))

Intersection s&t : O(min(len(s), len(t))) : O(len(s) * len(t)

Multiple intersection s1&s2&s3&...&sn : : (n-1) * O(l) where l is max(len(s1),...,len(sn))

s.difference_update(t) : O(len(t)) : O(len(t) * len(s))

s.symetric_difference_update(t) : O(len(t))

Symetric difference s^t : O(len(s)) : O(len(s) * len(t))

Union s|t : O(len(s) + len(t))

