---
metaTitle: "Algorithm - Counting Sort"
description: "Counting Sort Basic Information, Psuedocode Implementation, C# Implementation"
---

# Counting Sort



## Counting Sort Basic Information


[Counting sort](https://en.wikipedia.org/wiki/Counting_sort) is an integer sorting algorithm for a collection of objects that sorts according to the keys of the objects.

**Steps**

1. Construct a working array **C** that has size equal to the range of the input array **A**.
1. Iterate through **A**, assigning **C**[x] based on the number of times x appeared in **A**.
1. Transform **C** into an array where **C**[x] refers to the number of values ≤ x by iterating through the array, assigning to each **C**[x] the sum of its prior value and all values in **C** that come before it.
1. Iterate backwards through **A**, placing each value in to a new sorted array **B** at the index recorded in **C**. This is done for a given **A**[x] by assigning **B**[**C**[**A**[x]]] to **A**[x], and decrementing **C**[**A**[x]] in case there were duplicate values in the original unsorted array.

**Example of Counting Sort**

[<img src="http://i.stack.imgur.com/ccdTK.jpg" alt="Counting Sort" />](http://i.stack.imgur.com/ccdTK.jpg)

**Auxiliary Space:** `O(n+k)` <br>
**Time Complexity:** Worst-case: `O(n+k)`, Best-case: `O(n)`, Average-case `O(n+k)`



## Psuedocode Implementation


Constraints:

1. Input (an array to be sorted)
1. Number of element in input (n)
1. Keys in the range of **0..k-1** (k)
1. Count (an array of number)

Pseudocode:

```cpp
for x in input:
    count[key(x)] += 1
total = 0
for i in range(k):
    oldCount = count[i]
    count[i] = total
    total += oldCount
for x in input:
    output[count[key(x)]] = x
    count[key(x)] += 1
return output

```



## C# Implementation


```cpp
public class CountingSort
{
    public static void SortCounting(int[] input, int min, int max)
    {
        var count = new int[max - min + 1];
        var z = 0;

        for (var i = 0; i < count.Length; i++)
            count[i] = 0;

        foreach (int i in input)
            count[i - min]++;

        for (var i = min; i <= max; i++)
        {
            while (count[i - min]-- > 0)
            {
                input[z] = i;
                ++z;
            }
        }
    }

    public static int[] Main(int[] input)
    {
        SortCounting(input, input.Min(), input.Max());
        return input;
    }
}

```

