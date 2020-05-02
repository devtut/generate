---
metaTitle: "Cycle Sort"
description: "Pseudocode Implementation, Cycle Sort Basic Information, C# Implementation"
---

# Cycle Sort



## Pseudocode Implementation


```cpp
(input)
output = 0
for cycleStart from 0 to length(array) - 2
    item = array[cycleStart]
    pos = cycleStart
    for i from cycleStart + 1 to length(array) - 1
        if array[i] < item:
            pos += 1
    if pos == cycleStart:
        continue
    while item == array[pos]:
        pos += 1
    array[pos], item = item, array[pos]
    writes += 1
    while pos != cycleStart:
        pos = cycleStart
        for i from cycleStart + 1 to length(array) - 1
            if array[i] < item:
                pos += 1
        while item == array[pos]:
            pos += 1
        array[pos], item = item, array[pos]
        writes += 1
return outout

```



## Cycle Sort Basic Information


[Cycle Sort](https://en.wikipedia.org/wiki/Cycle_sort) is sorting algorithm which uses [comparison sort](https://en.wikipedia.org/wiki/Comparison_sort) that is theoretically optimal in terms of the total number of writes to original array, unlike any other in-place sorting algorithm. Cycle sort is unstable sorting algorithm. It is based on idea of permutation in which permutations are factored into cycles, which individually rotate and return a sorted output.

**Example of Cycle Sort:**

[<img src="http://i.stack.imgur.com/KvtRX.gif" alt="Cycle Sort" />](http://i.stack.imgur.com/KvtRX.gif)

**Auxiliary Space:** `O(1)`<br>
**Time Complexity:** `O(n^2)`



## C# Implementation


```cpp
public class CycleSort
{
    public static void SortCycle(int[] input)
    {
        for (var i = 0; i < input.Length; i++)
        {
            var item = input[i];
            var position = i;
            do
            {
                var k = input.Where((t, j) => position != j && t < item).Count();
                if (position == k) continue;
                while (position != k && item == input[k])
                {
                    k++;
                }
                var temp = input[k];
                input[k] = item;
                item = temp;
                position = k;
            } while (position != i);
        }
    }

    public static int[] Main(int[] input)
    {
        SortCycle(input);
        return input;
    }
}

```

