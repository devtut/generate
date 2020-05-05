---
metaTitle: "Algorithm - Pigeonhole Sort"
description: "Pigeonhole Sort Basic Information, C# Implementation"
---

# Pigeonhole Sort



## Pigeonhole Sort Basic Information


[Pigeonhole Sort](https://en.wikipedia.org/wiki/Pigeonhole_sort) is a sorting algorithm that is suitable for sorting lists of elements where the number of elements (n) and the number of possible key values (N) are approximately the same. It requires O(n + Range) time where n is number of elements in input array and ‘Range’ is number of possible values in array.

**Working(Pseudo code) for Pigeonhole Sort:**

1. Find minimum and maximum values in array. Let the minimum and maximum values be ‘min’ and ‘max’ respectively. Also find range as ‘max-min-1′.
1. Set up an array of initially empty “pigeonholes” the same size as of the range.
1. Visit each element of the array and then put each element in its pigeonhole. An element input[i] is put in hole at index input[i] – min.
1. Start the loop all over the pigeonhole array in order and put the elements from non- empty holes back into the original array.

Pigeonhole sort is similar to counting sort, so here is a comparison between Pigeonhole Sort and counting sort.

[<img src="http://i.stack.imgur.com/37SDY.jpg" alt="Comparison between Pigeonhole Sort and Counting Sort" />](http://i.stack.imgur.com/37SDY.jpg)

**Example of Pigeonhole Sort:**

[<img src="http://i.stack.imgur.com/VKhzI.jpg" alt="Example of Pigeonhole Sort" />](http://i.stack.imgur.com/VKhzI.jpg)

**Space Auxiliary:** `O(n)` <br>
**Time Complexity:** `O(n + N)`



## C# Implementation


```cpp
public class PigeonholeSort
{
    private static void SortPigeonhole(int[] input, int n)
    {
        int min = input[0], max = input[n];
        for (int i = 1; i < n; i++)
        {
            if (input[i] < min) min = input[i];
            if (input[i] > max) max = input[i];
        }
        int range = max - min + 1;
        int[] holes = new int[range];

        for (int i = 0; i < n; i++)
        {
            holes[input[i] - min] = input[i];
        }
        int index = 0;

        for (int i = 0; i < range; i++)
        {
            foreach (var value in holes)
            {
                input[index++] = value;
            }
        }
    }

    public static int[] Main(int[] input)
    {
        SortPigeonhole(input, input.Length);
        return input;
    }
}

```

