---
metaTitle: "Sliding Window Algorithm"
description: "Sliding Window Algorithm Basic Information, Implementation of Sliding Window Algorithm in C#"
---

# Sliding Window Algorithm



## Sliding Window Algorithm Basic Information


Sliding window algorithm is used to perform required operation on specific window size of given large buffer or array. Window starts from the 1st element and keeps shifting right by one element. The objective is to find the minimum k numbers present in each window. This is commonly know as Sliding window problem or algorithm.

For example to find the maximum or minimum element from every `n` element in given array, sliding window algorithm is used.

**Example:**

**Input Array:** [1 3 -1 -3 5 3 6 7]

**Window Size:** 3

**Maximum** element from every 3 element of input array:

```cpp
+---------------------------------+---------+
|      Windows Position           |   Max   |
+------------+----+---+---+---+---+---------+
|[1   3   -1]| -3 | 5 | 3 | 6 | 7 |    3    |
+------------+----+---+---+---+---+---------+
| 1 |[3   -1   -3]| 5 | 3 | 6 | 7 |    3    |
+---+-------------+---+---+---+---+---------+
| 1 | 3 |[-1   -3   5]| 3 | 6 | 7 |    5    |
+---+---+-------------+---+---+---+---------+
| 1 | 3 | -1 |[-3   5   3]| 6 | 7 |    5    |
+---+---+----+------------+---+---+---------+
| 1 | 3 | -1 | -3 |[5   3   6]| 7 |    6    |
+---+---+----+----+-----------+---+---------+
| 1 | 3 | -1 | -3 | 5 |[3   6   7]|    7    |
+---+---+----+----+---+-----------+---------+

```

**Minimum** element from every 3 element of input array:

```cpp
+---------------------------------+---------+
|      Windows Position           |   Min   |
+------------+----+---+---+---+---+---------+
|[1   3   -1]| -3 | 5 | 3 | 6 | 7 |   -1    |
+------------+----+---+---+---+---+---------+
| 1 |[3   -1   -3]| 5 | 3 | 6 | 7 |   -3    |
+---+-------------+---+---+---+---+---------+
| 1 | 3 |[-1   -3   5]| 3 | 6 | 7 |   -3    |
+---+---+-------------+---+---+---+---------+
| 1 | 3 | -1 |[-3   5   3]| 6 | 7 |   -3    |
+---+---+----+------------+---+---+---------+
| 1 | 3 | -1 | -3 |[5   3   6]| 7 |    3    |
+---+---+----+----+-----------+---+---------+
| 1 | 3 | -1 | -3 | 5 |[3   6   7]|    3    |
+---+---+----+----+---+-----------+---------+

```

**Methods to find the sum of 3 element:**

**Method 1:**

First way is to use quick sort, when pivot is at Kth position, all elements on the right side are greater than pivot, hence, all elements on the left side automatically become K smallest elements of given array.

**Method 2:**

Keep an array of K elements, Fill it with first K elements of given input array.
Now from K+1 element, check if the current element is less than the maximum element in the auxiliary array, if yes, add this element into array.
Only problem with above solution is that we need to keep track of maximum element. Still workable. How can we keep track of maximum element in set of integer? Think heap. Think Max heap.

**Method 3:**

Great! In O(1) we would get the max element among K elements already chose as smallest K elements . If max in current set is greater than newly considered element, we need to remove max and introduce new element in set of K smallest element. Heapify again to maintain the heap property. Now we can easily get K minimum elements in array of N.

**Space Auxiliary:** `O(n)`

**Time Complexity:** `O(n)`



## Implementation of Sliding Window Algorithm in C#


```cpp
public class SlidingWindow
{
    public static int[] MaxSlidingWindow(int[] input, int k)
    {
        int[] result = new int[input.Length - k + 1];
        for (int i = 0; i <= input.Length - k; i++)
        {
            var max = input[i];
            for (int j = 1; j < k; j++)
            {
                if (input[i + j] > max) max = input[i + j];
            }
            result[i] = max;
        }
        return result;
    }

    public static int[] MinSlidingWindow(int[] input, int k)
    {
        int[] result = new int[input.Length - k + 1];
        for (int i = 0; i <= input.Length - k; i++)
        {
            var min = input[i];
            for (int j = 1; j < k; j++)
            {
                if (input[i + j] < min) min = input[i + j];
            }
            result[i] = min;
        }
        return result;
    }

    public static int[] MainMax(int[] input, int n)
    {
        return MaxSlidingWindow(input, n);
    }

    public static int[] MainMin(int[] input, int n)
    {
        return MinSlidingWindow(input, n);
    }
}

```

