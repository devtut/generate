---
metaTitle: "Algorithm - Maximum Subarray Algorithm"
description: "Maximum Subarray Algorithm Basic Information, C# Implementation"
---

# Maximum Subarray Algorithm



## Maximum Subarray Algorithm Basic Information


[Maximum subarray problem](https://en.wikipedia.org/wiki/Maximum_subarray_problem) is the method to find the contiguous subarray within a one-dimensional array of numbers which has the largest sum.

The problem was originally proposed by [Ulf Grenander](https://en.wikipedia.org/wiki/Ulf_Grenander) of Brown University in 1977, as a simplified model for maximum likelihood estimation of patterns in digitized images.

We can problem like this, let us consider a list of various integers. We might be interested in which completely adjacent subset will have the greatest sum. For example, if we have the array `[0, 1, 2, -2, 3, 2]`, the maximum subarray is `[3, 2]`, with a sum of `5`.

**Brute-Force Approach for Solution:**

This method is most inefficient to find out the solution. In this, we will end up going through every single possible subarray, and then finding the sum of all of them. At last, compare all values and find out maximum subarray.

**Pseudo code for Brute-Force Approach:**

```cpp
MaxSubarray(array)
  maximum = 0
  for i in input
    current = 0
    for j in input
       current += array[j]
       if current > maximum
         maximum = current
  return maximum

```

**Time complexity for Brute-Force method is** `O(n^2)`. So let's move to divide and conquer approach.

**Divide and Conquer Approach for Solution:**

Find the sum of the subarrays on the left side, the subarrays on the right. Then, take a look through all of the ones that cross over the center divide, and finally return the maximum sum. Because this is a divide and conquer algorithm, we need to have two different functions.

First is divide step,

```cpp
maxSubarray(array)
  if start = end
    return array[start]
  else
    middle = (start + end) / 2
    return max(maxSubarray(array(From start to middle)), maxSubarray(array(From middle + 1 to end)), maxCrossover(array))

```

In second part, separate the different part that are created in first part.

```cpp
maxCrossover(array)
  currentLeftSum = 0
      leftSum = 0
  currentRightSum = 0
      rightSum = 0
  for i in array
    currentLeftSum += array[i]
    if currentLeftSum > leftSum
      leftSum = currentLeftSum
  for i in array
    currentRightSum += array[i]
    if currentRightSum > rightSum
      rightSum = currentRightSum
  return leftSum + rightSum

```

**Time complexity for Divide and Conquer method is** `O(nlogn)`. So let's move to dynamic programming approach.

**Dynamic Programming Approach:**

This solution is also known as Kadane's Algorithm. It is linear time algorithm. This solution is given by [Joseph B. Kadane](https://en.wikipedia.org/wiki/Joseph_Born_Kadane) in late '70s.

This algorithm just goes through the loop, continuously changing the current maximum sum. Interestingly enough, this is a very simple example of a dynamic programming algorithm, since it takes an overlapping problem and reduces it so we can find a more efficient solution.

**Pseudo code of Kadane's Algorithm:**

```cpp
MaxSubArray(array)
  max = 0
  currentMax = 0
  for i in array
    currentMax += array[i]
    if currentMax < 0
      currentMax = 0
    if max < currentMax
      max = currentMax
  return max

```

**Time complexity for Kadane's Algorithm is** `O(n)`.



## C# Implementation


```cpp
public class MaximumSubarray
{
    private static int Max(int a, int b)
    {
        return a > b ? a : b;
    }

    static int MaxSubArray(int[] input, int n)
    {
        int max = input[0];
        int currentMax = input[0];
        for (int i = 1; i < n; i++)
        {
            currentMax = Max(input[i], currentMax + input[i]);
            max = Max(max, currentMax);
        }
        return max;
    }

    public static int Main(int[] input)
    {
        int n = input.Length;
        return MaxSubArray(input, n);
    }
}

```

