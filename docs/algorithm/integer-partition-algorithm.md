---
metaTitle: "Integer Partition Algorithm"
description: "Basic Information of Integer Partition Algorithm, Implementation of Interger Partition Algorithm in C#"
---

# Integer Partition Algorithm



## Basic Information of Integer Partition Algorithm


The [partition of an integer](https://en.wikipedia.org/wiki/Partition_(number_theory)) is a way of writing it as a sum of positive integers. For example, the partitions of the number 5 are:

- 5
- 4 + 1
- 3 + 2
- 2 + 2 + 1
- 2 + 1 + 1 + 1
- 1 + 1 + 1 + 1 + 1

Notice that changing the order of the summands will not create a different partition.

The partition function is inherently recursive in nature since the results of smaller numbers appear as components in the result of a larger number. Let **p(n,m)** be the number of partitions of **n** using only positive integers that are less than or equal to **m**. It may be seen that **p(n)** = **p(n,n)**, and also **p(n,m)** = **p(n,n)** = **p(n)** for **m** > **n**.

[<img src="https://i.stack.imgur.com/CteBS.png" alt="Equation" />](https://i.stack.imgur.com/CteBS.png)

**Example of Integer Partition Algorithm:**

[<img src="https://i.stack.imgur.com/5kiXt.jpg" alt="Example of Integer Partition Algorithm" />](https://i.stack.imgur.com/5kiXt.jpg)

**Auxiliary Space:** `O(n^2)`<br>
**Time Complexity:** `O(n(logn))`



## Implementation of Interger Partition Algorithm in C#


```

public class IntegerPartition
{
    public static int[,] Result = new int[100,100];

    private static int Partition(int targetNumber, int largestNumber)
    {
        for (int i = 1; i <= targetNumber; i++)
        {
            for (int j = 1; j <= largestNumber; j++)
            {
                if (i - j < 0)
                {
                    Result[i, j] = Result[i, j - 1];
                    continue;
                }
                Result[i, j] = Result[i, j - 1] + Result[i - j, j];
            }
        }
        return Result[targetNumber, largestNumber];
    }

    public static int Main(int number, int target)
    {
        int i;
        for (i = 0; i <= number; i++)
        {
            Result[i, 0] = 0;
        }
        for (i = 1; i <= target; i++)
        {
            Result[0, i] = 1;
        }
        return Partition(number, target);
    }
}

```

