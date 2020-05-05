---
metaTitle: "Algorithm - Maximum Path Sum Algorithm"
description: "Maximum Path Sum Basic Information, C# Implementation"
---

# Maximum Path Sum Algorithm



## Maximum Path Sum Basic Information


Maximum Path Sum is an algorithm to find out a path such that sum of element(node) of that path is greater than any other path.

For example, let's we have a we a triangle as shown below.

```

       3
      7   4
    2   4   6
  8   5   9   3

```

In above triangle, find the maximum path which has maximum sum.
Answer is, `3 + 7 + 4 + 9 = 23`

To find out the solution, as always we get an idea of Brute-Force method. Brute-Force method is good for this 4 rows triangle but think about a triangle with 100 or more than 100 rows. So, We can not use Brute-Force method to solve this problem.

Let's move to dynamic programming approach.

**Algorithm:**

For each and every node in a triangle or in a binary tree there can be four ways that the max path goes through the node.

1. Node only
1. Max path through Left Child + Node
1. Max path through Right Child + Node
1. Max path through Left Child + Node + Max path through Right Child.

**Example of Maximum Path Sum Algorithm:**

[<img src="https://i.stack.imgur.com/2Hq2e.png" alt="Example of Maximum Path Sum" />](https://i.stack.imgur.com/2Hq2e.png)

**Space Auxiliary:** `O(n)`<br>
**Time Complexity:** `O(n)`



## C# Implementation


```cpp
public class Node
{

    public int Value;
    public Node Left, Right;

    public Node(int item)
    {
        Value = item;
        Left = Right = null;
    }
}

class Res
{
    public int Val;
}

public class MaximumPathSum
{
    Node _root;
    int FindMaxUtil(Node node, Res res)
    {
        if (node == null) return 0;
        int l = FindMaxUtil(node.Left, res);
        int r = FindMaxUtil(node.Right, res);
        int maxSingle = Math.Max(Math.Max(l, r) + node.Value, node.Value);
        int maxTop = Math.Max(maxSingle, l + r + node.Value);
        res.Val = Math.Max(res.Val, maxTop);
        return maxSingle;
    }

    int FindMaxSum()
    {
        return FindMaxSum(_root);
    }

    int FindMaxSum(Node node)
    {
        Res res = new Res {Val = Int32.MinValue};
        FindMaxUtil(node, res);
        return res.Val;
    }

    public static int Main()
    {
        MaximumPathSum tree = new MaximumPathSum
        {
            _root = new Node(10)
            {
                Left = new Node(2),
                Right = new Node(10)
            }
        };
        tree._root.Left.Left = new Node(20);
        tree._root.Left.Right = new Node(1);
        tree._root.Right.Right = new Node(-25)
        {
            Left = new Node(3),
            Right = new Node(4)
        };
        return tree.FindMaxSum();
    }
}

```

