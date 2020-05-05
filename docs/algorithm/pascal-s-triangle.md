---
metaTitle: "Algorithm - Pascal's Triangle"
description: "Pascal triangle in C, Pascal's Triagle Basic Information, Implementation of Pascal's Triangle in C#"
---

# Pascal's Triangle



## Pascal triangle in C


```cpp
int i, space, rows, k=0, count = 0, count1 = 0;
row=5;
for(i=1; i<=rows; ++i)
{
    for(space=1; space <= rows-i; ++space)
    {
        printf("  ");
        ++count;
    }

    while(k != 2*i-1)
    {
        if (count <= rows-1)
        {
            printf("%d ", i+k);
            ++count;
        }
        else
        {
            ++count1;
            printf("%d ", (i+k-2*count1));
        }
        ++k;
    }
    count1 = count = k = 0;

    printf("\n");
}

```

**Output**

```

       1
      2 3 2
    3 4 5 4 3
  4 5 6 7 6 5 4
5 6 7 8 9 8 7 6 5

```



## Pascal's Triagle Basic Information


One of the most interesting Number Patterns is [Pascal's Triangle](https://en.wikipedia.org/wiki/Pascal%27s_triangle). The Name "Pascal's Triangle" named after [Blaise Pascal](https://en.wikipedia.org/wiki/Blaise_Pascal), a famous French Mathematician and Philosopher.

In Mathematics, Pascal's Triangle is a triangular array of binomial coefficients.The rows of Pascal's triangle are conventionally enumerated starting with row n = 0 at the top (the 0th row). The entries in each row are numbered from the left beginning with k = 0 and are usually staggered relative to the numbers in the adjacent rows.

**The triangle is constructed in the below manner:**

- In the topmost row, there is a unique nonzero entry 1.
- Each entry of each subsequent row is constructed by adding the number above and to the left with the number above and to the right, treating blank entries as 0.

For example, the initial number in the first (or any other) row is 1 (the sum of 0 and 1), whereas the numbers 1 and 3 in the third row are added to produce the number 4 in the fourth row.

**Equation to generate each entry in Pascal triangle:**

[<img src="https://i.stack.imgur.com/7WEJ9.png" alt="Pascal Equation" />](https://i.stack.imgur.com/7WEJ9.png)

for any non-negative integer n and any integer k between 0 and n, inclusive. This recurrence for the binomial coefficients is known as [Pascal's rule](https://en.wikipedia.org/wiki/Pascal%27s_rule). Pascal's triangle has higher dimensional generalizations. The three-dimensional version is called Pascal's pyramid or Pascal's tetrahedron, while the general versions are called Pascal's simplices.

**Example of Pascal's Triangle:**

[<img src="https://i.stack.imgur.com/chhsE.gif" alt="Pascal's Triangle Example" />](https://i.stack.imgur.com/chhsE.gif)



## Implementation of Pascal's Triangle in C#


```cpp
public class PascalsTriangle
{
    static void PascalTriangle(int n)
    {
        for (int line = 1; line <= n; line++)
        {
            int c = 1;
            for (int i = 1; i <= line; i++)
            {
                Console.WriteLine(c);
                c = c * (line - i) / i;
            }
            Console.WriteLine("\n");
        }
    }

    public static int Main(int input)
    {
        PascalTriangle(input);
        return input;
    }
}

```

