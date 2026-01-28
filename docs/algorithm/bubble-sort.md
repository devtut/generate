---
title: "Algorithm - Bubble Sort"
description: "Bubble Sort, Implementation in  C & C++, Implementation in Javascript, Implementation in C#, Implementation in Java, Python Implementation"
date: 2026-01-27
tags: ["algorithm"]
---

# Bubble Sort

## Bubble Sort

The `BubbleSort` compares each successive pair of elements in an unordered list and inverts the elements if they are not in order.

The following example illustrates the bubble sort on the list &#123;6,5,3,1,8,7,2,4&#125; (pairs that were compared in each step are encapsulated in '**'):

```text
&#123;6,5,3,1,8,7,2,4&#125;
&#123;**5,6**,3,1,8,7,2,4&#125; -- 5 &lt; 6 -> swap
&#123;5,**3,6**,1,8,7,2,4&#125; -- 3 &lt; 6 -> swap
&#123;5,3,**1,6**,8,7,2,4&#125; -- 1 &lt; 6 -> swap
&#123;5,3,1,**6,8**,7,2,4&#125; -- 8 > 6 -> no swap
&#123;5,3,1,6,**7,8**,2,4&#125; -- 7 &lt; 8 -> swap
&#123;5,3,1,6,7,**2,8**,4&#125; -- 2 &lt; 8 -> swap
&#123;5,3,1,6,7,2,**4,8**&#125; -- 4 &lt; 8 -> swap
```

After one iteration through the list, we have &#123;5,3,1,6,7,2,4,8&#125;. Note that the greatest unsorted value in the array (8 in this case) will always reach its final position. Thus, to be sure the list is sorted we must iterate n-1 times for lists of length n.

Graphic:

[<img src="http://i.stack.imgur.com/NJPXP.gif" alt="BubbleSort" />](http://i.stack.imgur.com/NJPXP.gif)

## Implementation in  C & C++

An example implementation of `BubbleSort` in `C++`:

```

  void bubbleSort(vector&lt;int>numbers)
   &#123;
       for(int i = numbers.size() - 1; i >= 0; i--) &#123;
           for(int j = 1; j &lt;= i; j++) &#123;
               if(numbers[j-1] > numbers[j]) &#123;
                   swap(numbers[j-1],numbers(j));
               &#125;
           &#125;
       &#125;
   &#125;

```

**C Implementation**

```cpp
void bubble_sort(long list[], long n)
&#123;
  long c, d, t;
 
  for (c = 0 ; c &lt; ( n - 1 ); c++)
  &#123;
    for (d = 0 ; d &lt; n - c - 1; d++)
    &#123;
      if (list[d] > list[d+1])
      &#123;
        /* Swapping */
 
        t         = list[d];
        list[d]   = list[d+1];
        list[d+1] = t;
      &#125;
    &#125;
  &#125;
&#125;
```

**Bubble Sort with pointer**

```cpp
void pointer_bubble_sort(long * list, long n)
&#123;
  long c, d, t;
 
  for (c = 0 ; c &lt; ( n - 1 ); c++)
  &#123;
    for (d = 0 ; d &lt; n - c - 1; d++)
    &#123;
      if ( * (list + d ) > *(list+d+1))
      &#123;
        /* Swapping */
 
        t         = * (list + d );
        * (list + d )   = * (list + d + 1 );
        * (list + d + 1) = t;
      &#125;
    &#125;
  &#125;
&#125;
```

## Implementation in Javascript

```javascript
function bubbleSort(a)
    &#123;
        var swapped;
        do &#123;
            swapped = false;
            for (var i=0; i &lt; a.length-1; i++) &#123;
                if (a[i] > a[i+1]) &#123;
                    var temp = a[i];
                    a[i] = a[i+1];
                    a[i+1] = temp;
                    swapped = true;
                &#125;
            &#125;
        &#125; while (swapped);
    &#125;

var a = [3, 203, 34, 746, 200, 984, 198, 764, 9];
bubbleSort(a);
console.log(a); //logs [ 3, 9, 34, 198, 200, 203, 746, 764, 984 ]
```

## Implementation in C#

Bubble sort is also known as **Sinking Sort**. It is a simple sorting algorithm that repeatedly steps through the list to be sorted, compares each pair of adjacent items and swaps them if they are in the wrong order.

**Bubble sort example**
[<img src="http://i.stack.imgur.com/SDHQM.jpg" alt="Bubble sort example" />](http://i.stack.imgur.com/SDHQM.jpg)

**Implementation of Bubble Sort**<br />
I used C# language to implement bubble sort algorithm

```java
public class BubbleSort
&#123;
    public static void SortBubble(int[] input)
    &#123;
        for (var i = input.Length - 1; i >= 0; i--)
        &#123;
            for (var j = input.Length - 1 - 1; j >= 0; j--)
            &#123;
                if (input[j] &lt;= input[j + 1]) continue;
                var temp = input[j + 1];
                input[j + 1] = input[j];
                input[j] = temp;
            &#125;
        &#125;
    &#125;

    public static int[] Main(int[] input)
    &#123;
        SortBubble(input);
        return input;
    &#125;
&#125;
```

## Implementation in Java

```java
public class MyBubbleSort &#123;
  
    public static void bubble_srt(int array[]) &#123;//main logic
        int n = array.length;
        int k;
        for (int m = n; m >= 0; m--) &#123;
            for (int i = 0; i &lt; n - 1; i++) &#123;
                k = i + 1;
                if (array[i] > array[k]) &#123;
                    swapNumbers(i, k, array);
                &#125;
            &#125;
            printNumbers(array);
        &#125;
    &#125;
  
    private static void swapNumbers(int i, int j, int[] array) &#123;
  
        int temp;
        temp = array[i];
        array[i] = array[j];
        array[j] = temp;
    &#125;
  
    private static void printNumbers(int[] input) &#123;
          
        for (int i = 0; i &lt; input.length; i++) &#123;
            System.out.print(input[i] + ", ");
        &#125;
        System.out.println("\n");
    &#125;
  
    public static void main(String[] args) &#123;
        int[] input = &#123; 4, 2, 9, 6, 23, 12, 34, 0, 1 &#125;;
        bubble_srt(input);
  
    &#125;
&#125;
```

## Python Implementation

```cpp
#!/usr/bin/python

input_list = [10,1,2,11]

for i in range(len(input_list)):
  for j in range(i):
    if int(input_list[j]) > int(input_list[j+1]):
      input_list[j],input_list[j+1] = input_list[j+1],input_list[j]

print input_list
```

#### Parameters

| Parameter | Description |
| --------- | ----------- ||  |
| Stable                  | Yes         |
| In place                | Yes         |
| Best case complexity    | O(n)        |
| Average case complexity | O(n^2)      |
| Worst case complexity   | O(n^2)      |
| Space complexity        | O(1)        |
