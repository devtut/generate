---
metaTitle: "Algorithm - Algo:- Print a m*n matrix in square wise"
description: "Sample Example, Write the generic code"
---

# Algo:- Print a m*n matrix in square wise


Check sample input and output below.



## Sample Example


```cpp
Input :-

14 15 16 17 18 21
19 10 20 11 54 36
64 55 44 23 80 39
91 92 93 94 95 42

Output:- 
print value in index
14 15 16 17 18 21 36 39 42 95 94 93 92 91 64 19 10 20 11 54 80 23 44 55

or print index
00 01 02 03 04 05 15 25 35 34 33 32 31 30 20 10 11 12 13 14 24 23 22 21

```



## Write the generic code


```cpp
function noOfLooping(m,n) {
    if(m > n) {
        smallestValue = n;
    } else {
        smallestValue = m;
    }

    if(smallestValue % 2 == 0) {
            return smallestValue/2;
    } else {
            return (smallestValue+1)/2;
    }
}

function squarePrint(m,n) {
    var looping = noOfLooping(m,n);
    for(var i = 0; i < looping; i++) {
        for(var j = i; j < m - 1 - i; j++) {
                console.log(i+''+j);
        }
        for(var k = i; k < n - 1 - i; k++) {
                console.log(k+''+j);
        }
        for(var l = j; l > i; l--) {
                console.log(k+''+l);
        }
        for(var x = k; x > i; x--) {
                console.log(x+''+l);
        }
    }
}

squarePrint(6,4);

```

