---
metaTitle: "MATLAB - Vectorization"
description: "Use of bsxfun, Implicit array expansion (broadcasting) [R2016b], Element-wise operations, Logical Masking, Sum, mean, prod & co, Get the  value of a function  of two or more arguments"
---

# Vectorization



## Use of bsxfun


Quite often, the reason why code has been written in a `for` loop is to compute values from 'nearby' ones. The function `bsxfun` can often be used to do this in a more succinct fashion.

For example, assume that you wish to perform a columnwise operation on the matrix `B`, subtracting the mean of each column from it:

```matlab
B = round(randn(5)*10);                  % Generate random data 
A = zeros(size(B));                      % Preallocate array
for col = 1:size(B,2);                    % Loop over columns
    A(:,col) = B(:,col) - mean(B(:,col));   % Subtract means 
end 

```

This method is inefficient if `B` is large, often due to MATLAB having to move the contents of variables around in memory. By using `bsxfun`, one can do the same job neatly and easily in just a single line:

```matlab
A = bsxfun(@minus, B, mean(B));

```

Here, `@minus` is a [function handle](http://ch.mathworks.com/help/matlab/matlab_prog/creating-a-function-handle.html) to the [`minus`](http://ch.mathworks.com/help/matlab/ref/minus.html) operator (`-`) and will be applied between elements of the two matrices `B` and `mean(B)`. Other function handles, even user-defined ones, are possible as well.

Next, suppose you want to add row vector `v` to each row in matrix `A`:

```matlab
v = [1,  2,  3];

A = [8,  1,  6
     3,  5,  7
     4,  9,  2];

```

The naive approach is use a loop (**do **not** do this**):

```matlab
B = zeros(3);
for row = 1:3
    B(row,:) = A(row,:) + v;
end

```

Another option would be to replicate `v` with `repmat` (**do **not** do this either**):

```matlab
>> v = repmat(v,3,1)
v =
     1     2     3
     1     2     3
     1     2     3

>> B = A + v; 

```

Instead use [`bsxfun`](http://uk.mathworks.com/help/matlab/ref/bsxfun.html) for this task:

```matlab
>> B = bsxfun(@plus, A, v);
B =
     9     3     9
     4     7    10
     5    11     5

```

### Syntax

`bsxfun(@fun, A, B)`

where `@fun` is one of the [supported functions](http://uk.mathworks.com/help/matlab/ref/bsxfun.html) and the two arrays `A` and `B` respect the two conditions below.

The name `bsxfun` helps to understand how the function works and it stands for **B**inary **FUN**ction with **S**ingleton e**X**pansion. In other words, if:

1. two arrays share the same dimensions except for one
1. and the discordant dimension is a singleton (i.e. has a size of `1`) in either of the two arrays

then the array with the singleton dimension will be expanded to match the dimension of the other array. After the expansion, a binary function is applied elementwise on the two arrays.

For example, let `A` be an `M`-by-`N`-by`K` array and `B` is an `M`-by-`N` array. Firstly, their first two dimensions have corresponding sizes. Secondly, `A` has `K` layers while `B` has implicitly only `1`, hence it is a singleton. All **conditions** are met and `B` will be replicated to match the 3rd dimension of `A`.

In other languages, this is commonly referred to as **broadcasting** and happens automatically in Python (numpy) and Octave.

The function, `@fun`, must be a binary function meaning it must take exactly two inputs.

### Remarks

Internally, `bsxfun` does not replicate the array and executes an efficient loop.



## Implicit array expansion (broadcasting) [R2016b]


**MATLAB R2016b** featured a generalization of its scalar expansion<sup>[1](https://www.mathworks.com/help/stateflow/ug/scalar-expansion-for-converting-scalars-to-nonscalars.html),[2](http://blogs.mathworks.com/loren/2006/02/22/scalar-expansion-and-more-take-2/)</sup> mechanism, to also support certain element-wise operations between **arrays** of different sizes, as long as their dimension are compatible.
The operators that support implicit expansion are[<sup>1</sup>](https://www.mathworks.com/help/matlab/release-notes.html?startrelease=R2016b&endrelease=R2016b):

- **Element-wise arithmetic operators:** `+`, `-`, `.*`, `.^`, `./`, `.\`.
- **Relational operators:** `<`, `<=`, `>`, `>=`, `==`, `~=`.
- **Logical operators:** `&`, `|`, `xor`.
- **Bit-wise functions:** `bitand`, `bitor`, `bitxor`.
- **Elementary math functions:** `max`, `min`, `mod`, `rem`, `hypot`, `atan2`, `atan2d`.

The aforementioned binary operations are allowed between arrays, as long as they have "compatible sizes". Sizes are considered "compatible" when each dimension in one array is either exactly equal to the same dimension in the other array, or is equal to `1`. Note that trailing singleton (that is, of size `1`) dimensions are omitted by MATLAB, even though there's theoretically an infinite amount of them. In other words - dimensions that appear in one array and do not appear in the other, are implicitly fit for automatic expansion.

For example, in MATLAB versions **before R2016b** this would happen:

```matlab
>> magic(3) + (1:3)
Error using  + 
Matrix dimensions must agree.

```

Whereas **starting from R2016b** the previous operation will succeed:

```matlab
>> magic(3) + (1:3)
ans =

     9     3     9
     4     7    10
     5    11     5

```

### Examples of compatible sizes:

|Description|1<sup>st</sup> Array Size|2<sup>nd</sup> Array Size|Result Size
|---|---|---|---|---|---|---|---|---|---
|Vector and scalar|`[3x1]`|`[1x1]`|`[3x1]`
|Row and column vectors|`[1x3]`|`[2x1]`|`[2x3]`
|Vector and 2D matrix|`[1x3]`|`[5x3]`|`[5x3]`
|N-D and K-D arrays|`[1x3x3]`|`[5x3x1x4x2]`|`[5x3x3x4x2]`

### Examples of incompatible sizes:

|Description|1<sup>st</sup> Array Size|2<sup>nd</sup> Array Size|Possible Workaround
|---|---|---|---|---|---|---|---|---|---
|Vectors where a dimension is a multiple of the same dimension in the other array.|`[1x2]`|`[1x8]`|`transpose`
|Arrays with dimensions that are multiples of each other.|`[2x2]`|`[8x8]`|`repmat`, `reshape`
|N-D arrays that have the right amount of singleton dimensions but they're in the wrong order (#1).|`[2x3x4]`|`[2x4x3]`|`permute`
|N-D arrays that have the right amount of singleton dimensions but they're in the wrong order (#2).|`[2x3x4x5]`|`[5x2]`|`permute`

**IMPORTANT:**<br>
Code relying on this convention is **NOT** backward-compatible with **any** older versions of MATLAB. Therefore, the explicit invocation of `bsxfun`<sup>[1](http://www.mathworks.com/help/matlab/ref/bsxfun.html),[2](http://stackoverflow.com/documentation/matlab/750/vectorization/3560/use-of-bsxfun#t=201609152140397001511)</sup> (which achieves the same effect) should be used if code needs to run on older MATLAB versions. If such a concern does not exist, [MATLAB R2016 release notes](https://www.mathworks.com/help/matlab/release-notes.html?startrelease=R2016b&endrelease=R2016b) encourage users to switch from `bsxfun`:

> 
**Compared to using `bsxfun`, implicit expansion offers faster speed of execution, better memory usage, and improved readability of code.**


Related reading:

- MATLAB documentation on "[**Compatible Array Sizes for Basic Operations**](http://www.mathworks.com/help/matlab/matlab_prog/compatible-array-sizes-for-basic-operations.html)".
- NumPy's Broadcasting<sup>[1](http://docs.scipy.org/doc/numpy/user/basics.broadcasting.html),[2](http://stackoverflow.com/documentation/numpy/1296/the-basics/9742/broadcasting-array-operations#t=201609152142178194547)</sup>.
- A comparison between the [speed of computing using `bsxfun` vs. implicit array expansion](http://stackoverflow.com/questions/42559922/how-much-faster-is-implicit-expansion-compared-with-bsxfun).



## Element-wise operations


MATLAB supports (and encourages) vectorized operations on vectors and matrices.<br />
For example, suppose we have `A` and `B`, two `n`-by-`m` matrices and we want `C` to be the element-wise product of the corresponding elements (i.e., `C(i,j) = A(i,j)*B(i,j)`).

The un-vectorized way, using nested loops is as follows:

```matlab
C = zeros(n,m);
for ii=1:n
    for jj=1:m
        C(ii,jj) = A(ii,jj)*B(ii,jj);
    end
end

```

However, the vectorized way of doing this is by using the element-wise operator `.*`:

```matlab
C = A.*B;

```


- For more information on the element-wise multiplication in MATLAB see the documentation of [`times`](http://www.mathworks.com/help/matlab/ref/times.html).
- For more information about the difference between array and matrix operations see [Array vs. Matrix Operations](http://ch.mathworks.com/help/matlab/matlab_prog/array-vs-matrix-operations.html) in the MATLAB documentation.



## Logical Masking


MATLAB supports the use of logical masking in order to perform selection on a matrix without the use of for loops or if statements.

A logical mask is defined as a matrix composed of only `1` and `0`.

For example:

```matlab
mask = [1 0 0; 0 1 0; 0 0 1];

```

is a logical matrix representing the identity matrix.

We can generate a logical mask using a predicate to query a matrix.

```matlab
A = [1 2 3; 4 5 6; 7 8 9];
B = A > 4;

```

We first create a 3x3 matrix, `A`, containing the numbers 1 through 9. We then query `A` for values that are greater than 4 and store the result in a new matrix called `B`.

`B` is a logical matrix of the form:

```matlab
B = [0 0 0
     0 1 1
     1 1 1]

```

Or `1` when the predicate `A > 4` was true. And `0` when it was false.

We can use logical matrices to access elements of a matrix. If a logical matrix is used to select elements, indices where a `1` appear in the logical matrix will be selected in the matrix you are selecting from.

Using the same `B` from above, we could do the following:

```matlab
C = [0 0 0; 0 0 0; 0 0 0];
C(B) = 5;

```

This would select all of the elements of `C` where `B` has a `1` in that index. Those indices in `C` are then set to `5`.

Our `C` now looks like:

```matlab
C = [0 0 0
     0 5 5
     5 5 5]

```

We can reduce complicated code blocks containing `if` and `for` by using logical masks.

Take the non-vectorized code:

```matlab
A = [1 3 5; 7 9 11; 11 9 7];
for j = 1:length(A)
  if A(j) > 5
    A(j) = A(j) - 2;
  end
end

```

This can be shortened using logical masking to the following code:

```matlab
A = [1 3 5; 7 9 11; 11 9 7];
B = A > 5;
A(B) = A(B) - 2;

```

Or even shorter:

```matlab
A = [1 3 5; 7 9 11; 11 9 7];
A(A > 5) = A(A > 5) - 2;

```



## Sum, mean, prod & co


Given a random vector

```matlab
v = rand(10,1);

```

if you want the sum of its elements, do **NOT** use a loop

```matlab
s = 0;
for ii = 1:10
    s = s + v(ii);
end 

```

but use the vectorized capability of the [`sum()`](http://uk.mathworks.com/help/matlab/ref/sum.html) function

```matlab
s = sum(v);

```

Functions like [`sum()`](http://uk.mathworks.com/help/matlab/ref/sum.html), [`mean()`](http://uk.mathworks.com/help/matlab/ref/mean.html), [`prod()`](http://uk.mathworks.com/help/matlab/ref/prod.html) and others, have the ability to operate directly along rows, columns or other dimensions.

For instance, given a random matrix

```matlab
A = rand(10,10);

```

the average for each **column** is

```matlab
m = mean(A,1);

```

the average for each **row** is

```matlab
m = mean(A,2)

```

All the functions above work only on one dimension, but what if you want to sum the whole matrix? You could use:

```matlab
s = sum(sum(A))

```

But what if have an ND-array? applying `sum` on `sum` on `sum`... don't seem like the best option, instead use the `:` operator to vectorize your array:

```matlab
s = sum(A(:))

```

and this will result in one number which is the sum of all your array, doesn't matter how many dimensions it have.



## Get the  value of a function  of two or more arguments


In many application it is necessary to compute the function of two or more arguments.

Traditionally, we use `for`-loops. For example, if we need to calculate the `f = exp(-x^2-y^2)` (do not use this if you need **fast simulations**):

```matlab
% code1
x = -1.2:0.2:1.4;
y = -2:0.25:3;
for nx=1:lenght(x)
   for ny=1:lenght(y)
      f(nx,ny) = exp(-x(nx)^2-y(ny)^2);
   end
end

```

But vectorized version is more elegant and faster:

```matlab
% code2
[x,y] = ndgrid(-1.2:0.2:1.4, -2:0.25:3);
f = exp(-x.^2-y.^2);

```

than we can visualize it:

```matlab
surf(x,y,f)

```

**Note1** - Grids: Usually, the matrix storage is organized **row-by-row**. But in the MATLAB, it is the **column-by-column** storage as in FORTRAN. Thus, there are two simular functions `ndgrid` and `meshgrid` in MATLAB  to implement the two aforementioned models. To visualise the function in the case of `meshgrid`, we can use:

```matlab
surf(y,x,f)

```

**Note2** - Memory consumption: Let size of `x` or `y` is 1000. Thus, we need to store `1000*1000+2*1000 ~ 1e6` elements for non-vectorized ****code1****.
But we need `3*(1000*1000) = 3e6` elements in the case of vectorized ****code2****.
In the 3D case (let `z` has the same size as`x` or `y`), memory consumption increases dramatically: `4*(1000*1000*1000)` (~32GB for doubles) in the case of the vectorized ****code2**** vs `~1000*1000*1000` (just ~8GB) in the case of ****code1****. Thus, we have to choose either the memory or speed.

