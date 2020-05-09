---
metaTitle: "MATLAB - Getting started with MATLAB Language"
description: "Indexing matrices and arrays, Anonymous functions and function handles, Matrices and Arrays, Cell arrays, Hello World, Helping yourself, Scripts and Functions, Reading Input & Writing Output, Data Types"
---

# Getting started with MATLAB Language



## Indexing matrices and arrays


MATLAB allows for several methods to index (access) elements of matrices and arrays:

- **Subscript indexing** - where you specify the position of the elements you want in each dimension of the matrix separately.
- **Linear indexing** - where the matrix is treated as a vector, no matter its dimensions. That means, you specify each position in the matrix with a single number.
- **Logical indexing** - where you use a logical matrix (and matrix of `true` and `false` values) with the identical dimensions of the matrix you are trying to index as a mask to specify which value to return.

These three methods are now explained in more detail using the following 3-by-3 matrix `M` as an example:

```matlab
>> M = magic(3)

ans = 

       8    1    6
       3    5    7
       4    9    2

```

### Subscript indexing

The most straight-forward method for accessing an element, is to specify its row-column index. For example, accessing the element on the second row and third column:

```matlab
>> M(2, 3)

ans =

      7

```

The number of subscripts provided exactly matches the number of dimensions `M` has (two in this example).

Note that the order of subscripts is the same as the mathematical convention: row index is the first. Moreover, MATLAB indices **starts with `1`** and **not `0`** like most programming languages.

You can index multiple elements at once by passing a vector for each coordinate instead of a single number. For example to get the entire second row, we can specify that we want the first, second and third columns:

```matlab
>> M(2, [1,2,3])

ans =

       3    5    7

```

In MATLAB, the vector `[1,2,3]` is more easily created using the colon operator, i.e. `1:3`. You can use this in indexing as well. To select an entire row (or column), MATLAB provides a shortcut by allowing you just specify `:`. For example, the following code will also return the entire second row

```matlab
>> M(2, :)

ans =

       3    5    7

```

MATLAB also provides a shortcut for specifying the last element of a dimension in the form of the [`end`](http://ch.mathworks.com/help/fixedpoint/ref/end.html) keyword. The `end` keyword will work exactly as if it was the number of the last element in that dimension. So if you want all the columns from column `2` to the last column, you can use write the following:

```matlab
>> M(2, 2:end)

ans =

       5    7

```

Subscript indexing can be restrictive as it will not allow to extract single values from different columns and rows; it will extract the combination of all rows and columns.

```matlab
>> M([2,3], [1,3])
ans =

       3    7
       4    2

```

For example subscript indexing cannot extract only the elements `M(2,1)` or `M(3,3)`. To do this we must consider linear indexing.

### Linear indexing

MATLAB allows you to treat n-dimensional arrays as one-dimensional arrays when you index using only one dimension. You can directly access the first element:

```matlab
>> M(1)

ans = 

       8

```

Note that arrays are stored in [column-major order](https://en.wikipedia.org/wiki/Row-major_order) in MATLAB which means that you access the elements by first going down the columns. So `M(2)` is the second element of the first column which is `3` and `M(4)` will be the first element of the second column i.e.

```matlab
>> M(4)

ans = 

        1

```

There exist built-in functions in MATLAB to convert subscript indices to linear indices, and vice versa: [`sub2ind`](http://www.mathworks.com/help/matlab/ref/sub2ind.html) and [`ind2sub`](http://www.mathworks.com/help/matlab/ref/ind2sub.html) respectively. You can manually convert the subscripts (`r`,`c`) to a linear index by

```matlab
idx = r + (c-1)*size(M,1)

```

To understand this, if we are in the first column then the linear index will simply be the row index. The formula above holds true for this because for `c == 1`, `(c-1) == 0`. In the next columns, the linear index is the row number plus all the rows of the previous columns.

Note that the `end` keyword still applies and now refers to the very last element of the array i.e. `M(end) == M(end, end) == 2`.

You can also index multiple elements using linear indexing. Note that if you do that, the returned matrix will have the same shape as the matrix of index vectors.

`M(2:4)` returns a row vector because `2:4` represents the row vector `[2,3,4]`:

```matlab
>> M(2:4)

ans =

        3    4    1

```

As an other example, `M([1,2;3,4])` returns a 2-by-2 matrix because `[1,2;3,4]` is a 2-by-2 matrix as well. See the below code to convince yourself:

```matlab
>> M([1,2;3,4])

ans =

       8        3
       4        1

```

Note that indexing with `:` alone will **always** return a column vector:

```matlab
>> M(:)

ans = 

        8
        3
        4
        1
        5
        9
        6
        7
        2

```

This example also illustrates the order in which MATLAB returns elements when using linear indexing.

### Logical indexing

The third method of indexing is to use a logical matrix, i.e. a matrix containing only `true` or `false` values, as a mask to filter out the elements you don't want. For example, if we want to find all the elements of `M` that are greater than `5` we can use the logical matrix

```matlab
>> M > 5

ans =

    1    0    1
    0    0    1
    0    1    0

```

to index `M` and return only the values that are greater than `5` as follows:

```matlab
>> M(M > 5)

ans =

        8
        9
        6
        7

```

If you wanted these number to stay in place (i.e. keep the shape of the matrix), then you could assign to the logic compliment

```matlab
>> M(~(M > 5)) = NaN

ans = 

    8      NaN    6
    NaN    NaN    7
    NaN    9      Nan

```

We can reduce complicated code blocks containing `if` and `for` statements by using logical indexing.

Take the non-vectorized (already shortened to a single loop by using linear indexing):

```matlab
for elem = 1:numel(M)
  if M(elem) > 5
    M(elem) = M(elem) - 2;
  end
end

```

This can be shortened to the following code using logical indexing:

```matlab
idx = M > 5;
M(idx) = M(idx) - 2;

```

Or even shorter:

```matlab
M(M > 5) = M(M > 5) - 2;

```

### More on indexing

**Higher dimension matrices**

All the methods mentioned above generalize into n-dimensions. If we use the three-dimensional matrix `M3 = rand(3,3,3)` as an example, then you can access all the rows and columns of the second slice of the third dimension by writing

```matlab
>> M(:,:,2)

```

You can access the first element of the second slice using linear indexing. Linear indexing will only move on to the second slice after all the rows and all the columns of the first slice. So the linear index for that element is

```matlab
>> M(size(M,1)*size(M,2)+1)

```

In fact, in MATLAB, **every** matrix is n-dimensional: it just happens to be that the size of most of the other n-dimensions are one. So, if `a = 2` then `a(1) == 2` (as one would expect), **but also** `a(1, 1) == 2`, as does `a(1, 1, 1) == 2`, `a(1, 1, 1, ..., 1) == 2` and so on. These "extra" dimensions (of size `1`), are referred to as **singleton dimensions**. The command `squeeze` will remove them, and one can use `permute` to swap the order of dimensions around (and introduce singleton dimensions if required).

An n-dimensional matrix can also be indexed using an m subscripts (where m<=n). The rule is that the first m-1 subscripts behave ordinarily, while the last (m'th) subscript references the remaining (n-m+1) dimensions, just as a linear index would reference an (n-m+1) dimensional array. Here is an example:

```matlab
>> M = reshape(1:24,[2,3,4]);
>> M(1,1)
ans =
     1
>> M(1,10)
ans =
    19
>> M(:,:)
ans =
     1     3     5     7     9    11    13    15    17    19    21    23
     2     4     6     8    10    12    14    16    18    20    22    24

```

**Returning ranges of elements**

With subscript indexing, if you specify more than one element in more than one dimension, MATLAB returns each possible pair of coordinates. For example, if you try M([1,2],[1,3]) MATLAB will return `M(1,1)` and `M(2,3)` but it will **also** return `M(1,3)` and `M(2,1)`. This can seem unintuitive when you are looking for the elements for a list of coordinate pairs but consider the example of a larger matrix, `A = rand(20)` (note `A` is now `20`-by-`20`), where you want to get the top right hand quadrant. In this case instead of having to specify every coordinate pair in that quadrant (and this this case that would be `100` pairs), you just specify the `10` rows and the `10` columns you want so `A(1:10, 11:end)`. **Slicing** a matrix like this is far more common than requiring a list of coordinate pairs.

In the event that you do want to get a list of coordinate pairs, the simplest solution is to convert to linear indexing. Consider the problem where you have a vector of column indices you want returned, where each row of the vector contains the column number you want returned for the **corresponding** row of the matrix. For example

```matlab
colIdx = [3;2;1]

```

So in this case you actually want to get back the elements at `(1,3)`, `(2,2)` and `(3,1)`. So using linear indexing:

```matlab
>> colIdx = [3;2;1];
>> rowIdx = 1:length(colIdx);
>> idx = sub2ind(size(M), rowIdx, colIdx);
>> M(idx)

ans = 

        6    5    4

```

**Returning an element multiple times**

With subscript and linear indexing you can also return an element multiple times by repeating it's index so

```matlab
>> M([1,1,1,2,2,2])

ans = 

        8    8    8    3    3    3

```

You can use this to duplicate entire rows and column for example to repeat the first row and last column

```matlab
>> M([1, 1:end], [1:end, end])

ans = 

        8    1    6    6 
        8    1    6    6
        3    5    7    7
        4    9    2    2

```

For more information, see [here](http://www.mathworks.com/company/newsletters/articles/matrix-indexing-in-matlab.html).



## Anonymous functions and function handles


### Basics

Anonymous functions are a powerful tool of the MATLAB language. They are functions that exist locally, that is: in the current workspace. However, they do not exist on the MATLAB path like a regular function would, e.g. in an m-file. That is why they are called anonymous, although they can have a name like a variable in the workspace.

### The `@` operator

Use the `@` operator to create anonymous functions and function handles. For example, to create a handle to the `sin` function (sine) and use it as `f`:

```matlab
>> f = @sin
f = 
    @sin

```

Now `f` is a handle to the `sin` function. Just like (in real life) a door handle is a way to use a door, a function handle is a way to use a function. To use `f`, arguments are passed to it as if it were the `sin` function:

```matlab
>> f(pi/2)
ans =
     1

```

`f` accepts any input arguments the `sin` function accepts. If `sin` would be a function that accepts zero input arguments (which it does not, but others do, e.g. the `peaks` function), `f()` would be used to call it without input arguments.

### Custom anonymous functions

### Anonymous functions of one variable

It is not obviously useful to create a handle to an existing function, like `sin` in the example above. It is kind of redundant in that example. However, it is useful to create anonymous functions that do custom things that otherwise would need to be repeated multiple times or created a separate function for. As an example of a custom anonymous function that accepts one variable as its input, sum the sine and cosine squared of a signal:

```matlab
>> f = @(x) sin(x)+cos(x).^2
f = 
    @(x)sin(x)+cos(x).^2

```

Now `f` accepts one input argument called `x`. This was specified using parentheses `(...)` directly after the `@` operator. `f` now is an anonymous function of `x`: `f(x)`. It is used by passing a value of `x` to `f`:

```matlab
>> f(pi)
ans =
    1.0000

```

A vector of values or a variable can also be passed to `f`, as long as they are used in a valid way within `f`:

```matlab
>> f(1:3) % pass a vector to f
ans =
    1.1334    1.0825    1.1212
>> n = 5:7;
>> f(n) % pass n to f
ans =
   -0.8785    0.6425    1.2254

```

### Anonymous functions of more than one variable

In the same fashion anonymous functions can be created to accept more than one variable. An example of an anonymous function that accepts three variables:

```matlab
>> f = @(x,y,z) x.^2 + y.^2 - z.^2
f = 
    @(x,y,z)x.^2+y.^2-z.^2
>> f(2,3,4)
ans =
    -3

```

### Parameterizing anonymous functions

Variables in the workspace can be used within the definition of anonymous functions. This is called parameterizing. For example, to use a constant `c = 2` in an anonymous function:

```matlab
>> c = 2;
>> f = @(x) c*x
f = 
    @(x)c*x
>> f(3)
ans =
     6

```

`f(3)` used the variable `c` as a parameter to multiply with the provided `x`. Note that if the value of `c` is set to something different at this point, then `f(3)` is called, the result would **not** be different. The value of `c` is the value **at the time of creation** of the anonymous function:

```matlab
>> c = 2;
>> f = @(x) c*x;
>> f(3)
ans =
     6
>> c = 3;
>> f(3)
ans =
     6

```

### Input arguments to an anonymous function do not refer to workspace variables

Note that using the name of variables in the workspace as one of the input arguments of an anonymous function (i.e., using `@(...)`) will **not** use those variables' values. Instead, they are treated as different variables within the scope of the anonymous function, that is: the anonymous function has its private workspace where the input variables never refer to the variables from the main workspace. The main workspace and the anonymous function's workspace do not know about each other's contents. An example to illustrate this:

```matlab
>> x = 3 % x in main workspace
x =
     3
>> f = @(x) x+1; % here x refers to a private x variable
>> f(5)
ans =
     6
>> x
x =
     3

```

The value of `x` from the main workspace is not used within `f`. Also, in the main workspace `x` was left untouched. Within the scope of `f`, the variable names between parentheses after the `@` operator are independent from the main workspace variables.

### Anonymous functions are stored in variables

An anonymous function (or, more precisely, the function handle pointing at an anonymous function) is stored like any other value in the current workspace: In a variable (as we did above), in a cell array (`{@(x)x.^2,@(x)x+1}`), or even in a property (like `h.ButtonDownFcn` for interactive graphics). This means the anonymous function can be treated like any other value. When storing it in a variable, it has a name in the current workspace and can be changed and cleared just like variables holding numbers.

Put differently: A function handle (whether in the `@sin` form or for an anonymous function) is simply a value that can be stored in a variable, just like a numerical matrix can be.

### Advanced use

### Passing function handles to other functions

Since function handles are treated like variables, they can be passed to functions that accept function handles as input arguments.

An example: A function is created in an m-file that accepts a function handle and a scalar number. It then calls the function handle by passing `3` to it and then adds the scalar number to the result. The result is returned.

Contents of `funHandleDemo.m`:

```matlab
function y = funHandleDemo(fun,x)
y = fun(3);
y = y + x;

```

Save it somewhere on the path, e.g. in MATLAB's current folder. Now `funHandleDemo` can be used as follows, for example:

```matlab
>> f = @(x) x^2; % an anonymous function
>> y = funHandleDemo(f,10) % pass f and a scalar to funHandleDemo
y =
    19

```

The handle of another existing function can be passed to `funHandleDemo`:

```matlab
>> y = funHandleDemo(@sin,-5)
y =
   -4.8589

```

Notice how `@sin` was a quick way to access the `sin` function without first storing it in a variable using `f = @sin`.

### Using `bsxfun`, `cellfun` and similar functions with anonymous functions

MATLAB has some built-in functions that accept anonymous functions as an input. This is a way to perform many calculations with a minimal number of lines of code. For example `bsxfun`, which performs element-by-element binary operations, that is: it applies a function on two vectors or matrices in an element-by-element fashion. Normally, this would require use of `for`-loops, which often requires preallocation for speed. Using `bsxfun` this process is sped up. The following example illustrates this using `tic` and `toc`, two functions that can be used to time how long code takes. It calculates the difference of every matrix element from the matrix column mean.

```matlab
A = rand(50); % 50-by-50 matrix of random values between 0 and 1

% method 1: slow and lots of lines of code
tic
meanA = mean(A); % mean of every matrix column: a row vector
% pre-allocate result for speed, remove this for even worse performance
result = zeros(size(A));
for j = 1:size(A,1)
    result(j,:) = A(j,:) - meanA;
end
toc
clear result % make sure method 2 creates its own result

% method 2: fast and only one line of code
tic
result = bsxfun(@minus,A,mean(A));
toc

```

Running the example above results in two outputs:

```matlab
Elapsed time is 0.015153 seconds.
Elapsed time is 0.007884 seconds.

```

These lines come from the `toc` functions, which print the elapsed time since the last call to the `tic` function.

The `bsxfun` call applies the function in the first input argument to the other two input arguments. `@minus` is a long name for the same operation as the minus sign would do. A different anonymous function or handle (`@`) to any other function could have been specified, as long as it accepts `A` and `mean(A)` as inputs to generate a meaningful result.

Especially for large amounts of data in large matrices, `bsxfun` can speed up things a lot. It also makes code look cleaner, although it might be more difficult to interpret for people who don't know MATLAB or `bsxfun`. (Note that in MATLAB R2016a and later, many operations that previously used `bsxfun` no longer need them; `A-mean(A)` works directly and can in some cases be even faster.)



## Matrices and Arrays


In MATLAB, the most basic data type is the numeric array. It can be a scalar, a 1-D vector, a 2-D matrix, or an N-D multidimensional array.

```matlab
% a 1-by-1 scalar value
x = 1;

```

To create a row vector, enter the elements inside brackets, separated by spaces or commas:

```matlab
% a 1-by-4 row vector
v = [1, 2, 3, 4];
v = [1 2 3 4];

```

To create a column vector, separate the elements with semicolons:

```matlab
% a 4-by-1 column vector
v = [1; 2; 3; 4];

```

To create a matrix, we enter the rows as before separated by semicolons:

```matlab
% a 2 row-by-4 column matrix
M = [1 2 3 4; 5 6 7 8];

% a 4 row-by-2 column matrix
M = [1 2; ...
     4 5; ...
     6 7; ...
     8 9];

```

Notice you cannot create a matrix with unequal row / column size. All rows must be the same length, and all columns must be the same length:

```matlab
% an unequal row / column matrix
M = [1 2 3 ; 4 5 6 7]; % This is not valid and will return an error

% another unequal row / column matrix
M = [1 2 3; ...
     4   5; ...
     6 7 8; ...
     9   10];     % This is not valid and will return an error

```

To transpose a vector or a matrix, we use the `.'`-operator, or the `'` operator to take its Hermitian conjugate, which is the complex conjugate of its transpose. For real matrices, these two are the same:

```matlab
% create a row vector and transpose it into a column vector
v = [1 2 3 4].';              % v is equal to [1; 2; 3; 4];

% create a 2-by-4 matrix and transpose it to get a 4-by-2 matrix
M = [1 2 3 4; 5 6 7 8].';     % M is equal to [1 5; 2 6; 3 7; 4 8]

% transpose a vector or matrix stored as a variable
A = [1 2; 3 4];
B = A.';                      % B is equal to [1 3; 2 4]

```

For arrays of more than two-dimensions, there is no direct language syntax to enter them literally. Instead we must use functions to construct them (such as `ones`, `zeros`, `rand`) or by manipulating other arrays (using functions such as `cat`, `reshape`, `permute`). Some examples:

```matlab
% a 5-by-2-by-4-by-3 array (4-dimensions)
arr = ones(5, 2, 4, 3);

% a 2-by-3-by-2 array (3-dimensions)
arr = cat(3, [1 2 3; 4 5 6], [7 8 9; 0 1 2]);

% a 5-by-4-by-3-by-2 (4-dimensions)
arr = reshape(1:120, [5 4 3 2]);

```



## Cell arrays


Elements of the same class can often be concatenated into arrays (with a few rare exceptions, e.g. function handles). Numeric scalars, by default of class `double`, can be stored in a matrix.

```matlab
>> A = [1, -2, 3.14, 4/5, 5^6; pi, inf, 7/0, nan, log(0)]
A =
   1.0e+04 *
    0.0001   -0.0002    0.0003    0.0001    1.5625
    0.0003       Inf       Inf       NaN      -Inf

```

Characters, which are of class `char` in MATLAB, can also be stored in array using similar syntax. Such an array is similar to a string in many other programming languages.

```matlab
>> s = ['MATLAB ','is ','fun']
s =
MATLAB is fun

```

Note that despite both of them are using brackets `[` and `]`, the result classes are different. Therefore the operations that can be done on them are also different.

```matlab
>> whos
  Name      Size            Bytes  Class     Attributes

  A         2x5                80  double              
  s         1x13               26  char                

```

In fact, the array `s` is not an array of the strings `'MATLAB '`,`'is '`, and `'fun'`, it is just one string - an array of 13 characters. You would get the same results if it were defined by any of the following:

```matlab
>> s = ['MAT','LAB ','is f','u','n'];
>> s = ['M','A','T','L','A','B,' ','i','s',' ','f','u','n'];

```

A regular MATLAB vector does not let you store a mix of variables of different classes, or a few different strings. This is where the `cell` array comes in handy. This is an array of cells that each can contain some MATLAB object, whose class can be different in every cell if needed. Use curly braces `{` and `}` around the elements to store in a cell array.

```matlab
>> C = {A; s}
C = 
    [2x5 double]
    'MATLAB is fun'
>> whos C
  Name      Size            Bytes  Class    Attributes

  C         2x1               330  cell 

```

Standard MATLAB objects of any classes can be stored together in a cell array. Note that cell arrays require more memory to store their contents.

Accessing the contents of a cell is done using curly braces `{` and `}`.

```matlab
>> C{1}
ans =
   1.0e+04 *
    0.0001   -0.0002    0.0003    0.0001    1.5625
    0.0003       Inf       Inf       NaN      -Inf

```

Note that `C(1)` is different from `C{1}`. Whereas the latter returns the cell's content (and has class `double` in out example), the former returns a cell array which is a sub-array of `C`. Similarly, if `D` were an 10 by 5 cell array, then `D(4:8,1:3)` would return a sub-array of `D` whose size is 5 by 3 and whose class is `cell`. And the syntax `C{1:2}` does not have a single returned object, but rater it returns 2 different objects (similar to a MATLAB function with multiple return values):

```matlab
>> [x,y] = C{1:2}
x =
                         1                        -2                      3.14                       0.8                     15625
          3.14159265358979                       Inf                       Inf                       NaN                      -Inf
y =
MATLAB is fun

```



## Hello World


Open a new blank document in the MATLAB Editor (in recent versions of MATLAB, do this by selecting the Home tab of the toolstrip, and clicking on New Script). The default keyboard shortcut to create a new script is `Ctrl-n`.

Alternatively, typing `edit myscriptname.m` will open the file `myscriptname.m` for editing, or offer to create the file if it does not exist on the MATLAB path.

In the editor, type the following:

```matlab
disp('Hello, World!');

```

Select the Editor tab of the toolstrip, and click Save As. Save the document to a file in the current directory called `helloworld.m`. Saving an untitled file will bring up a dialog box to name the file.

In the MATLAB Command Window, type the following:

```matlab
>> helloworld

```

You should see the following response in the MATLAB Command Window:

```matlab
Hello, World!

```

We see that in the Command Window, we are able to type the names of functions or script files that we have written, or that are shipped with MATLAB, to run them.

Here, we have run the 'helloworld' script. Notice that typing the extension (`.m`) is unnecessary. The instructions held in the script file are executed by MATLAB, here printing 'Hello, World!' using the `disp` function.

Script files can be written in this way to save a series of commands for later (re)use.



## Helping yourself


MATLAB comes with many built-in scripts and functions which range from simple multiplication to image recognition toolboxes. In order to get information about a function you want to use type: `help functionname` in the command line. Lets take the `help` function as an example.

Information on how to use it can be obtained by typing:

`>> help help`

in the command window. This will return information of the usage of function `help`. If the information you are looking for is still unclear you can try the **documentation** page of the function. Simply type:

`>> doc help`

in the command window. This will open the browsable documentation on the page for function `help` providing all the information you need to understand how the 'help' works.

**This procedure works for all built-in functions and symbols.**

When developing your own functions you can let them have their own help section by adding comments at the top of the function file or just after the function declaration.

Example for a simple function `multiplyby2`  saved in file `multiplyby2.m`

```matlab
function [prod]=multiplyby2(num)
% function MULTIPLYBY2 accepts a numeric matrix NUM and returns output PROD 
% such that all numbers are multiplied by 2

    prod=num*2;
end

```

or

```matlab
% function MULTIPLYBY2 accepts a numeric matrix NUM and returns output PROD 
% such that all numbers are multiplied by 2

function [prod]=multiplyby2(num)
    prod=num*2;
end

```

This is very useful when you pick up your code weeks/months/years after having written it.

The `help` and `doc` function provide a lot of information, learning how to use those features will help you progress rapidly and use MATLAB efficiently.



## Scripts and Functions


MATLAB code can be saved in m-files to be reused. m-files have the `.m` extension which is automatically associated with MATLAB. An m-file can contain either a script or functions.

**Scripts**

Scripts are simply program files that execute a series of MATLAB commands in a predefined order.

Scripts do not accept input, nor do scripts return output. Functionally, scripts are equivalent to typing commands directly into the MATLAB command window and being able to replay them.

An example of a script:

```matlab
length = 10;
width = 3;
area = length * width;

```

This script will define `length`, `width`, and `area` in the current workspace with the value `10`, `3`, and `30` respectively.

As stated before, the above script is functionally equivalent to typing the same commands directly into the command window.

```matlab
>> length = 10;
>> width = 3;
>> area = length * width;

```

**Functions**

Functions, when compared to scripts, are much more flexible and extensible. Unlike scripts, functions can accept input and return output to the caller. A function has its own workspace, this means that internal operations of the functions will not change the variables from the caller.

All functions are defined with the same header format:

```matlab
function [output] = myFunctionName(input)

```

The `function` keyword begins every function header. The list of outputs follows. The list of outputs can also be a comma separated list of variables to return.

```matlab
function [a, b, c] = myFunctionName(input)

```

Next is the name of the function that will be used for calling. This is generally the same name as the filename. For example, we would save this function as `myFunctionName.m`.

Following the function name is the list of inputs. Like the outputs, this can also be a comma separated list.

```matlab
function [a, b, c] = myFunctionName(x, y, z)

```

We can rewrite the example script from before as a reusable function like the following:

```matlab
function [area] = calcRecArea(length, width)
   area = length * width;
end

```

We can call functions from other functions, or even from script files. Here is an example of our above function being used in a script file.

```matlab
l = 100;
w = 20;
a = calcRecArea(l, w);

```

As before, we create `l`, `w`, and `a` in the workspace with the values of `100`, `20`, and `2000` respectively.



## Reading Input & Writing Output


Just like all programming language, Matlab is designed to read and write in a large variety of formats. The native library supports a large number of Text,Image,Video,Audio,Data formats with more formats included in each version update - [check here](https://uk.mathworks.com/help/matlab/import_export/supported-file-formats.html) to see the full list of supported file formats and what function to use to import them.

Before you attempt to load in your file, you must ask yourself what do you want the data to become and how you expect the computer to organize the data for you. Say you have a txt/csv file in the following format:

```matlab
Fruit,TotalUnits,UnitsLeftAfterSale,SellingPricePerUnit
Apples,200,67,$0.14
Bananas,300,172,$0.11
Pineapple,50,12,$1.74

```

We can see that the first column is in the format of Strings, while the second, third are Numeric, the last column is in the form of Currency. Let's say we want to find how much revenue we made today using Matlab and first we want to load in this txt/csv file. After checking the link, we can see that String and Numeric type of txt files are handled by `textscan`. So we could try:

```matlab
fileID = fopen('dir/test.txt'); %Load file from dir
C = textscan(fileID,'%s %f %f %s','Delimiter',',','HeaderLines',1); %Parse in the txt/csv

```

where `%s` suggest that the element is a String type, `%f` suggest that the element is a Float type, and that the file is Delimited by ",". The HeaderLines option asks Matlab to skip the First N lines while the 1 immediately after it means to skip the first line (the header line).

Now C is the data we have loaded which is in the form of a Cell Array of 4 cells, each containing the column of data in the txt/csv file.

So first we want to calculate how many fruits we sold today by subtracting the third column from the second column, this can be done by:

```matlab
sold = C{2} - C{3}; %C{2} gives the elements inside the second cell (or the second column)

```

Now we want to multiply this vector by the Price per unit, so first we need to convert that column of Strings into a column of Numbers, then convert it into a Numeric Matrix using Matlab's `cell2mat` the first thing we need to do is to strip-off the "$" sign, there are many ways to do this. The most direct way is using a simple regex:

```matlab
D = cellfun(@(x)(str2num(regexprep(x, '\$',''))), C{4}, 'UniformOutput', false);%cellfun allows us to avoid looping through each element in the cell.

```

Or you can use a loop:

```matlab
for t=1:size(C{4},1)
   D{t} = str2num(regexprep(C{4}{t}, '\$',''));
end

E = cell2mat(D)% converts the cell array into a Matrix

```

The `str2num` function turns the string which had "$" signs stripped into numeric types and `cell2mat` turns the cell of numeric elements into a matrix of numbers

Now we we can multiply the units sold by the cost per unit:

```matlab
revenue = sold .* E; %element-wise product is denoted by .* in Matlab

totalrevenue = sum(revenue);

```



## Data Types


There are [16 fundamental data types](http://www.mathworks.com/help/matlab/matlab_prog/fundamental-matlab-classes.html), or classes, in MATLAB. Each of these classes is in the form of a matrix or array. With the exception of function handles, this matrix or array is a minimum of 0-by-0 in size and can grow to an n-dimensional array of any size. A function handle is always scalar (1-by-1).

Important moment in MATLAB is that you don't need to use any type declaration or dimension statements by default.  When you define new variable MATLAB creates it automatically and allocates appropriate memory space.

Example:

```matlab
a = 123;
b = [1 2 3];
c = '123';

>> whos
  Name      Size            Bytes  Class     Attributes

  a         1x1                 8  double              
  b         1x3                24  double              
  c         1x3                 6  char    

```

If the variable already exists, MATLAB replaces the original data with new one and allocates new storage space if necessary.

**Fundamental data types**

Fundamental data types are: numeric, `logical`, `char`, `cell`, `struct`, `table` and `function_handle`.

[Numeric data types](http://www.mathworks.com/help/matlab/numeric-types.html):

<li>
[Floating-Point numbers](http://www.mathworks.com/help/matlab/matlab_prog/floating-point-numbers.html) (**default**)
MATLAB represents floating-point numbers in either double-precision or single-precision format. The default is double precision, but you can make any number single precision with a simple conversion function:

```matlab
a = 1.23;
b = single(a);

>> whos
  Name      Size            Bytes  Class     Attributes

  a         1x1                 8  double              
  b         1x1                 4  single     

```


</li>
<li>
[Integers](http://www.mathworks.com/help/matlab/matlab_prog/integers.html)
MATLAB has four signed and four unsigned integer classes. Signed types enable you to work with negative integers as well as positive, but cannot represent as wide a range of numbers as the unsigned types because one bit is used to designate a positive or negative sign for the number. Unsigned types give you a wider range of numbers, but these numbers can only be zero or positive.
MATLAB supports 1-, 2-, 4-, and 8-byte storage for integer data. You can save memory and execution time for your programs if you use the smallest integer type that accommodates your data. For example, you do not need a 32-bit integer to store the value 100.

```matlab
a = int32(100);
b = int8(100);

>> whos
  Name      Size            Bytes  Class    Attributes

  a         1x1                 4  int32              
  b         1x1                 1  int8               

```


To store data as an integer, you need to convert from double to the desired integer type. If the number being converted to an integer has a fractional part, MATLAB rounds to the nearest integer. If the fractional part is exactly `0.5`, then from the two equally nearby integers, MATLAB chooses the one for which the absolute value is larger in magnitude.

```matlab
a  = int16(456);

```


</li>
<li>
[`char`](http://www.mathworks.com/help/matlab/characters-and-strings.html)
Character arrays provide storage for text data in MATLAB. In keeping with traditional programming terminology, an array (sequence) of characters is defined as a string. There is no explicit string type in retail releases of MATLAB.
</li>
<li>
logical: logical values of 1 or 0, represent true and false respectively. Use for relational conditions and array indexing. Because it's just TRUE or FALSE it has size of 1 byte.

```matlab
a = logical(1);

```


</li>
<li>
structure. A structure array is a data type that groups variables of different data types using data containers called **fields**. Each field can contain any type of data. Access data in a structure using dot notation of the form structName.fieldName.

```matlab
field1 = 'first';
field2 = 'second';
value1 = [1 2 3 4 5];
value2 = 'sometext';
s = struct(field1,value1,field2,value2);

```


In order to access value1, each of the following syntax are equivalent

```matlab
s.first or s.(field1) or s.('first')

```


We can explicitly access a field we know will exist with the first method, or either pass a string or create a string to access the field in the second example.  The third example is demostrating that the dot parenthases notation takes a string, which is the same one stored in the field1 variable.
</li>

<li>
table variables can be of different sizes and data types, but all variables must have the same number of rows.

```matlab
Age = [15 25 54]';
Height = [176 190 165]';
Name = {'Mike', 'Pete', 'Steeve'}';
T = table(Name,Age, Height);

```


</li>
<li>
cell. It's very useful MATLAB data type: cell array is an array each element of it can be of different data type and size. It's very strong instrument for manipulating data as you wish.

```matlab
a = { [1 2 3], 56, 'art'};

```


or

```matlab
a = cell(3);

```


</li>
<li>
[function handles](http://www.mathworks.com/help/matlab/matlab_prog/creating-a-function-handle.html) stores a pointer to a function (for example, to anonymous function). It allows you to pass a function to another function, or call local functions from outside the main function.
</li>

There are a lot of instruments to work with each data type and also built-in [data type conversion functions](http://www.mathworks.com/help/matlab/data-type-conversion.html) (`str2double`, `table2cell`).

**Additional data types**

There are several additional data types which are useful in some specific cases. They are:

<li>
<p>Date and time: arrays to represent dates, time, and duration.
`datetime('now')` returns `21-Jul-2016 16:30:16`.</p>
</li>
<li>
Categorical arrays: it's data type for storing data with values from a set of discrete categories. Useful for storing nonnumeric data (memory effective). Can be used in a table to select groups of rows.

```matlab
a = categorical({'a' 'b' 'c'});

```


</li>
<li>
Map containers is a data structure that has unique ability to indexing not only through the any scalar numeric values but character vector. Indices into the elements of a Map are called keys. These keys, along with the data values associated with them, are stored within the Map.
</li>
<li>
[Time series](http://www.mathworks.com/help/matlab/ref/timeseries-class.html) are data vectors sampled over time, in order, often at regular intervals. It's useful to store the data connected with timesteps and it has a lot of useful methods to work with.
</li>

