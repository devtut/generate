---
metaTitle: "MATLAB - Common mistakes and errors"
description: "The transpose operators, Do not name a variable with an existing function name, Be aware of floating point inaccuracy, What you see is NOT what you get: char vs cellstring in the command window, Undefined Function or Method X for Input Arguments of Type Y, The use of i or j as imaginary unit, loop indices or common variable., Not enough input arguments, Using `length` for multidimensional arrays, Watch out for array size changes"
---

# Common mistakes and errors



## The transpose operators


- `.'` is the correct way to **transpose** a vector or matrix in MATLAB.
- `'` is the correct way to take the **complex conjugate transpose** (a.k.a. Hermitian conjugate) of a vector or matrix in MATLAB.

Note that for the transpose `.'`, there is a **period** in front of the apostrophe. This is in keeping with the syntax for the other element-wise operations in MATLAB: `*` multiplies **matrices**, `.*` multiplies **elements of matrices** together. The two commands are very similar, but conceptually very distinct. Like other MATLAB commands, these operators are "syntactical sugar" that gets turned into a "proper" function call at runtime. Just as `==` becomes an evaluation of the [eq](http://uk.mathworks.com/help/matlab/ref/eq.html) function, think of `.'` as the shorthand for [`transpose`](http://www.mathworks.com/help/matlab/ref/transpose.html). If you would only write `'` (without the point), you are in fact using the [`ctranspose`](http://www.mathworks.com/help/matlab/ref/ctranspose.html) command instead, which calculates the [complex conjugate transpose](http://www.mathworks.com/help/matlab/ref/ctranspose.html#buaudse-6), which is also known as the [Hermitian conjugate](http://en.wikipedia.org/wiki/Hermitian_conjugate), often used in physics. As long as the transposed vector or matrix is real-valued, the two operators produce the same result. But as soon as we deal with [complex numbers](http://www.mathworks.com/help/matlab/complex-numbers.html), we will inevitably run into problems if we do not use the "correct" shorthand. What "correct" is depends on your application.

Consider the following example of a matrix `C` containing complex numbers:

```matlab
>> C = [1i, 2; 3*1i, 4]
C =
   0.0000 + 1.0000i   2.0000 + 0.0000i
   0.0000 + 3.0000i   4.0000 + 0.0000i

```

Let's take the **transpose** using the shorthand `.'` (with the period). The output is as expected, the transposed form of `C`.

```matlab
>> C.'
ans =
   0.0000 + 1.0000i   0.0000 + 3.0000i
   2.0000 + 0.0000i   4.0000 + 0.0000i

```

Now, let's use `'` (without the period). We see, that in addition to the transposition, the complex values have been transformed to their **complex conjugates** as well.

```matlab
>> C'
ans =
   0.0000 - 1.0000i   0.0000 - 3.0000i
   2.0000 + 0.0000i   4.0000 + 0.0000i

```

To sum up, if you intend to calculate the Hermitian conjugate, the complex conjugate transpose, then use `'` (without the period). If you just want to calculate the transpose without complex-conjugating the values, use `.'` (with the period).



## Do not name a variable with an existing function name


There is already a function [`sum()`](http://uk.mathworks.com/help/matlab/ref/sum.html). As a result, if we name a variable with the same name

```matlab
sum = 1+3;

```

and if we try to use the function while the variable still exists in the workspace

```matlab
A = rand(2);
sum(A,1)

```

we will get the cryptic **error**:

```matlab
Subscript indices must either be real positive integers or logicals.

```

[`clear()`](http://uk.mathworks.com/help/matlab/ref/clear.html) the variable first and then use the function

```matlab
clear sum

sum(A,1)
ans =
       1.0826       1.0279

```

How can we check if a function already exists to avoid this conflict?

Use [`which()`](http://uk.mathworks.com/help/matlab/ref/which.html) with the `-all` flag:

```matlab
which sum -all
sum is a variable.
built-in (C:\Program Files\MATLAB\R2016a\toolbox\matlab\datafun\@double\sum)   % Shadowed double method
...

```

This output is telling us that `sum` is first a variable and that the following methods (functions) are shadowed by it, i.e. MATLAB will first try to apply our syntax to the variable, rather than using the method.



## Be aware of floating point inaccuracy


Floating-point numbers cannot represent all real numbers. This is known as floating point inaccuracy.

There are infinitely many floating points numbers and they can be infinitely long (e.g. `π`), thus being able to represent them perfectly would require infinitely amount of memory. Seeing this was a problem, a special representation for "real number" storage in computer was designed, the [IEEE 754 standard](https://en.wikipedia.org/wiki/IEEE_floating_point). In short, it describes how computers store this type of numbers, with an exponent and mantissa, as,

`floatnum = sign * 2^exponent * mantissa`

With limited amount of bits for each of these, only a finite precision can be achieved. The smaller the number, smaller the gap between possible numbers (and vice versa!). You can try your real numbers [in this online demo](http://www.h-schmidt.net/FloatConverter/IEEE754.html).

Be aware of this behavior and try to avoid all floating points comparison and their use as stopping conditions in loops. See below two examples:

### **Examples: Floating point comparison done WRONG:**

```matlab
>> 0.1 + 0.1 + 0.1  == 0.3

ans =

  logical

   0

```

It is poor practice to use floating point comparison as shown by the precedent example. You can overcome it by taking the absolute value of their difference and comparing it to a (small) tolerance level.

Below is another example, where a floating point number is used as a stopping condition in a while loop:**

```matlab
k = 0.1;
while k <= 0.3 
  disp(num2str(k));
  k = k + 0.1;
end

% --- Output: ---
0.1
0.2

```

It misses the last expected loop (`0.3 <= 0.3`).

### **Example: Floating point comparison done RIGHT:**

```matlab
x = 0.1 + 0.1 + 0.1;
y = 0.3;
tolerance = 1e-10; % A "good enough" tolerance for this case.

if ( abs( x - y ) <= tolerance )
  disp('x == y');
else
  disp('x ~= y');
end

% --- Output: ---
x == y

```

Several things to note:

- As expected, now `x` and `y` are treated as equivalent.
- In the example above, the choice of tolerance was done arbitrarily. Thus, the chosen value might not be suitable for all cases (especially when working with much smaller numbers). Choosing the bound **intelligently** can be done using the [`eps`](https://www.mathworks.com/help/matlab/ref/eps.html) function, i.e. `N*eps(max(x,y))`, where `N` is some problem-specific number. A reasonable choice for `N`, which is also permissive enough, is `1E2` (even though, in the above problem `N=1` would also suffice).

### **Further reading:**

See these questions for more information about floating point inaccuracy:

- [Why is 24.0000 not equal to 24.0000 in MATLAB?](http://stackoverflow.com/questions/686439/why-is-24-0000-not-equal-to-24-0000-in-matlab)
- [Is floating point math broken?](http://stackoverflow.com/questions/588004/is-floating-point-math-broken)



## What you see is NOT what you get: char vs cellstring in the command window


This a basic example aimed at new users. It does not focus on explaining the difference between `char` and `cellstring`.

It might happen that you want to get rid of the `'` in your strings, although you never added them. In fact, those are **artifacts** that the **command window** uses  to distinguish between some types.

A [string](http://uk.mathworks.com/help/matlab/ref/strings.html) will print

```matlab
s = 'dsadasd'
s =
dsadasd

```

A [cellstring](http://uk.mathworks.com/help/matlab/ref/cellstr.html) will print

```matlab
c = {'dsadasd'};
c = 
    'dsadasd'

```

Note how the **single quotes** and the **indentation** are artifacts to notify us that `c` is a `cellstring` rather than a `char`. The string is in fact contained in the cell, i.e.

```matlab
c{1}
ans =
dsadasd

```



## Undefined Function or Method X for Input Arguments of Type Y


This is MATLAB's long-winded way of saying that it cannot find the function that you're trying to call. There are a number of reasons you could get this error:

### That function was introduced **after** your current version of MATLAB

The MATLAB online documentation provides a very nice feature which allows you to determine in what version a given function was introduced. It is located in the bottom left of every page of the documentation:

[<img src="http://i.stack.imgur.com/W1ZaW.png" alt="enter image description here" />](http://i.stack.imgur.com/W1ZaW.png)

Compare this version with your own current version ([`ver`](http://www.mathworks.com/help/matlab/ref/ver.html)) to determine if this function is available in your particular version. If it's not, try searching the [archived versions of the documentation](http://www.mathworks.com/help/doc-archives.html) to find a suitable alternative in your version.

### You don't have that toolbox!

The base MATLAB installation has a large number of functions; however, more specialized functionality is packaged within toolboxes and sold separately by the Mathworks. The documentation for **all** toolboxes is visible whether you have the toolbox or not so be sure to check and see if you have the appropriate toolbox.

To check which toolbox a given function belongs to, look to the top left of the online documentation to see if a specific toolbox is mentioned.

[<img src="http://i.stack.imgur.com/JyvMG.png" alt="enter image description here" />](http://i.stack.imgur.com/JyvMG.png)

You can then determine which toolboxes your version of MATLAB has installed by issuing the [`ver`](http://www.mathworks.com/help/matlab/ref/ver.html) command which will print a list of all installed toolboxes.

If you do not have that toolbox installed and want to use the function, you will need to purchase a license for that particular toolbox from The Mathworks.

### MATLAB cannot locate the function

If MATLAB still can't find your function, then it must be a user-defined function. It is possible that it lives in another directory and that directory should be [added to the search path](http://www.mathworks.com/help/matlab/matlab_env/add-remove-or-reorder-folders-on-the-search-path.html) for your code to run. You can check whether MATLAB can locate your function by using [`which`](http://www.mathworks.com/help/matlab/ref/which.html) which should return the path to the source file.



## The use of "i" or "j" as imaginary unit, loop indices or common variable.


### Recommendation

Because the symbols `i` and `j` can represent significantly different things in MATLAB, their use as loop indices has split the MATLAB user community since ages. While some historic performance reasons could help the balance lean to one side, this is no longer the case and now the choice rest entirely on you and the coding practices you choose to follow.

The current official recommendations from Mathworks are:

> 
<ul>
- Since `i` is a function, it can be overridden and used as a variable.    However, it is best to avoid using `i` and `j` for variable names if you intend to use them in complex arithmetic.
- For speed and improved robustness in complex arithmetic, use `1i` and    `1j` instead of `i` and `j`.
</ul>


### Default

In MATLAB, by default, the letters [`i`](http://mathworks.com/help/matlab/ref/i.html) and [`j`](http://uk.mathworks.com/help/matlab/ref/j.html) are built-in `function` names, which both refer to the imaginary unit in the complex domain.

So by default `i = j = sqrt(-1)`.

```matlab
>> i
ans =
   0.0000 + 1.0000i
>> j
ans =
   0.0000 + 1.0000i

```

and as you should expect:

```matlab
>> i^2
ans =
    -1

```

### Using them as a variable (for loop indices or other variable)

MATLAB allows using built-in function name as a standard variable. In this case the symbol used will not point to the built-in function any more but to your own user defined variable. This practice, however, is not generally recommended as it can lead to confusion, difficult debugging and maintenance (**see other example [do-not-name-a-variable-with-an-existing-function-name](http://stackoverflow.com/documentation/matlab/973/common-mistakes-and-errors/3168/do-not-name-a-variable-with-an-existing-function-name)**).

If you are ultra pedantic about respecting conventions and best practices, you will avoid using them as loop indices in this language. However, it is allowed by the compiler and perfectly functional so you may also choose to keep old habits and use them as loop iterators.

```matlab
>> A = nan(2,3);
>> for i=1:2        % perfectly legal loop construction
       for j = 1:3
        A(i, j) = 10 * i + j;
       end
   end

```

Note that loop indices do not go out of scope at the end of the loop, so they keep their new value.

```matlab
>> [ i ; j ]
ans =
     2
     3

```

In the case you use them as variable, make sure **they are initialised** before they are used. In the loop above MATLAB initialise them automatically when it prepare the loop, but if not initialised properly you can quickly see that you may inadvertently introduce `complex` numbers in your result.

If later on, you need to undo the shadowing of the built-in function (=e.g. you want `i` and `j` to represent the imaginary unit again), you can `clear` the variables:

```matlab
>> clear i j

```

You understand now the Mathworks reservation about using them as loop indices **if you intend to use them in complex arithmetic**. Your code would be riddled with variable initialisations and `clear` commands, best way to confuse the most serious programmer (**yes you there!...**) and program accidents waiting to happen.

If no complex arithmetic is expected, the use of `i` and `j` is perfectly functional and there is no performance penalty.

### Using them as imaginary unit:

If your code has to deal with `complex` numbers, then `i` and `j` will certainly come in handy. However, for the sake of disambiguation and even for performances, it is recommended to use the full form instead of the shorthand syntax. The full form is `1i` (or `1j`).

```matlab
>> [ i ; j ; 1i ; 1j]
ans =
   0.0000 + 1.0000i
   0.0000 + 1.0000i
   0.0000 + 1.0000i
   0.0000 + 1.0000i

```

They do represent the same value `sqrt(-1)`, but the later form:

- is more explicit, in a semantic way.
<li>is more maintainable (someone looking at your code later will not
have to read up the code to find whether `i` or `j` was a variable or
the imaginary unit).</li>
- is faster (source: Mathworks).

Note that the full syntax `1i` is valid with any number preceding the symbol:

```matlab
>> a = 3 + 7.8j
a =
   3.0000 + 7.8000i

```

This is the only function which you can stick with a number without an operator between them.

### Pitfalls

While their use as **imaginary unit** **OR** **variable** is perfectly legal, here is just a small example of how confusing it could get if both usages get mixed:

Let's override `i` and make it a variable:

```matlab
>> i=3
i =
     3

```

Now `i` is a **variable** (holding the value `3`), but we only overrid the **shorthand** notation of the imaginary unit, the full form is still interpreted correctly:

```matlab
>> 3i
ans =
   0.0000 + 3.0000i

```

Which now lets us build the most obscure formulations. I let you assess the readability of all the following constructs:

```matlab
>> [ i ; 3i ; 3*i ; i+3i ; i+3*i ]
ans =
   3.0000 + 0.0000i
   0.0000 + 3.0000i
   9.0000 + 0.0000i
   3.0000 + 3.0000i
  12.0000 + 0.0000i

```

As you can see, each value in the array above return a different result. While each result is valid (provided that was the initial intent), most of you will admit that it would be a proper nightmare to read a code riddled with such constructs.



## Not enough input arguments


Often beginning MATLAB developers will use MATLAB's editor to write and edit code, in particular custom functions with inputs and outputs.  There is a **Run** button at the top that is available in recent versions of MATLAB:

[<img src="http://i.stack.imgur.com/Spgbr.png" alt="enter image description here" />](http://i.stack.imgur.com/Spgbr.png)

Once the developer finishes with the code, they are often tempted to push the **Run** button.  For some functions this will work fine, but for others they will receive a `Not enough input arguments` error and be puzzled about why the error occurs.

The reason why this error may not happen is because you wrote a MATLAB script or a function that takes in no input arguments. Using the **Run** button will run a test script or run a function assuming no input arguments.  If your function requires input arguments, the `Not enough input arguments` error will occur as you have written a functions that expects inputs to go inside the function.  Therefore, you cannot expect the function to run by simply pushing the **Run** button.

To demonstrate this issue, suppose we have a function `mult` that simply multiplies two matrices together:

```matlab
function C = mult(A, B)
    C = A * B;
end

```

In recent versions of MATLAB, if you wrote this function and pushed the **Run** button, it will give you the error we expect:

```matlab
>> mult
Not enough input arguments.

Error in mult (line 2)
    C = A * B;

```

There are two ways to resolve this issue:

### Method #1 - Through the Command Prompt

Simply create the inputs you need in the Command Prompt, then run the function using those inputs you have created:

```matlab
A = rand(5,5);
B = rand(5,5);
C = mult(A,B);

```

### Method #2 - Interactively through the Editor

Underneath the **Run** button, there is a dark black arrow. If you click on that arrow, you can specify the variables you would like to get from the MATLAB workspace by typing the way you want to call the function exactly as how you have seen in method #1. Be sure that the variables you are specifying inside the function exist in the MATLAB workspace:

<img src="http://i.stack.imgur.com/iWOWg.jpg" alt="" />



## Using `length` for multidimensional arrays


A common mistake MATLAB coders have, is using the `length` function for matrices (as opposed to **vectors**, for which it is intended). The `length` function, as mentioned in [its documentation](https://www.mathworks.com/help/matlab/ref/length.html), "**returns the length of the largest array dimension**" of the input.

For vectors, the return value of `length` has two different meanings:

1. The total number of elements in the vector.
1. The largest dimension of the vector.

Unlike in vectors, the above values would not be equal for arrays of more than one non-singleton (i.e. whose size is larger than `1`) dimension. This is why using `length` for matrices is ambiguous. Instead, using one of the following functions is encouraged, even when working with vectors, to make the intention of the code perfectly clear:

1. [`size(A)`](https://www.mathworks.com/help/matlab/ref/size.html) - returns a row vector whose elements contain the amount of elements along the corresponding dimension of `A`.
1. [`numel(A)`](https://www.mathworks.com/help/matlab/ref/numel.html) - returns the number of elements in `A`. Equivalent to `prod(size(A))`.
1. [`ndims(A)`](https://www.mathworks.com/help/matlab/ref/ndims.html) - returns the number of dimensions in the array `A`. Equivalent to `numel(size(A))`.

This is especially important when writing "future-proof", [vectorized](http://stackoverflow.com/documentation/matlab/750/vectorization#t=201701240948060515351) library  functions, whose inputs are not known in advance, and can have various sizes and shapes.



## Watch out for array size changes


Some common operations in MATLAB, like **differentiation** or **integration**, output results that have a different amount of elements than the input data has. This fact can easily be overlooked, which would usually cause errors like `Matrix dimensions must agree`. Consider the following example:

```matlab
t = 0:0.1:10;        % Declaring a time vector
y = sin(t);          % Declaring a function

dy_dt = diff(y);     % calculates dy/dt for y = sin(t)

```

Let's say we want to plot these results. We take a look at the array sizes and see:

```matlab
size(y) is 1x101
size(t) is 1x101

```

But:

```matlab
size(dy_dt) is 1x100

```

The array is one element shorter!

Now imagine you have measurement data of positions over time and want to calculate **jerk(t)**, you will get an array 3 elements less than the time array (because the jerk is the position differentiated 3 times).

```matlab
vel = diff(y);       % calculates velocity vel=dy/dt for y = sin(t)  size(vel)=1x100
acc = diff(vel);     % calculates acceleration acc=d(vel)/dt         size(acc)=1x99
jerk = diff(acc);    % calculates jerk jerk=d(acc)/dt                size(jerk)=1x98   

```

And then operations like:

```matlab
x = jerk .* t;          % multiplies jerk and t element wise

```

return errors, because the matrix dimensions do not agree.

To calculate operations like above you have to adjust the bigger array size to fit the smaller one. You could also run a regression (`polyfit`) with your data to get a polynomial for your data.

### Dimension Mismatch Errors

**Dimension mismatch** errors typically appear when:

- Not paying attention to the shape of returned variables from function/method calls. In many inbuilt MATLAB functions, matrices are converted into vectors to speed up the calculations, and the returned variable might still be a vector rather than the matrix we expected. This is also a common scenario when [logical masking](http://stackoverflow.com/documentation/matlab/750/vectorization/9514/logical-masking#t=20170124102350965011) is involved.
- Using incompatible array sizes while invoking [implicit array expansion](http://stackoverflow.com/documentation/matlab/750/vectorization/23082/implicit-array-expansion-broadcasting-r2016b#t=201701240947543973603).

