---
metaTitle: "MATLAB - For loops"
description: "Iterate over columns of matrix, Notice: Weird same counter nested loops., Iterate over elements of vector, Loop 1 to n, Loop over indexes, Nested Loops"
---

# For loops



## Iterate over columns of matrix


If the right-hand side of the assignment is a matrix, then in each iteration the variable is assigned subsequent columns of this matrix.

```matlab
some_matrix = [1, 2, 3; 4, 5, 6]; % 2 by 3 matrix
for some_column = some_matrix
    display(some_column)
end

```

(The row vector version is a normal case of this, because in Matlab a row vector is just a matrix whose columns are size 1.)

The output would display

```matlab
1
4
2
5
3
6

```

i.e. each column of the iterated matrix displayed, each column printed on each call of `display`.



## Notice: Weird same counter nested loops.


This is not something you will see in other programming environments. I came across it some years back and I couldn't understand why it was happening, but after working with MATLAB for some time I was able to figure it out. Look at the code snippet below:

```matlab
for x = 1:10
    for x = 1:10
        fprintf('%d,', x);
    end
    fprintf('\n');
end

```

you wouldn't expect this to work properly but it does, producing the following output:

```matlab
1,2,3,4,5,6,7,8,9,10,
1,2,3,4,5,6,7,8,9,10,
1,2,3,4,5,6,7,8,9,10,
1,2,3,4,5,6,7,8,9,10,
1,2,3,4,5,6,7,8,9,10,
1,2,3,4,5,6,7,8,9,10,
1,2,3,4,5,6,7,8,9,10,
1,2,3,4,5,6,7,8,9,10,
1,2,3,4,5,6,7,8,9,10,
1,2,3,4,5,6,7,8,9,10,

```

The reason is that, as with everything else in MATLAB, the `x` counter is also a matrix—a vector to be precise. As such, `x` is only a reference to an 'array' (a coherent, consecutive memory structure) which is appropriatelly referenced with every consequent loop (nested or not). The fact that the nested loop uses the same identifier makes no difference to how values from that array are referenced. The only problem is that within the nested loop the outer `x` is hidden by the nested (local) `x` and therefore cannot be referenced. However, the functionality of the nested loop structure remains intact.



## Iterate over elements of vector


The right-hand side of the assignment in a `for` loop can be any row vector. The left-hand side of the assignment can be any valid variable name. The `for` loop assigns a different element of this vector to the variable each run.

```matlab
other_row_vector = [4, 3, 5, 1, 2];
for any_name = other_row_vector
    display(any_name)
end

```

The output would display

```matlab
4
3
5
1
2

```

(The `1:n` version is a normal case of this, because in Matlab `1:n` is just syntax for constructing a row vector of `[1, 2, ..., n]`.)

Hence, the two following blocks of code are identical:

```matlab
A = [1 2 3 4 5];
for x = A
  disp(x);
end

```

and

```matlab
for x = 1:5
  disp(x);
end

```

And the following are identical as well:

```matlab
A = [1 3 5 7 9];
for x = A
  disp(x);
end

```

and

```matlab
for x = 1:2:9
  disp(x);
end

```

Any row vector will do. They don't have to be numbers.

```matlab
my_characters = 'abcde';
for my_char = my_characters
    disp(my_char)
end

```

will output

```matlab
a
b
c
d
e

```



## Loop 1 to n


The simplest case is just preforming a task for a fixed known number of times. Say we want to display the numbers between 1 to n, we can write:

```matlab
n = 5;
for k = 1:n
    display(k)
end

```

The loop will execute the inner statement(s), everything between the `for` and the `end`, for `n` times (5 in this example):

```matlab
1
2
3
4
5

```

Here is another example:

```matlab
n = 5;
for k = 1:n
    disp(n-k+1:-1:1) % DISP uses more "clean" way to print on the screen
end

```

this time we use both the `n` and `k` in the loop, to create a "nested" display:

```

5     4     3     2     1

 4     3     2     1

 3     2     1

 2     1

 1

```



## Loop over indexes


```matlab
my_vector = [0, 2, 1, 3, 9];
for i = 1:numel(my_vector)
    my_vector(i) = my_vector(i) + 1;
end

```

Most simple things done with `for` loops can be done faster and easier by vectorized operations. For example, the above loop can be replaced by `my_vector = my_vector + 1`.



## Nested Loops


Loops can be nested, to preform iterated task within another iterated task. Consider the following loops:

```matlab
ch = 'abc';
m = 3;
for c = ch
    for k = 1:m
        disp([c num2str(k)]) % NUM2STR converts the number stored in k to a charachter,
                             % so it can be concataneted with the letter in c
    end
end

```

we use 2 iterators to display all combinations of elements from `abc` and `1:m`, which yields:

```matlab
a1
a2
a3
b1
b2
b3
c1
c2
c3

```

We can also use nested loops to combine between tasks to be done each time, and tasks to be done once in a several iterations:

```matlab
N = 10;
n = 3;
a1 = 0; % the first element in Fibonacci series
a2 = 1; % the secound element in Fibonacci series
for j = 1:N
    for k = 1:n
        an = a1 + a2; % compute the next element in Fibonacci series
        a1 = a2;      % save the previous element for the next iteration
        a2 = an;      % save ht new element for the next iteration
    end
    disp(an) % display every n'th element
end

```

Here we want to compute all the [Fibonacci series](https://en.wikipedia.org/wiki/Fibonacci_number), but to display only the `n`th element each time, so we get

```

  3
   13
   55
   233
   987
   4181
   17711
   75025
   317811
   1346269

```

Another thing we can do is to use the first (outer) iterator within the inner loop. Here is another example:

```matlab
N = 12;
gap = [1 2 3 4 6];
for j = gap
    for k = 1:j:N
        fprintf('%d ',k) % FPRINTF prints the number k proceeding to the next the line
    end
    fprintf('\n')        % go to the next line
end

```

This time we use the nested loop to format the output, and brake the line only when a new gap (`j`) between the elements was introduced. We loop through the gap width in the outer loop and use it within the inner loop to iterate through the vector:

```matlab
1 2 3 4 5 6 7 8 9 10 11 12 
1 3 5 7 9 11 
1 4 7 10 
1 5 9 
1 7 

```



#### Remarks


### Iterate over column vector

A common source of bugs is trying to loop over the elements of a column vector. A column vector is treated like a matrix with one column. (There is actually no distinction in Matlab.) The `for` loop runs once with the loop variable set to the column.

```matlab
% Prints once: [3, 1]
my_vector = [1; 2; 3];
for i = my_vector
    display(size(i))
end

```

### Altering the iteration variable

Altering the iteration variable changes its value for the current iteration, but has no impact on its value in subsequent iterations.

```matlab
% Prints 1, 2, 3, 4, 5
for i = 1:5
    display(i)
    i = 5; % Fail at trying to terminate the loop
end

```

### Special case performance of `a:b` in right-hand side

The basic example treats `1:n` as a normal instance of creating a row vector and then iterating over it. For performance reasons, Matlab actually treats any `a:b` or `a:c:b` specially by not creating the row vector entirely, but instead creating each element one at a time.

This can be detected by slightly altering the syntax.

```matlab
% Loops forever
for i = 1:1e50
end

```

```matlab
% Crashes immediately
for i = [1:1e50]
end

```

