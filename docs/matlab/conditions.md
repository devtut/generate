---
metaTitle: "MATLAB - Conditions"
description: "IF condition, IF-ELSE condition, IF-ELSEIF condition, Nested conditions"
---

# Conditions



## IF condition


Conditions are a fundamental part of almost any part of code. They are used to execute some parts of the code only in some situations, but not other. Let's look at the basic syntax:

```matlab
a = 5;
if a > 10    % this condition is not fulfilled, so nothing will happen
    disp('OK')
end

if a < 10    % this condition is fulfilled, so the statements between the if...end are executed
    disp('Not OK')
end

```

Output:

```matlab
Not OK

```

In this example we see the `if` consists of 2 parts: the condition, and the code to run if the condition is true. The code is everything written after the condition and before the `end` of that `if`. The first condition was not fulfilled and hence the code within it was not executed.

Here is another example:

```matlab
a = 5;
if a ~= a+1        % "~=" means "not equal to"
    disp('It''s true!') % we use two apostrophes to tell MATLAB that the ' is part of the string
end

```

The condition above will always be true, and will display the output `It's true!`.

We can also write:

```matlab
a = 5;
if a == a+1    % "==" means "is equal to", it is NOT the assignment ("=") operator
    disp('Equal')
end

```

This time the condition is always false, so we will never get the output `Equal`.

There is not much use for conditions that are always true or false, though, because if they are always false we can simply delete this part of the code, and if they are always true then the condition is not needed.



## IF-ELSE condition


In some cases we want to run an alternative code if the condition is false, for this we use the optional `else` part:

```matlab
a = 20;
if a < 10
    disp('a smaller than 10')
else
    disp('a bigger than 10')
end

```

Here we see that because `a` is not smaller than `10` the second part of the code, after the `else` is executed and we get the output `a bigger than 10`. Now let's look at another try:

```matlab
a = 10;
if a > 10
    disp('a bigger than 10')
else
    disp('a smaller than 10')
end

```

In this example shows that we did not checked if `a` is indeed smaller than 10, and we get a wrong message because the condition only check the expression as it is, and ANY case that does not equals true (`a = 10`) will cause the second part to be executed.

This type of error is a very common pitfall for both beginners and experienced programmers, especially when conditions become complex, and should be always kept in mind



## IF-ELSEIF condition


Using `else` we can perform some task when the condition is not satisfied. But what if we want to check a second condition in case that the first one was false. We can do it this way:

```matlab
a = 9;
if mod(a,2)==0   % MOD - modulo operation, return the remainder after division of 'a' by 2
    disp('a is even')
else
    if mod(a,3)==0
        disp('3 is a divisor of a')
    end
end

OUTPUT:
3 is a divisor of a

```

This is also called ["nested condition"](http://stackoverflow.com/documentation/matlab/3806/conditions/14308/nested-conditions), but here we have a speciel case that can improve code readability, and reduce the chance for anr error - we can write:

```matlab
a = 9;
if mod(a,2)==0
    disp('a is even')
elseif mod(a,3)==0
    disp('3 is a divisor of a')
end

OUTPUT:
3 is a divisor of a

```

using the `elseif` we are able to check another expression within the same block of condition, and this is not limited to one try:

```matlab
a = 25;
if mod(a,2)==0
    disp('a is even')
elseif mod(a,3)==0
    disp('3 is a divisor of a')
elseif mod(a,5)==0
    disp('5 is a divisor of a')
end

OUTPUT:
5 is a divisor of a

```

Extra care should be taken when choosing to use `elseif` in a row, since **only one** of them will be executed from all the `if` to `end` block. So, in our example if we want to display all the divisors of `a` (from those we explicitly check) the example above won't be good:

```matlab
a = 15;
if mod(a,2)==0
    disp('a is even')
elseif mod(a,3)==0
    disp('3 is a divisor of a')
elseif mod(a,5)==0
    disp('5 is a divisor of a')
end

OUTPUT:
3 is a divisor of a

```

not only 3, but also 5 is a divisor of 15, but the part that check the division by 5 is not reached if any of the expressions above it was true.

Finally, we can add one `else` (and only **one**) after all the `elseif` conditions to execute a code when none of the conditions above are met:

```matlab
a = 11;
if mod(a,2)==0
    disp('a is even')
elseif mod(a,3)==0
    disp('3 is a divisor of a')
elseif mod(a,5)==0
    disp('5 is a divisor of a')
else
    disp('2, 3 and 5 are not divisors of a')
end

OUTPUT:
2, 3 and 5 are not divisors of a

```



## Nested conditions


When we use a condition within another condition we say the conditions are "nested". One special case of nested conditions is given by the [`elseif`](http://stackoverflow.com/documentation/matlab/3806/conditions/14307/if-elseif-condition) option, but there are numerous other ways to use nested conditons. Let's examine the following code:

```matlab
a = 2;
if mod(a,2)==0    % MOD - modulo operation, return the remainder after division of 'a' by 2
    disp('a is even')
    if mod(a,3)==0
        disp('3 is a divisor of a')
        if mod(a,5)==0
            disp('5 is a divisor of a')
        end
    end
else
    disp('a is odd')
end 

```

For `a=2`, the output will be `a is even`, which is correct. For `a=3`, the output will be `a is odd`, which is also correct, but misses the check if 3 is a divisor of `a`. This is because the conditions are nested, so **only** if the first is `true`, than we move to the inner one, and if `a` is odd, none of the inner conditions are even checked. This is somewhat opposite to the use of `elseif` where only if the first condition is `false` than we check the next one. What about checking the division by 5? only a number that has 6 as a divisor (both 2 and 3) will be checked for the division by 5, and we can test and see that for `a=30` the output is:

```matlab
a is even
3 is a divisor of a
5 is a divisor of a

```

We should also notice two things:

1. The position of the `end` in the right place for each `if` is crucial for the set of conditions to work as expected, so indentation is more than a good recommendation here.
1. The position of the `else` statement is also crucial, because we need to know in which `if` (and there could be several of them) we want to do something in case the expression if `false`.

Let's look at another example:

```matlab
for a = 5:10    % the FOR loop execute all the code within it for every a from 5 to 10
    ch = num2str(a);    % NUM2STR converts the integer a to a character
    if mod(a,2)==0
        if mod(a,3)==0
            disp(['3 is a divisor of ' ch])
        elseif  mod(a,4)==0
            disp(['4 is a divisor of ' ch])
        else
            disp([ch ' is even'])
        end
    elseif mod(a,3)==0
        disp(['3 is a divisor of ' ch])
        
    else
        disp([ch ' is odd'])
    end
end

```

And the output will be:

```matlab
5 is odd
3 is a divisor of 6
7 is odd
4 is a divisor of 8
3 is a divisor of 9
10 is even

```

we see that we got only 6 lines for 6 numbers, because the conditions are nested in a way that ensure only one print per number, and also (although can't be seen directly from the output) no extra checks are preformed, so if a number is not even there is no point to check if 4 is one of it divisors.



#### Syntax


- if **expression** ... end
- if **expression** ... else ... end
- if **expression** ... elseif **expression** ... end
- if **expression** ... elseif **expression** ... else ... end



#### Parameters


|Parameter|Description
|---|---|---|---|---|---|---|---|---|---
|**expression**|an expression that has logical meaning

