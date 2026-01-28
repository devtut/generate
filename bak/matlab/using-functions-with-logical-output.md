---
metaTitle: "MATLAB - Using functions with logical output"
description: "All and Any with empty arrays"
---

# Using functions with logical output



## All and Any with empty arrays


Special care needs to be taken when there is a possibility that an array become an empty array when it comes to logical operators. It is often expected that if `all(A)` is true then `any(A)` must be true and if `any(A)` is false, `all(A)` must also be false. That is not the case in MATLAB with empty arrays.

```matlab
>> any([])
ans =
     0
>> all([])
ans =
     1

```

So if for example you are comparing all elements of an array with a certain threshold, you need to be aware of the case where the array is empty:

```matlab
>> A=1:10;
>> all(A>5)
ans =
     0
>> A=1:0;
>> all(A>5)
ans =
     1

```

Use the built-in function `isempty` to check for empty arrays:

```matlab
a = [];
isempty(a)
ans =
1

```

