---
metaTitle: "MATLAB - Functions"
description: "nargin, nargout, Base example, Multiple outputs"
---

# Functions



## nargin, nargout


In the body of a function `nargin` and `nargout` indicate respectively the actual number of input and output supplied in the call.

We can for example control the execution of a function based on the number of provided input.

**myVector.m**:

```matlab
function [res] = myVector(a, b, c)
    % Roughly emulates the colon operator

    switch nargin
        case 1
            res = [0:a];
        case 2
            res = [a:b];
        case 3
            res = [a:b:c];
        otherwise
            error('Wrong number of params');
    end
end

```

**terminal:**

```matlab
>> myVector(10)

ans =

    0    1    2    3    4    5    6    7    8    9   10

>> myVector(10, 20)

ans =

   10   11   12   13   14   15   16   17   18   19   20

>> myVector(10, 2, 20)

ans =

   10   12   14   16   18   20

```

In a similar way we can control the execution of a function based on the number of output parameters.

**myIntegerDivision**:

```matlab
function [qt, rm] = myIntegerDivision(a, b)
    qt = floor(a / b);

    if nargout == 2
        rm = rem(a, b);
    end
end

```

**terminal**:

```matlab
>> q = myIntegerDivision(10, 7)

q = 1

>> [q, r] = myIntegerDivision(10, 7)

q = 1
r = 3

```



## Base example


The following MATLAB script shows how to define and call a basic function:

**myFun.m**:

```

   function [out1] = myFun(arg0, arg1)
        out1 = arg0 + arg1;
    end

```

**terminal**:

```

   >> res = myFun(10, 20)

    res =

        30

```



## Multiple outputs


The following MATLAB script shows how to return multiple outputs in a single function:

**myFun.m**:

```

   function [out1, out2, out3] = myFun(arg0, arg1)
        out1 = arg0 + arg1;
        out2 = arg0 * arg1;
        out3 = arg0 - arg1;
    end

```

**terminal**:

```

   >> [res1, res2, res3] = myFun(10, 20)

    res1 =

            30

    res2 =

            200

    res3 =
            -10

```

However MATLAB will return only the first value when assigned to a single variable

```

   >> res = myFun(10, 20)

    res =

            30

```

The following example shows how to get a specific output

```

   >> [~, res] = myFun(10, 20)

    res =

            200

```

