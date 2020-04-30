---
metaTitle: "Code Contracts and Assertions"
description: "Assertions to check logic should always be true"
---

# Code Contracts and Assertions



## Assertions to check logic should always be true


Assertions are used not to perform testing of input parameters, but to verify that program flow is corect -- i.e., that you can make certain assumptions about your code at a certain point in time. In other words: a test done with `Debug.Assert` should **always** assume that the value tested is `true`.

Debug.Assert only executes in DEBUG builds; it is filtered out of RELEASE builds. It must be considered a debugging tool in addition to unit testing and not as a replacement of code contracts or input validation methods.

For instance, this is a good assertion:

```cs
var systemData = RetrieveSystemConfiguration();
Debug.Assert(systemData != null);

```

Here assert is a good choice because we can assume that RetrieveSystemConfiguration() will return a valid value and will never return null.

Here is another good example:

```cs
UserData user = RetrieveUserData();
Debug.Assert(user != null);
Debug.Assert(user.Age > 0);
int year = DateTime.Today.Year - user.Age;

```

First, we may assume that RetrieveUserData() will return a valid value. Then, before using the Age property, we verify the assumption (which should always be true) that the age of the user is strictly positive.

This is a bad example of assert:

```cs
string input = Console.ReadLine();
int age = Convert.ToInt32(input);
Debug.Assert(age > 16);
Console.WriteLine("Great, you are over 16");

```

Assert is not for input validation because it is incorrect to assume that this assertion will always be true. You must use input validation methods for that. In the case above, you should also verify that the input value is a number in the first place.

