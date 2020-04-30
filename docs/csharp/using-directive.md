---
metaTitle: "Using Directive"
description: "Access Static Members of a Class, Associate an Alias to Resolve Conflicts, Using alias directives, Basic Usage, Reference a Namespace, Associate an Alias with a Namespace"
---

# Using Directive



## Access Static Members of a Class


Allows you to import a specific type and use the type's static members without qualifying them with the type name. This shows an example using static methods:

```cs
using static System.Console;

// ...

string GetName()
{
    WriteLine("Enter your name.");
    return ReadLine();
}

```

And this shows an example using static properties and methods:

```cs
using static System.Math;

namespace Geometry
{
    public class Circle
    {
        public double Radius { get; set; };

        public double Area => PI * Pow(Radius, 2);
    }
}

```



## Associate an Alias to Resolve Conflicts


If you are using multiple namespaces that may have same-name classes(such as `System.Random` and `UnityEngine.Random`), you can use an alias to specify that `Random` comes from one or the other without having to use the entire namespace in the call.

For instance:

```cs
using UnityEngine;
using System;

Random rnd = new Random();

```

This will cause the compiler to be unsure which `Random` to evaluate the new variable as.  Instead, you can do:

```cs
using UnityEngine;
using System;
using Random = System.Random;

Random rnd = new Random();

```

This doesn't preclude you from calling the other by it's fully qualified namespace, like this:

```cs
using UnityEngine;
using System;
using Random = System.Random;

Random rnd = new Random();
int unityRandom = UnityEngine.Random.Range(0,100);

```

`rnd` will be a `System.Random` variable and `unityRandom` will be a `UnityEngine.Random` variable.



## Using alias directives


You can use `using` in order to set an alias for a namespace or type. More detail can be found in [here](https://msdn.microsoft.com/en-us/library/aa664765(v=vs.71).aspx).

Syntax:

```cs
using <identifier> = <namespace-or-type-name>;

```

Example:

```cs
using NewType = Dictionary<string, Dictionary<string,int>>;
NewType multiDictionary = new NewType();
//Use instances as you are using the original one
multiDictionary.Add("test", new Dictionary<string,int>());

```



## Basic Usage


```cs
using System;
using BasicStuff = System;
using Sayer = System.Console;
using static System.Console;  //From C# 6

class Program
{
    public static void Main()
    {
        System.Console.WriteLine("Ignoring usings and specifying full type name");
        Console.WriteLine("Thanks to the 'using System' directive");
        BasicStuff.Console.WriteLine("Namespace aliasing");
        Sayer.WriteLine("Type aliasing");
        WriteLine("Thanks to the 'using static' directive (from C# 6)");
    }
}

```



## Reference a Namespace


```cs
using System.Text;
//allows you to access classes within this namespace such as StringBuilder
//without prefixing them with the namespace.  i.e:

//...
var sb = new StringBuilder();
//instead of
var sb = new System.Text.StringBuilder();

```



## Associate an Alias with a Namespace


```cs
using st = System.Text;
//allows you to access classes within this namespace such as StringBuilder
//prefixing them with only the defined alias and not the full namespace.  i.e:

//...
var sb = new st.StringBuilder();
//instead of
var sb = new System.Text.StringBuilder();

```



#### Remarks


The `using` keyword is both a directive (this topic) and a statement.

For the `using` statement (i.e. to encapsulate the scope of an `IDisposable` object, ensuring that outside of that scope the object becomes cleanly disposed) please see [Using Statement](http://stackoverflow.com/documentation/c%23/38/using-statement).

