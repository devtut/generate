---
metaTitle: "C# | Recursion"
description: "Recursion in plain English, Fibonacci Sequence, Recursively describe an object structure, Using Recursion to Get Directory Tree, PowerOf calculation, Factorial calculation"
---

# Recursion




## Recursion in plain English


Recursion can be defined as:

> 
A method that calls itself until a specific condition is met.


An excellent and simple example of recursion is a method that will get the factorial of a given number:

```cs
public int Factorial(int number)
{
    return number == 0 ? 1 : n * Factorial(number - 1);
}

```

In this method, we can see that the method will take an argument, `number`.

Step by step:

Given the example, executing `Factorial(4)`

1. Is `number (4) == 1`?
1. No? return `4 * Factorial(number-1)` (3)
1. Because the method is called once again, it now repeats the first step using `Factorial(3)` as the new argument.
1. This continues until `Factorial(1)` is executed and `number (1) == 1` returns 1.
1. Overall, the calculation "builds up" `4 * 3 * 2 * 1` and finally returns 24.

The key to understanding recursion is that the method calls a **new instance** of itself. After returning, the execution of the calling instance continues.



## Fibonacci Sequence


You can calculate a number in the Fibonacci sequence using recursion.

Following the math theory of F(n) = F(n-2) + F(n-1), for any i > 0,

```cs
// Returns the i'th Fibonacci number
public int fib(int i) {
    if(i <= 2) {
        // Base case of the recursive function.
        // i is either 1 or 2, whose associated Fibonacci sequence numbers are 1 and 1.
        return 1;
    }
    // Recursive case. Return the sum of the two previous Fibonacci numbers.
    // This works because the definition of the Fibonacci sequence specifies
    // that the sum of two adjacent elements equals the next element.
    return  fib(i - 2) + fib(i - 1);
    
}

fib(10); // Returns 55

```



## Recursively describe an object structure


Recursion is when a method calls itself. Preferably it will do so until a specific condition is met and then it will exit the method normally, returning to the point from which the method was called. If not, a stack overflow exception might occur due to too many recursive calls.

```cs
/// <summary>
/// Create an object structure the code can recursively describe
/// </summary>
public class Root
{
    public string Name { get; set; }
    public ChildOne Child { get; set; }
}
public class ChildOne
{
    public string ChildOneName { get; set; }
    public ChildTwo Child { get; set; }
}
public class ChildTwo
{
    public string ChildTwoName { get; set; }
}
/// <summary>
/// The console application with the recursive function DescribeTypeOfObject
/// </summary>
public class Program
{
    static void Main(string[] args)
    {
        // point A, we call the function with type 'Root'
        DescribeTypeOfObject(typeof(Root));
        Console.WriteLine("Press a key to exit");
        Console.ReadKey();
    }

    static void DescribeTypeOfObject(Type type)
    {
        // get all properties of this type
        Console.WriteLine($"Describing type {type.Name}");
        PropertyInfo[] propertyInfos = type.GetProperties();
        foreach (PropertyInfo pi in propertyInfos)
        {
            Console.WriteLine($"Has property {pi.Name} of type {pi.PropertyType.Name}");
            // is a custom class type? describe it too
            if (pi.PropertyType.IsClass && !pi.PropertyType.FullName.StartsWith("System."))
            {
                // point B, we call the function type this property
                DescribeTypeOfObject(pi.PropertyType);
            }
        }
        // done with all properties
        // we return to the point where we were called
        // point A for the first call
        // point B for all properties of type custom class
    }
}

```



## Using Recursion to Get Directory Tree


One of the uses of recursion is to navigate through a hierarchical data structure, like a file system directory tree, without knowing how many levels the tree has or the number of objects on each level. In this example, you will see how to use recursion on a directory tree to find all sub-directories of a specified directory and print the whole tree to the console.

```cs
internal class Program
{
    internal const int RootLevel = 0;
    internal const char Tab = '\t';

    internal static void Main()
    {
        Console.WriteLine("Enter the path of the root directory:");
        var rootDirectorypath = Console.ReadLine();

        Console.WriteLine(
            $"Getting directory tree of '{rootDirectorypath}'");

        PrintDirectoryTree(rootDirectorypath);
        Console.WriteLine("Press 'Enter' to quit...");
        Console.ReadLine();
    }

    internal static void PrintDirectoryTree(string rootDirectoryPath)
    {
        try
        {
            if (!Directory.Exists(rootDirectoryPath))
            {
                throw new DirectoryNotFoundException(
                    $"Directory '{rootDirectoryPath}' not found.");
            }

            var rootDirectory = new DirectoryInfo(rootDirectoryPath);
            PrintDirectoryTree(rootDirectory, RootLevel);
        }
        catch (DirectoryNotFoundException e)
        {
            Console.WriteLine(e.Message);
        }
    }

    private static void PrintDirectoryTree(
        DirectoryInfo directory, int currentLevel)
    {
        var indentation = string.Empty;
        for (var i = RootLevel; i < currentLevel; i++)
        {
            indentation += Tab;
        }

        Console.WriteLine($"{indentation}-{directory.Name}");
        var nextLevel = currentLevel + 1;
        try
        {
            foreach (var subDirectory in directory.GetDirectories())
            {
                PrintDirectoryTree(subDirectory, nextLevel);
            }
        }
        catch (UnauthorizedAccessException e)
        {
            Console.WriteLine($"{indentation}-{e.Message}");
        }
    }
}

```

This code is somewhat more complicated than the bare minimum to complete this task, as it includes exception checking to handle any issues with getting the directories. Below you will find a break-down of the code into smaller segments with explanations of each.

`Main`:

The main method takes an input from a user as a string, which is to be used as the path to the root directory. It then calls the `PrintDirectoryTree` method with this string as the parameter.

`PrintDirectoryTree(string)`:

This is the first of two methods that handle the actual directory tree printing. This method takes a string representing the path to the root directory as a parameter. It checks if the path is an actual directory, and if not, throws a `DirectoryNotFoundException` which is then handled in the catch block. If the path is a real directory, a `DirectoryInfo` object `rootDirectory` is created from the path, and the second `PrintDirectoryTree` method is called with the `rootDirectory` object and `RootLevel`, which is an integer constant with a value of zero.

`PrintDirectoryTree(DirectoryInfo, int)`:

This second method handles the brunt of the work. It takes a `DirectoryInfo` and an integer as parameters. The `DirectoryInfo` is the current directory, and the integer is the depth of the directory relative to the root. For ease of reading, the output is indented for each level deep the current directory is, so that the output looks like this:

```cs
-Root
    -Child 1
    -Child 2
        -Grandchild 2.1
    -Child 3

```

Once the current directory is printed, its sub directories are retrieved, and this method is then called on each of them with a depth level value of one more than the current. That part is the recursion: the method calling itself. The program will run in this manner until it has visited every directory in the tree. When it reached a directory with no sub directories, the method will return automatically.

This method also catches an `UnauthorizedAccessException`, which is thrown if any of the sub directories of the current directory are protected by the system. The error message is printed at the current indentation level for consistency.

The method below provides a more basic approach to this problem:

```cs
internal static void PrintDirectoryTree(string directoryName)
{
    try
    {
        if (!Directory.Exists(directoryName)) return;
        Console.WriteLine(directoryName);
        foreach (var d in Directory.GetDirectories(directoryName))
        {
            PrintDirectoryTree(d);
        }
    }
    catch (Exception e)
    {
        Console.WriteLine(e.Message);
    }
}

```

This does not include the specific error checking or output formatting of the first approach, but it effectively does the same thing. Since it only uses strings as opposed to `DirectoryInfo`, it cannot provide access to other directory properties like permissions.



## PowerOf calculation


Calculating the power of a given number can be done recursively as well.
Given a base number `n` and exponent `e`, we need to make sure to split the problem in chunks by decreasing the exponent `e`.

Theoretical Example:

- 2² = 2x2
<li>2³ = 2x2x2
or, 2³ = 2² x 2<br/>In there lies the secret of our recursive algorithm (see the code below). This is about taking the problem and separating it into smaller and simpler to solve chunks.</li>
<li>**Notes**
<ul>
- when the base number is 0, we have to be aware to return 0 as 0³ = 0 x 0 x 0
- when the exponent is 0, we have to be aware to always return 1, as this is a mathematical rule.

Code Example:

```cs
public int CalcPowerOf(int b, int e) {
    if (b == 0) { return 0; } // when base is 0, it doesn't matter, it will always return 0
    if (e == 0) { return 1; } // math rule, exponent 0 always returns 1
    return b * CalcPowerOf(b, e - 1); // actual recursive logic, where we split the problem, aka: 2³ = 2 * 2² etc..
}

```

Tests in xUnit to verify the logic:<br/>
Although this is not necessary, it's always good to write tests to verify your logic. I include those here written in the [xUnit framework](https://xunit.github.io/).

```cs

   [Theory]
    [MemberData(nameof(PowerOfTestData))]
    public void PowerOfTest(int @base, int exponent, int expected) {
        Assert.Equal(expected, CalcPowerOf(@base, exponent));
    }

    public static IEnumerable<object[]> PowerOfTestData() {
        yield return new object[] { 0, 0, 0 };
        yield return new object[] { 0, 1, 0 };
        yield return new object[] { 2, 0, 1 };
        yield return new object[] { 2, 1, 2 };
        yield return new object[] { 2, 2, 4 };
        yield return new object[] { 5, 2, 25 };
        yield return new object[] { 5, 3, 125 };
        yield return new object[] { 5, 4, 625 };
}

```



## Factorial calculation


The factorial of a number (denoted with !, as for instance 9!) is the multiplication of that number with the factorial of one lower. So, for instance, 9! = 9 x 8! = 9 x 8 x 7! = 9 x 8 x 7 x 6 x 5 x 4 x 3 x 2 x 1.

So in code that becomes, using recursion:

```cs
long Factorial(long x)
{
    if (x < 1)
    {
        throw new OutOfRangeException("Factorial can only be used with positive numbers.");
    }

    if (x == 1)
    {
        return 1;
    } else {
        return x * Factorial(x - 1);
    }
}

```



#### Remarks


Note that using recursion can have a severe impact on your code, as each recursive function call will be appended to the stack. If there are too many calls this could lead to a **StackOverflow**Exception. Most "natural recursive functions" can be written as a `for`, `while` or `foreach` loop construct, and whilst not looking so **posh** or **clever** will be more efficient.

Always think twice and use recursion carefully - know why you use it:

<li>recursion should be used when you know the number of recursive calls isn't **excessive**
<ul>
- **excessive** means, it depends on how much memory is available

- but be aware, it can be less efficient! For example in the Fibonacci recursion, to compute the **nth** number in the sequence, the calculation time will grow exponentially!

If you want more theory, please read:

- [https://www.cs.umd.edu/class/fall2002/cmsc214/Tutorial/recursion2.html](https://www.cs.umd.edu/class/fall2002/cmsc214/Tutorial/recursion2.html)
- [https://en.wikipedia.org/wiki/Recursion#In_computer_science](https://en.wikipedia.org/wiki/Recursion#In_computer_science)

