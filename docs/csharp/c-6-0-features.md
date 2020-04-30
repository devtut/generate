---
metaTitle: "C# 6.0 Features"
description: "Exception filters, String interpolation, Auto-property initializers, Null propagation, Expression-bodied function members, Operator nameof, Using static type, Index initializers, Improved overload resolution, Await in catch and finally, Minor changes and bugfixes, Using an extension method for collection initialization, Disable Warnings Enhancements"
---

# C# 6.0 Features


This sixth iteration of the C# language is provided by the Roslyn compiler.  This compiler came out with version 4.6 of the .NET Framework, however it can generate code in a backward compatible manner to allow targeting earlier framework versions.  C# version 6 code can be compiled in a fully backwards compatible manner to .NET 4.0.  It can also be used for earlier frameworks, however some features that require additional framework support may not function correctly.



## Exception filters


[Exception filters](https://github.com/dotnet/roslyn/wiki/New-Language-Features-in-C%23-6#exception-filters) give developers the ability to add a condition (in the form of a `boolean` expression) to a [catch](http://stackoverflow.com/documentation/c%23/26/keywords/148/try-catch-finally-throw) block, allowing the `catch` to execute only if the condition evaluates to `true`.

Exception filters allow the propagation of debug information in the original exception, where as using an `if` statement inside a `catch` block and re-throwing the exception stops the propagation of debug information in the original exception. With exception filters, the exception continues to propagate upwards in the call stack **unless** the condition is met. As a result, exception filters make the debugging experience much easier. Instead of stopping on the `throw` statement, the debugger will stop on the statement throwing the exception, with the current state and all local variables preserved. Crash dumps are affected in a similar way.

> 
Exception filters have been supported by the [**CLR**](https://msdn.microsoft.com/en-us/library/8bs2ecf4(v=vs.110).aspx) since the beginning and they've been accessible from VB.NET and F# for over a decade by exposing a part of the CLR's exception handling model. Only after the release of C# 6.0 has the functionality also been available for C# developers.


### Using exception filters

Exception filters are utilized by appending a `when` clause to the `catch` expression. It is possible to use any expression returning a `bool` in a `when` clause (except [await](http://stackoverflow.com/documentation/c%23/26/keywords/5993/async-await)). The declared Exception variable `ex` is accessible from within the `when` clause:

```cs
var SqlErrorToIgnore = 123;
try
{
    DoSQLOperations();
}
catch (SqlException ex) when (ex.Number != SqlErrorToIgnore)
{
    throw new Exception("An error occurred accessing the database", ex);
}

```

Multiple `catch` blocks with `when` clauses may be combined. The first `when` clause returning `true` will cause the exception to be caught. Its `catch` block will be entered, while the other `catch` clauses will be ignored (their `when` clauses won't be evaluated). For example:

```cs
try
{ ... }
catch (Exception ex) when (someCondition) //If someCondition evaluates to true,
                                          //the rest of the catches are ignored.
{ ... }
catch (NotImplementedException ex) when (someMethod()) //someMethod() will only run if
                                                       //someCondition evaluates to false
{ ... }
catch(Exception ex) // If both when clauses evaluate to false
{ ... }

```

### Risky when clause

> 
**Caution**
It can be risky to use exception filters: when an `Exception` is thrown from within the `when` clause, the `Exception` from the `when` clause is ignored and is treated as `false`. This approach allows developers to write `when` clause without taking care of invalid cases.


The following example illustrates such a scenario:

```cs
public static void Main()
{
    int a = 7;
    int b = 0;
    try
    {
        DoSomethingThatMightFail();
    }
    catch (Exception ex) when (a / b == 0)
    {
        // This block is never reached because a / b throws an ignored
        // DivideByZeroException which is treated as false.
    }
    catch (Exception ex)
    {
        // This block is reached since the DivideByZeroException in the 
        // previous when clause is ignored.
    }
}

public static void DoSomethingThatMightFail()
{
    // This will always throw an ArgumentNullException.
    Type.GetType(null);
}

```

[View Demo](https://dotnetfiddle.net/Iex6DP)

Note that exception filters avoid the confusing line number problems associated with using `throw` when failing code is within the same function. For example in this case the line number is reported as 6 instead of 3:

```cs
1. int a = 0, b = 0;
2. try {
3.     int c = a / b;
4. }
5. catch (DivideByZeroException) {
6.     throw;
7. }

```

The exception line number is reported as 6 because the error was caught and re-thrown with the `throw` statement on line 6.

The same does not happen with exception filters:

```cs
1. int a = 0, b = 0;
2. try {
3.     int c = a / b;
4. }
5. catch (DivideByZeroException) when (a != 0) {
6.     throw;
7. }

```

In this example `a` is 0 then `catch` clause is ignored but 3 is reported as line number. This is because they **do not unwind the stack**. More specifically, the exception **is not caught** on line 5 because `a` in fact does equal `0` and thus there is no opportunity for the exception to be re-thrown on line 6 because line 6 does not execute.

### Logging as a side effect

Method calls in the condition can cause side effects, so exception filters can be used to run code on exceptions without catching them. A common example that takes advantage of this is a `Log` method that always returns `false`. This allows tracing log information while debugging without the need to re-throw the exception.

> 
**Be aware that** while this seems to be a comfortable way of logging, it can be risky, especially if 3rd party logging assemblies are used. These might throw exceptions while logging in non-obvious situations that may not be detected easily (see **Risky `when(...)` clause** above).


```cs
try
{
    DoSomethingThatMightFail(s);
}
catch (Exception ex) **when** (Log(ex, "An error occurred"))
{
    // This catch block will never be reached
}

// ...

static bool Log(Exception ex, string message, params object[] args)
{
    Debug.Print(message, args);
    return false;
}
```

[View Demo](https://dotnetfiddle.net/pqPc7B)

The common approach in previous versions of C# was to log and re-throw the exception.

```cs
try
{
    DoSomethingThatMightFail(s);
}
catch (Exception ex)
{
     Log(ex, "An error occurred");
     throw;
}

// ...

static void Log(Exception ex, string message, params object[] args)
{
    Debug.Print(message, args);
}

```

[View Demo](https://dotnetfiddle.net/kEWLue)

### The `finally` block

The [`finally`](https://stackoverflow.com/documentation/c%23/40/exception-handling/172/finally-block) block executes every time whether the exception is thrown or not. One subtlety with expressions in `when` is exception filters are executed further up the stack **before** entering the inner `finally` blocks. This can cause unexpected results and behaviors when code attempts to modify global state (like the current thread's user or culture) and set it back in a `finally` block.

### Example: `finally` block

```cs
private static bool Flag = false;

static void Main(string[] args)
{
    Console.WriteLine("Start");
    try
    {
        SomeOperation();
    }
    catch (Exception) when (EvaluatesTo())
    {
        Console.WriteLine("Catch");
    }
    finally
    {
        Console.WriteLine("Outer Finally");
    }
}

private static bool EvaluatesTo()
{
    Console.WriteLine($"EvaluatesTo: {Flag}");
    return true;
}

private static void SomeOperation()
{
    try
    {
        Flag = true;
        throw new Exception("Boom");
    }
    finally
    {
        Flag = false;
        Console.WriteLine("Inner Finally");
    }
}

```

Produced Output:

> 
<p>Start<br />
EvaluatesTo: True<br />
Inner Finally<br />
Catch<br />
Outer Finally</p>


[View Demo](https://ideone.com/gxfBA8)

In the example above, if the method `SomeOperation` does not wish to "leak" the global state changes to caller's `when` clauses, it should also contain a `catch` block to modify the state. For example:

```cs
private static void SomeOperation()
{
    try
    {
        Flag = true;
        throw new Exception("Boom");
    }
    catch
    {
       Flag = false;
       throw;
    }
    finally
    {
        Flag = false;
        Console.WriteLine("Inner Finally");
    }
}

```

It is also common to see [`IDisposable`](https://stackoverflow.com/documentation/c%23/1795/idisposable-interface) helper classes leveraging the semantics of [using](http://stackoverflow.com/documentation/c%23/26/keywords/5062/using) blocks to achieve the same goal, as `IDisposable.Dispose` will always be called before an exception called within a `using` block starts bubbling up the stack.



## String interpolation


String interpolation allows the developer to combine `variables` and text to form a string.

### Basic Example

Two `int` variables are created: `foo` and `bar`.

```cs
int foo = 34;
int bar = 42;

string resultString = $"The foo is {foo}, and the bar is {bar}.";

Console.WriteLine(resultString);

```

**Output**:

> 
The foo is 34, and the bar is 42.


[View Demo](https://ideone.com/bRFOaV)

Braces within strings can still be used, like this:

```cs
var foo = 34;
var bar = 42;

// String interpolation notation (new style)
Console.WriteLine($"The foo is **{{foo}}**, and the bar is **{{bar}}**.");
```

This produces the following output:

> 
The foo is {foo}, and the bar is {bar}.


### Using interpolation with verbatim string literals

Using `@` before the string will cause the string to be interpreted verbatim. So, e.g. Unicode characters or line breaks will stay exactly as they've been typed. However, this will not effect the expressions in an interpolated string as shown in the following example:
```cs
Console.WriteLine($@"In case it wasn't clear:
\u00B9
The foo
is **{foo}**,
and the bar
is **{bar}**.");
```


Output:

> 
<p>In case it wasn't clear:<br />
\u00B9<br />
The foo<br />
is 34,<br />
and the bar<br />
is 42.</p>


[View Demo](https://dotnetfiddle.net/FLs4Ae)

### Expressions

With string interpolation, **expressions** within curly braces `{}` can also be evaluated. The result will be inserted at the corresponding location within the string. For example, to calculate the maximum of `foo` and `bar` and insert it, use `Math.Max` within the curly braces:<pre>`Console.WriteLine($"And the greater one is: **{ Math.Max(foo, bar) }**");`</pre>

Output:

> 
And the greater one is: 42


**Note: Any leading or trailing whitespace (including space, tab and CRLF/newline) between the curly brace and the expression is completely ignored and not included in the output**

[View Demo](https://ideone.com/qY1Y4B)

As another example, variables can be formatted as a currency:<pre>`Console.WriteLine($"Foo formatted as a currency to 4 decimal places: **{foo:c4}**");`</pre>

Output:

> 
Foo formatted as a currency to 4 decimal places: $34.0000


[View Demo](https://ideone.com/CPB8UJ)

Or they can be formatted as dates:<pre>`Console.WriteLine($"Today is: **{DateTime.Today:dddd, MMMM dd - yyyy}**");`</pre>

Output:

> 
Today is: Monday, July, 20 - 2015


[View Demo](https://ideone.com/PkjA6k)

Statements with a [Conditional (Ternary) Operator](https://msdn.microsoft.com/en-us/library/ty67wk28.aspx) can also be evaluated within the interpolation. However, these must be wrapped in parentheses, since the colon is otherwise used to indicate formatting as shown above:

```cs
Console.WriteLine($"{(foo > bar ? "Foo is larger than bar!" : "Bar is larger than foo!")}");
```

Output:

> 
Bar is larger than foo!


[View Demo](https://ideone.com/sX6tO3)

Conditional expressions and format specifiers can be mixed:

```cs
Console.WriteLine($"Environment: {(Environment.Is64BitProcess ? 64 : 32):00'-bit'} process");

```

Output:

> 
Environment: 32-bit process


### Escape sequences

Escaping backslash (`\`) and quote (`"`) characters works exactly the same in interpolated strings as in non-interpolated strings, for both verbatim and non-verbatim string literals:

```cs
Console.WriteLine($"Foo is: **{foo}**. In a non-verbatim string, we need to escape \" and \\ with backslashes.");
Console.WriteLine($@"Foo is: **{foo}**. In a verbatim string, we need to escape "" with an extra quote, but we don't need to escape \");

```

Output:

> 
<p>Foo is 34. In a non-verbatim string, we need to escape " and \ with backslashes.<br />
Foo is 34. In a verbatim string, we need to escape " with an extra quote, but we don't need to escape \</p>


To include a curly brace `{` or `}` in an interpolated string, use two curly braces `{{` or `}}`:<pre>`$"{{foo}} is: **{foo}**"`</pre>

Output:

> 
{foo} is: 34


[View Demo](https://dotnetfiddle.net/BuudHP)

### FormattableString type

The type of a `$"..."` string interpolation expression [is not always](http://stackoverflow.com/questions/38119074) a simple string. The compiler decides which type to assign depending on the context:
```cs
string s = $"hello, **{name}**";
System.FormattableString s = $"Hello, **{name}**";
System.IFormattable s = $"Hello, **{name}**";
```



This is also the order of type preference when the compiler needs to choose which overloaded method is going to be called.

A [new type](https://msdn.microsoft.com/en-us/library/system.formattablestring(v=vs.110).aspx), `System.FormattableString`, represents a composite format string, along with the arguments to be formatted. Use this to write applications that handle the interpolation arguments specifically:

```cs
public void AddLogItem(FormattableString formattableString)
{
    foreach (var arg in formattableString.GetArguments())
    {
        // do something to interpolation argument 'arg'
    }

    // use the standard interpolation and the current culture info
    // to get an ordinary String:
    var formatted = formattableString.ToString();

    // ...
}

```

Call the above method with:<pre>`AddLogItem($"The foo is **{foo}**, and the bar is **{bar}**.");`</pre>
For example, one could choose not to incur the performance cost of formatting the string if the logging level was already going to filter out the log item.<hr>

### Implicit conversions

There are implicit type conversions from an interpolated string:
```cs
var s = $"Foo: **{foo}**";
System.IFormattable s = $"Foo: **{foo}**";
```


You can also produce an `IFormattable` variable that allows you to convert the string with invariant context:
```cs
var s = $"Bar: **{bar}**";
System.FormattableString s = $"Bar: **{bar}**";
```

<hr>

```cs
var s = $"Bar: **{bar}**";
System.FormattableString s = $"Bar: **{bar}**";
```

### Current and Invariant Culture Methods

If code analysis is turned on, interpolated strings will all produce warning [CA1305](https://msdn.microsoft.com/en-us/library/ms182190.aspx) (Specify `IFormatProvider`).
A static method may be used to apply current culture.

```cs
public static class Culture
{
    public static string Current(FormattableString formattableString)
    {
        return formattableString?.ToString(CultureInfo.CurrentCulture);
    }
    public static string Invariant(FormattableString formattableString)
    {
        return formattableString?.ToString(CultureInfo.InvariantCulture);
    }
}

```

Then, to produce a correct string for the current culture, just use the expression:
```cs
Culture.Current($"interpolated **{typeof(string).Name}** string.")
Culture.Invariant($"interpolated **{typeof(string).Name}** string.")
```


**Note**: `Current` and `Invariant` cannot be created as extension methods because, by default, the compiler assigns type `String` to **interpolated string expression** which causes the following code to fail to compile:

```cs
$"interpolated {typeof(string).Name} string.".Current();

```

`FormattableString` class already contains `Invariant()` method, so the simplest way of switching to invariant culture is by relying on `using static`:
```cs
using static System.FormattableString;

```cs
using static System.FormattableString;</p>
<p>string invariant = Invariant($"Now = **{DateTime.Now}**");
string current = $"Now = **{DateTime.Now}**";
```

string invariant = Invariant($"Now = **{DateTime.Now}**");
string current = $"Now = **{DateTime.Now}**";
```

<hr>

### Behind the scenes

Interpolated strings are just a syntactic sugar for `String.Format()`. The compiler ([Roslyn](https://github.com/dotnet/roslyn)) will turn it into a `String.Format` behind the scenes:

```cs
var text = $"Hello {name + lastName}";

```

The above will be converted to something like this:

```cs
string text = string.Format("Hello {0}", new object[] {
    name + lastName
});

```

### String Interpolation and Linq

It's possible to use interpolated strings in Linq statements to increase readability further.

```cs
var fooBar = (from DataRow x in fooBarTable.Rows
          select string.Format("{0}{1}", x["foo"], x["bar"])).ToList();

```

Can be re-written as:

```cs
var fooBar = (from DataRow x in fooBarTable.Rows
          select $"{x["foo"]}{x["bar"]}").ToList();

```

### Reusable Interpolated Strings

With `string.Format`, you can create reusable format strings:

```cs
public const string ErrorFormat = "Exception caught:\r\n{0}";

// ...

Logger.Log(string.Format(ErrorFormat, ex));

```

Interpolated strings, however, will not compile with placeholders referring to non-existent variables. The following will not compile:

```cs
public const string ErrorFormat = $"Exception caught:\r\n{error}";
// CS0103: The name 'error' does not exist in the current context

```

Instead, create a `Func<>` which consumes variables and returns a `String`:

```cs
public static Func<Exception, string> FormatError =
    error => $"Exception caught:\r\n{error}";

// ...

Logger.Log(FormatError(ex));

```

### String interpolation and localization

If you’re localizing your application you may wonder if it is possible to use string interpolation along with localization. Indeed, it would be nice to have the possibility to store in resource files `String`s like:<pre>`"My name is **{name} {middlename} {surname}**"`</pre>
instead of the much less readable:

```cs
"My name is {0} {1} {2}"

```

`String` interpolation process occurs **at compile time**, unlike formatting string with `string.Format` which occurs **at runtime**. Expressions in an interpolated string must reference names in the current context and need to be stored in resource files. That means that if you want to use localization you have to do it like:

```cs
var FirstName = "John";

// method using different resource file "strings"
// for French ("strings.fr.resx"), German ("strings.de.resx"), 
// and English ("strings.en.resx")
void ShowMyNameLocalized(string name, string middlename = "", string surname = "")
{
    // get localized string
    var localizedMyNameIs = Properties.strings.Hello;
    // insert spaces where necessary
    name = (string.IsNullOrWhiteSpace(name) ? "" : name + " ");
    middlename = (string.IsNullOrWhiteSpace(middlename) ? "" : middlename + " ");
    surname = (string.IsNullOrWhiteSpace(surname) ? "" : surname + " ");
    // display it
    Console.WriteLine($"{localizedMyNameIs} {name}{middlename}{surname}".Trim());
}

// switch to French and greet John
Thread.CurrentThread.CurrentUICulture = CultureInfo.GetCultureInfo("fr-FR");
ShowMyNameLocalized(FirstName);

// switch to German and greet John
Thread.CurrentThread.CurrentUICulture = CultureInfo.GetCultureInfo("de-DE");
ShowMyNameLocalized(FirstName);

// switch to US English and greet John
Thread.CurrentThread.CurrentUICulture = CultureInfo.GetCultureInfo("en-US");
ShowMyNameLocalized(FirstName);

```

If the resource strings for the languages used above are correctly stored in the individual resource files, you should get the following output:

> 
<p>Bonjour, mon nom est John<br/>
Hallo, mein Name ist John<br/>
Hello, my name is John<br/></p>


**Note** that this implies that the name follows the localized string in every language. If that is not the case, you need to add placeholders to the resource strings and modify the function above or you need to query the culture info in the function and provide a switch case statement containing the different cases.
For more details about resource files, see [How to use localization in C#](https://stackoverflow.com/a/1142840/1016343).

It is a good practice to use a default fallback language most people will understand, in case a translation is not available. I suggest to use English as default fallback language.

### Recursive interpolation

Although not very useful, it is allowed to use an interpolated `string` recursively inside another's curly brackets:

```cs
Console.WriteLine($"String has {$"My class is called {nameof(MyClass)}.".Length} chars:");
Console.WriteLine($"My class is called {nameof(MyClass)}.");

```

Output:

> 
String has 27 chars:


> 
My class is called MyClass.




## Auto-property initializers


### Introduction

Properties can be initialized with the `=` operator after the closing `}`. The `Coordinate` class below shows the available options for initializing a property:

```cs
public class Coordinate
{ 
    public int X { get; set; } = 34; // get or set auto-property with initializer

    public int Y { get; } = 89;      // read-only auto-property with initializer              
}

```

### Accessors With Different Visibility

You can initialize auto-properties that have different visibility on their accessors. Here’s an example with a protected setter:

```

   public string Name { get; protected set; } = "Cheeze";

```

The accessor can also be `internal`, `internal protected`, or `private`.

### Read-Only Properties

In addition to flexibility with visibility, you can also initialize read-only auto-properties. Here’s an example:

```

   public List<string> Ingredients { get; } = 
        new List<string> { "dough", "sauce", "cheese" };

```

This example also shows how to initialize a property with a complex type. Also, auto-properties can’t be write-only, so that also precludes write-only initialization.

### Old style (pre C# 6.0)

Before C# 6, this required much more verbose code. We were using one extra variable called backing property for the property to give default value or to initialize the public property like below,

```cs
public class Coordinate
{
    private int _x = 34;
    public int X { get { return _x; } set { _x = value; } }

    private readonly int _y = 89;
    public int Y { get { return _y; } }
    
    private readonly int _z;
    public int Z { get { return _z; } }

    public Coordinate()
    {
        _z = 42;
    }
}

```

****Note:** Before C# 6.0, you could still initialize read and write [**auto implemented properties**](http://stackoverflow.com/documentation/c%23/49/properties/3365/auto-implemented-properties#t=201608062134378589394) (properties with a getter and a setter) from within the constructor, but you could not initialize the property inline with its declaration**

[View Demo](http://ideone.com/2OgrPQ)

### Usage

Initializers must evaluate to static expressions, just like field initializers. If you need to reference non-static members, you can either initialize properties in constructors like before, or use expression-bodied properties. Non-static expressions, like the one below (commented out), will generate a compiler error:

```cs
// public decimal X { get; set; } = InitMe();  // generates compiler error

decimal InitMe() { return 4m; }

```

But static methods **can** be used to initialize auto-properties:

```cs
public class Rectangle
{
    public double Length { get; set; } = 1;
    public double Width { get; set; } = 1;
    public double Area { get; set; } = CalculateArea(1, 1);

    public static double CalculateArea(double length, double width)
    {
        return length * width;
    }
}

```

This method can also be applied to properties with different level of accessors:

```cs
public short Type { get; private set; } = 15;

```

The auto-property initializer allows assignment of properties directly within their declaration. For read-only properties, it takes care of all the requirements required to ensure the property is immutable. Consider, for example, the `FingerPrint` class in the following example:

```cs
public class FingerPrint
{
  public DateTime TimeStamp { get; } = DateTime.UtcNow;

  public string User { get; } =
    System.Security.Principal.WindowsPrincipal.Current.Identity.Name;

  public string Process { get; } =
    System.Diagnostics.Process.GetCurrentProcess().ProcessName;
}

```

[View Demo](http://ideone.com/qjDRmx)

### Cautionary notes

Take care to not confuse auto-property or field initializers with similar-looking [expression-body methods](http://stackoverflow.com/documentation/c%23/24/c-sharp-6-0-features/44/expression-bodied-function-members) which make use of `=>` as opposed to `=`, and fields which do not include `{ get; }`.

For example, each of the following declarations are different.

```cs
public class UserGroupDto
{
    // Read-only auto-property with initializer:       
    public ICollection<UserDto> Users1 { get; } = new HashSet<UserDto>();
    
    // Read-write field with initializer:
    public ICollection<UserDto> Users2 = new HashSet<UserDto>();

    // Read-only auto-property with expression body:
    public ICollection<UserDto> Users3 => new HashSet<UserDto>();
}

```

Missing `{ get; }` in the property declaration results in a public field. Both read-only auto-property `Users1` and read-write field `Users2` are initialized only once, but a public field allows changing collection instance from outside the class, which is usually undesirable. Changing a read-only auto-property with expression body to read-only property with initializer requires not only removing `>` from `=>`, but adding `{ get; }`.

The different symbol (`=>` instead of `=`) in `Users3` results in each access to the property returning a new instance of the `HashSet<UserDto>` which, while valid C# (from the compiler's point of view) is unlikely to be the desired behavior when used for a collection member.

The above code is equivalent to:

```cs
public class UserGroupDto
{
    // This is a property returning the same instance
    // which was created when the UserGroupDto was instantiated.
    private ICollection<UserDto> _users1 = new HashSet<UserDto>();
    public ICollection<UserDto> Users1 { get { return _users1; } }

    // This is a field returning the same instance
    // which was created when the UserGroupDto was instantiated.
    public virtual ICollection<UserDto> Users2 = new HashSet<UserDto>();

    // This is a property which returns a new HashSet<UserDto> as
    // an ICollection<UserDto> on each call to it.
    public ICollection<UserDto> Users3 { get { return new HashSet<UserDto>(); } }
}

```



## Null propagation


The `?.` operator and `?[...]` operator are called the [null-conditional operator](https://msdn.microsoft.com/en-us/library/dn986595.aspx). It is also sometimes referred to by other names such as the [safe navigation operator](https://en.wikipedia.org/wiki/Safe_navigation_operator).

This is useful, because if the `.` (member accessor) operator is applied to an expression that evaluates to `null`, the program will throw a `NullReferenceException`.  If the developer instead uses the `?.` (null-conditional) operator, the expression will evaluate to null instead of throwing an exception.

Note that if the `?.` operator is used and the expression is non-null, `?.` and `.` are equivalent.

### Basics

```cs
var teacherName = classroom.GetTeacher().Name;
// throws NullReferenceException if GetTeacher() returns null

```

[View Demo](http://ideone.com/p8OGBB)

If the `classroom` does not have a teacher, `GetTeacher()` may return `null`. When it is `null` and the `Name` property is accessed, a `NullReferenceException` will be thrown.

If we modify this statement to use the `?.` syntax, the result of the entire expression will be `null`:

```cs
var teacherName = classroom.GetTeacher()?.Name;
// teacherName is null if GetTeacher() returns null

```

[View Demo](http://ideone.com/3aqGlE)

Subsequently, if `classroom` could also be `null`, we could also write this statement as:

```cs
var teacherName = classroom?.GetTeacher()?.Name;
// teacherName is null if GetTeacher() returns null OR classroom is null

```

[View Demo](http://ideone.com/voljZh)

This is an example of short-circuiting: When any conditional access operation using the null-conditional operator evaluates to null, the entire expression evaluates to null immediately, without processing the rest of the chain.

When the terminal member of an expression containing the null-conditional operator is of a value type, the expression evaluates to a `Nullable<T>` of that type and so cannot be used as a direct replacement for the expression without `?.`.

```cs
bool hasCertification = classroom.GetTeacher().HasCertification;
// compiles without error but may throw a NullReferenceException at runtime

bool hasCertification = classroom?.GetTeacher()?.HasCertification;
// compile time error: implicit conversion from bool? to bool not allowed

bool? hasCertification = classroom?.GetTeacher()?.HasCertification;
// works just fine, hasCertification will be null if any part of the chain is null

bool hasCertification = classroom?.GetTeacher()?.HasCertification.GetValueOrDefault();
// must extract value from nullable to assign to a value type variable

```

### Use with the Null-Coalescing Operator (??)

You can combine the null-conditional operator with the [Null-coalescing Operator](https://msdn.microsoft.com/en-us/library/ms173224.aspx) (`??`) to return a default value if the expression resolves to `null`. Using our example above:

```cs
var teacherName = classroom?.GetTeacher()?.Name ?? "No Name";
// teacherName will be "No Name" when GetTeacher() 
// returns null OR classroom is null OR Name is null

```

### Use with Indexers

The null-conditional operator can be used with [indexers](https://msdn.microsoft.com/en-us/library/6x16t2tx.aspx):

```cs
var firstStudentName = classroom?.Students?[0]?.Name;

```

In the above example:

- The first `?.` ensures that `classroom` is not `null`.
- The second `?` ensures that the entire `Students` collection is not `null`.
- The third `?.` after the indexer ensures that the `[0]` indexer did not return a `null` object. It should be noted that this operation can **still** throw an `IndexOutOfRangeException`.

### Use with void Functions

Null-conditional operator can also be used with `void` functions. However in this case, the statement will not evaluate to `null`. It will just prevent a `NullReferenceException`.

```cs
List<string> list = null;
list?.Add("hi");          // Does not evaluate to null

```

### Use with Event Invocation

Assuming the following event definition:

```cs
private event EventArgs OnCompleted;

```

When invoking an event, traditionally, it is best practice to check if the event is `null` in case no subscribers are present:

```cs
var handler = OnCompleted;
if (handler != null)
{
    handler(EventArgs.Empty);
}

```

Since the null-conditional operator has been introduced, the invocation can be reduced to a single line:

```cs
OnCompleted?.Invoke(EventArgs.Empty);

```

### Limitations

Null-conditional operator produces rvalue, not lvalue, that is, it cannot be used for property assignment, event subscription etc. For example, the following code will not work:

```cs
// Error: The left-hand side of an assignment must be a variable, property or indexer
Process.GetProcessById(1337)?.EnableRaisingEvents = true;
// Error: The event can only appear on the left hand side of += or -=
Process.GetProcessById(1337)?.Exited += OnProcessExited;

```

### Gotchas

Note that:

```cs
int? nameLength = person?.Name.Length;    // safe if 'person' is null

```

is **not** the same as:

```cs
int? nameLength = (person?.Name).Length;  // avoid this

```

because the former corresponds to:

```cs
int? nameLength = person != null ? (int?)person.Name.Length : null;

```

and the latter corresponds to:

```cs
int? nameLength = (person != null ? person.Name : null).Length;

```

Despite ternary operator `?:` is used here for explaining the difference between two cases, these operators are not equivalent. This can be easily demonstrated with the following example:

```cs
void Main()
{
    var foo = new Foo();
    Console.WriteLine("Null propagation");
    Console.WriteLine(foo.Bar?.Length);

    Console.WriteLine("Ternary");
    Console.WriteLine(foo.Bar != null ? foo.Bar.Length : (int?)null);
}

class Foo
{
    public string Bar
    {
        get
        {
            Console.WriteLine("I was read");
            return string.Empty;
        }
    }
}

```

Which outputs:

> 
<p>Null propagation<br />
I was read<br />
0<br />
Ternary<br />
I was read<br />
I was read<br />
0</p>


[View Demo](https://dotnetfiddle.net/BytXEz)

To avoid multiple invocations equivalent would be:

```cs
var interimResult = foo.Bar;
Console.WriteLine(interimResult != null ? interimResult.Length : (int?)null);

```

And this difference somewhat explains why null propagation operator is [not yet supported](https://roslyn.codeplex.com/discussions/571077) in expression trees.



## Expression-bodied function members


Expression-bodied function members allow the use of lambda expressions as member bodies. For simple members, it can result in cleaner and more readable code.

Expression-bodied functions can be used for properties, indexers, methods, and operators.

### Properties

```cs
public decimal TotalPrice => BasePrice + Taxes;

```

Is equivalent to:

```cs
public decimal TotalPrice
{
    get
    {
        return BasePrice + Taxes;
    }
}

```

When an expression-bodied function is used with a property, the property is implemented as a getter-only property.

[View Demo](https://dotnetfiddle.net/djFd7O)

### Indexers

```cs
public object this[string key] => dictionary[key];

```

Is equivalent to:

```cs
public object this[string key]
{
    get
    {
        return dictionary[key];
    }
}

```

### Methods

```cs
static int Multiply(int a, int b) => a * b;

```

Is equivalent to:

```cs
static int Multiply(int a, int b)
{
    return a * b;
}

```

Which can also be used with `void` methods:

```cs
public void Dispose() => resource?.Dispose();

```

An override of `ToString` could be added to the `Pair<T>` class:

```cs
public override string ToString() => $"{First}, {Second}";

```

Additionally, this simplistic approach works with the `override` keyword:

```cs
public class Foo
{
    public int Bar { get; }

    public string override ToString() => $"Bar: {Bar}";
}

```

### Operators

This also can be used by operators:

```cs
public class Land
{
    public double Area { get; set; }

    public static Land operator +(Land first, Land second) =>
        new Land { Area = first.Area + second.Area };
}

```

### Limitations

Expression-bodied function members have some limitations. They can't contain block statements and any other statements that contain blocks: `if`, `switch`, `for`, `foreach`, `while`, `do`, `try`, etc.

Some `if` statements can be replaced with ternary operators. Some `for` and `foreach` statements can be converted to LINQ queries, for example:

```cs
IEnumerable<string> Digits
{
    get
    {
        for (int i = 0; i < 10; i++)
            yield return i.ToString();
    }
}

```

```cs
IEnumerable<string> Digits => Enumerable.Range(0, 10).Select(i => i.ToString());

```

In all other cases, the old syntax for function members can be used.

Expression-bodied function members can contain `async`/`await`, but it's often redundant:

```cs
async Task<int> Foo() => await Bar();  

```

Can be replaced with:

```cs
Task<int> Foo() => Bar();

```



## Operator nameof


The `nameof` operator returns the name of a code element as a `string`. This is useful when throwing exceptions related to method arguments and also when implementing `INotifyPropertyChanged`.

```cs
public string SayHello(string greeted)
{
    if (greeted == null)
        throw new ArgumentNullException(nameof(greeted));
    
    Console.WriteLine("Hello, " + greeted);
}

```

The `nameof` operator is evaluated at compile time and changes the expression into a string literal. This is also useful for strings that are named after their member that exposes them. Consider the following:

```cs
public static class Strings
{
    public const string Foo = nameof(Foo); // Rather than Foo = "Foo"
    public const string Bar = nameof(Bar); // Rather than Bar = "Bar"
}

```

Since `nameof` expressions are compile-time constants, they can be used in attributes, `case` labels, `switch` statements, and so on.

---


It is convenient to use `nameof` with `Enum`s. Instead of:

```cs
Console.WriteLine(Enum.One.ToString());

```

it is possible to use:

```cs
Console.WriteLine(nameof(Enum.One))

```

The output will be `One` in both cases.

---


The `nameof` operator can access non-static members using static-like syntax. Instead of doing:

```cs
string foo = "Foo";
string lengthName = nameof(foo.Length);

```

Can be replaced with:

```cs
string lengthName = nameof(string.Length);

```

The output will be `Length` in both examples. However, the latter prevents the creation of unnecessary instances.

---


Although the `nameof` operator works with most language constructs, there are some limitations. For example, you cannot use the `nameof` operator on open generic types or method return values:

```cs
public static int Main()
{   
    Console.WriteLine(nameof(List<>)); // Compile-time error
    Console.WriteLine(nameof(Main())); // Compile-time error
}

```

Furthermore, if you apply it to a generic type, the generic type parameter will be ignored:

```cs
Console.WriteLine(nameof(List<int>));  // "List"
Console.WriteLine(nameof(List<bool>)); // "List"

```

For more examples, see [this topic](https://stackoverflow.com/documentation/c%23/80/nameof-operator#t=201608031424500177545) dedicated to `nameof`.

---


### Workaround for previous versions ([more detail](http://stackoverflow.com/documentation/c%23/80/nameof-operator/26157/name-of-extension-support-added-for-before-c-sharp-6-version#t=201612071107472552734))

Although the `nameof` operator does not exist in C# for versions prior to 6.0, similar functionality can be had by using `MemberExpression` as in the following:

Expression:

```cs
public static string NameOf<T>(Expression<Func<T>> propExp)
{
    var memberExpression = propExp.Body as MemberExpression;
    return memberExpression != null ? memberExpression.Member.Name : null;
}

public static string NameOf<TObj, T>(Expression<Func<TObj, T>> propExp)
{
    var memberExpression = propExp.Body as MemberExpression;
    return memberExpression != null ? memberExpression.Member.Name : null;
}

```

Usage:

```cs
string variableName = NameOf(() => variable);
string propertyName = NameOf((Foo o) => o.Bar);

```

Note that this approach causes an expression tree to be created on every call, so the performance is much worse compared to `nameof` operator which is evaluated at compile time and has zero overhead at runtime.



## Using static type


The `using static [Namespace.Type]` directive allows the importing of static members of types and enumeration values. Extension methods are imported as extension methods (from just one type), not into top-level scope.

```cs
using static System.Console;
using static System.ConsoleColor;
using static System.Math;

class Program
{
    static void Main()
    {
        BackgroundColor = DarkBlue;
        WriteLine(Sqrt(2));
    }
}

```

[Live Demo Fiddle](https://dotnetfiddle.net/7Ll3XN)

```cs
using System;

class Program
{
    static void Main()
    {
        Console.BackgroundColor = ConsoleColor.DarkBlue;
        Console.WriteLine(Math.Sqrt(2));
    }
}

```



## Index initializers


Index initializers make it possible to create and initialize objects with indexes at the same time.

This makes initializing Dictionaries very easy:

```cs
var dict = new Dictionary<string, int>()
{
    ["foo"] = 34,
    ["bar"] = 42
};

```

Any object that has an indexed getter or setter can be used with this syntax:

```cs
class Program
{
    public class MyClassWithIndexer
    {
        public int this[string index]
        {
            set
            {
                Console.WriteLine($"Index: {index}, value: {value}");
            }
        }
    }

    public static void Main()
    {
        var x = new MyClassWithIndexer()
        {
            ["foo"] = 34,
            ["bar"] = 42
        };

        Console.ReadKey();
    }
}

```

Output:

> 
<p>Index: foo, value: 34<br />
Index: bar, value: 42</p>


[View Demo](https://dotnetfiddle.net/Evs4Qx)

If the class has multiple indexers it is possible to assign them all in a single group of statements:

```cs
class Program
{
    public class MyClassWithIndexer
    {
        public int this[string index]
        {
            set
            {
                Console.WriteLine($"Index: {index}, value: {value}");
            }
        }
        public string this[int index]
        {
            set
            {
                Console.WriteLine($"Index: {index}, value: {value}");
            }
        }
    }

    public static void Main()
    {
        var x = new MyClassWithIndexer()
        {
            ["foo"] = 34,
            ["bar"] = 42,
            [10] = "Ten",
            [42] = "Meaning of life"
        };
    }
}

```

Output:

> 
<p>Index: foo, value: 34<br />
Index: bar, value: 42<br />
Index: 10, value: Ten<br />
Index: 42, value: Meaning of life</p>


It should be noted that the indexer `set` accessor might behave differently compared to an `Add` method (used in collection initializers).

For example:

```cs
var d = new Dictionary<string, int>
{
    ["foo"] = 34,
    ["foo"] = 42,
}; // does not throw, second value overwrites the first one

```

versus:

```cs
var d = new Dictionary<string, int>
{
    { "foo", 34 },
    { "foo", 42 },
}; // run-time ArgumentException: An item with the same key has already been added.

```



## Improved overload resolution


Following snippet shows an example of passing a method group (as opposed to a lambda) when a delegate is expected. Overload resolution will now resolve this instead of raising an ambiguous overload error due to the ability of **C# 6** to check the return type of the method that was passed.

```cs
using System;
public class Program
{
    public static void Main()
    {
        Overloaded(DoSomething);
    }

    static void Overloaded(Action action)
    {
       Console.WriteLine("overload with action called");
    }

    static void Overloaded(Func<int> function)
    {
       Console.WriteLine("overload with Func<int> called");
    }

    static int DoSomething()
    {
        Console.WriteLine(0);
        return 0;
    }
}

```

Results:

**Output**

> 
overload with Func<int> called


[View Demo](https://dotnetfiddle.net/Vnudqy)

**Error**

> 
<p>error CS0121: The call is ambiguous between the following methods or properties:
'Program.Overloaded(System.Action)' and 'Program.Overloaded(System.Func)'</p>


**C# 6** can also handle well the following case of exact matching for lambda expressions which would have resulted in an error in **C# 5**.

```cs
using System;

class Program
{
    static void Foo(Func<Func<long>> func) {}
    static void Foo(Func<Func<int>> func) {}

    static void Main()
    {
        Foo(() => () => 7);
    }
}

```



## Await in catch and finally


It is possible to use `await` expression to apply [await operator](https://msdn.microsoft.com/en-us/library/hh156528.aspx) to [Tasks](https://msdn.microsoft.com/en-us/library/system.threading.tasks.task.aspx) or [Task(Of TResult)](https://msdn.microsoft.com/en-us/library/dd321424.aspx) in the `catch` and `finally` blocks in C#6.

It was not possible to use the `await` expression in the `catch` and `finally` blocks in earlier versions due to compiler limitations. C#6 makes awaiting async tasks a lot easier by allowing the `await` expression.

```cs
try
{
    //since C#5
    await service.InitializeAsync();
} 
catch (Exception e)
{
    //since C#6
    await logger.LogAsync(e);
}
finally
{
    //since C#6
    await service.CloseAsync();
}

```

It was required in C# 5 to use a `bool` or declare an `Exception` outside the try catch to perform async operations. This method is shown in the following example:

```cs
bool error = false;
Exception ex = null;

try
{
    // Since C#5
    await service.InitializeAsync();
} 
catch (Exception e)
{
    // Declare bool or place exception inside variable
    error = true;
    ex = e;
}

// If you don't use the exception
if (error)
{
    // Handle async task
}

// If want to use information from the exception
if (ex != null)
{
    await logger.LogAsync(e);
}    

// Close the service, since this isn't possible in the finally
await service.CloseAsync();

```



## Minor changes and bugfixes


Parentheses are now forbidden around named parameters. The following compiles in C#5, but not C#6

```cs
Console.WriteLine((value: 23));

```

Operands of `is` and `as` are no longer allowed to be method groups. The following compiles in C#5, but not C#6

```cs
var result = "".Any is byte;

```

> 
The native compiler allowed this (although it did show a warning), and in fact didn’t even check extension method compatibility, allowing crazy things like `1.Any is string` or `IDisposable.Dispose is object`.


See [this reference](http://blog.slaks.net/2014-05-28/exploring-roslyn-part-3-breaking-changes/) for updates on changes.



## Using an extension method for collection initialization


Collection initialization syntax can be used when instantiating any class which implements `IEnumerable` and has a method named `Add` which takes a single parameter.

In previous versions, this `Add` method had to be an **instance** method on the class being initialized. In C#6, it can also be an extension method.

```cs
public class CollectionWithAdd : IEnumerable
{
    public void Add<T>(T item)
    {
        Console.WriteLine("Item added with instance add method: " + item);
    }

    public IEnumerator GetEnumerator()
    {
        // Some implementation here
    }
}

public class CollectionWithoutAdd : IEnumerable
{
    public IEnumerator GetEnumerator()
    {
        // Some implementation here
    }
}

public static class Extensions
{
    public static void Add<T>(this CollectionWithoutAdd collection, T item)
    {
        Console.WriteLine("Item added with extension add method: " + item);
    }
}

public class Program
{
    public static void Main()
    {
        var collection1 = new CollectionWithAdd{1,2,3}; // Valid in all C# versions
        var collection2 = new CollectionWithoutAdd{4,5,6}; // Valid only since C# 6
    }
}

```

This will output:

> 
<p>Item added with instance add method: 1<br />
Item added with instance add method: 2<br />
Item added with instance add method: 3<br />
Item added with extension add method: 4<br />
Item added with extension add method: 5<br />
Item added with extension add method: 6</p>




## Disable Warnings Enhancements


In C# 5.0 and earlier the developer could only suppress warnings by number. With the introduction of Roslyn Analyzers, C# needs a way to disable warnings issued from specific libraries. With C# 6.0 the pragma directive can suppress warnings by name.

Before:

```cs
#pragma warning disable 0501

```

C# 6.0:

```cs
#pragma warning disable CS0501

```



#### Remarks


The sixth version of C# was released July 2015 alongside Visual Studio 2015 and .NET 4.6.

As well as adding some new language features it includes a complete rewrite of the compiler. Previously `csc.exe` was a native Win32 application written in C++, with C# 6 it is now a .NET managed application written in C#. This rewrite was known as project "Roslyn" and the code is now open source and available on [GitHub](https://github.com/dotnet/roslyn).

