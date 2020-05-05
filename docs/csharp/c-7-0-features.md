---
metaTitle: "C# 7.0 Features"
description: "Language support for Tuples, Local functions, out var declaration, Pattern Matching, Digit separators, Binary literals, throw expressions, Extended expression bodied members list, ref return and ref local, ValueTask<T>"
---

# C# 7.0 Features


C# 7.0 is the seventh version of C#. This version contains some new features: language support for Tuples, local functions, `out var` declarations, digit separators, binary literals, pattern matching, throw expressions, `ref return` and `ref local` and extended expression bodied members list.

Official reference: [What's new in C# 7](https://docs.microsoft.com/en-us/dotnet/articles/csharp/csharp-7)



## Language support for Tuples


### Basics

A **tuple** is an ordered, finite list of elements.  Tuples are commonly used in programming as a means to work with one single entity collectively instead of individually working with each of the tuple's elements, and to represent individual rows (ie. "records") in a relational database.

In C# 7.0, methods can have multiple return values. Behind the scenes, the compiler will use the new [ValueTuple](https://github.com/dotnet/corefx/blob/master/src/System.ValueTuple/src/System/ValueTuple/ValueTuple.cs) struct.

```cs
public (int sum, int count) GetTallies() 
{
    return (1, 2);
}

```

**Side note**: for this to work in Visual Studio 2017, you need to get the `System.ValueTuple` package.

If a tuple-returning method result is assigned to a single variable you can access the members by their defined names on the method signature:

```cs
var result = GetTallies();
// > result.sum
// 1
// > result.count
// 2

```

### Tuple Deconstruction

Tuple deconstruction separates a tuple into its parts.

For example, invoking `GetTallies` and assigning the return value to two separate variables deconstructs the tuple into those two variables:

```cs
(int tallyOne, int tallyTwo) = GetTallies();

```

`var` also works:

```cs
(var s, var c) = GetTallies();

```

You can also use shorter syntax, with `var` outside of `()`:

```cs
var (s, c) = GetTallies();

```

You can also deconstruct into existing variables:

```cs
int s, c;
(s, c) = GetTallies();

```

Swapping is now much simpler (no temp variable needed):

```cs
(b, a) = (a, b);

```

Interestingly, any object can be deconstructed by defining a `Deconstruct` method in the class:

```cs
class Person
{
    public string FirstName { get; set; }
    public string LastName { get; set; }

    public void Deconstruct(out string firstName, out string lastName)
    {
        firstName = FirstName;
        lastName = LastName;
    }
}

var person = new Person { FirstName = "John", LastName = "Smith" };
var (localFirstName, localLastName) = person;

```

In this case, the `(localFirstName, localLastName) = person` syntax is invoking `Deconstruct` on the `person`.

Deconstruction can even be defined in an extension method. This is equivalent to the above:

```cs
public static class PersonExtensions
{
    public static void Deconstruct(this Person person, out string firstName, out string lastName)
    {
        firstName = person.FirstName;
        lastName = person.LastName;
    }
}

var (localFirstName, localLastName) = person;

```

An alternative approach for the `Person` class is to define the `Name` itself as a `Tuple`. Consider the following:

```cs
class Person
{
    public (string First, string Last) Name { get; }

    public Person((string FirstName, string LastName) name)
    {
        Name = name;
    }
}

```

Then you can instantiate a person like so (where we can take a tuple as an argument):

```cs
var person = new Person(("Jane", "Smith"));

var firstName = person.Name.First; // "Jane"
var lastName = person.Name.Last;   // "Smith"

```

### Tuple Initialization

You can also arbitrarily create tuples in code:

```cs
var name = ("John", "Smith");
Console.WriteLine(name.Item1);
// Outputs John

Console.WriteLine(name.Item2);
// Outputs Smith

```

### 

When creating a tuple, you can assign ad-hoc item names to the members of the tuple:

```cs
var name = (first: "John", middle: "Q", last: "Smith");
Console.WriteLine(name.first);
// Outputs John

```

### Type inference

Multiple tuples defined with the same signature (matching types and count) will be inferred as matching types. For example:

```cs
public (int sum, double average) Measure(List<int> items)
{
    var stats = (sum: 0, average: 0d);
    stats.sum = items.Sum();
    stats.average = items.Average();
    return stats;
}

```

`stats` can be returned since the declaration of the `stats` variable and the method's return signature are a match.

### Reflection and Tuple Field Names

Member names do not exist at runtime. Reflection will consider tuples with the same number and types of members the same even if member names do not match. Converting a tuple to an `object` and then to a tuple with the same member types, but different names, will not cause an exception either.

While the ValueTuple class itself does not preserve information for member names the information is available through reflection in a TupleElementNamesAttribute. This attribute is not applied to the tuple itself but to method parameters, return values, properties and fields. This allows tuple item names to be preserved across assemblies i.e. if a method returns (string name, int count) the names name and count will be available to callers of the method in another assembly because the return value will be marked with TupleElementNameAttribute containing the values "name" and "count".

### Use with generics and `async`

The new tuple features (using the underlying `ValueTuple` type) fully support generics and can be used as generic type parameter. That makes it possible to use them with the `async`/`await` pattern:

```cs
public async Task<(string value, int count)> GetValueAsync()
{
    string fooBar = await _stackoverflow.GetStringAsync();
    int num = await _stackoverflow.GetIntAsync();

    return (fooBar, num);
}

```

### Use with collections

It may become beneficial to have a collection of tuples in (as an example) a scenario where you're attempting to find a matching tuple based on conditions to avoid code branching.

Example:

```cs
private readonly List<Tuple<string, string, string>> labels = new List<Tuple<string, string, string>>()
{
    new Tuple<string, string, string>("test1", "test2", "Value"),
    new Tuple<string, string, string>("test1", "test1", "Value2"),
    new Tuple<string, string, string>("test2", "test2", "Value3"),
};

public string FindMatchingValue(string firstElement, string secondElement)
{
    var result = labels
        .Where(w => w.Item1 == firstElement && w.Item2 == secondElement)
        .FirstOrDefault();

    if (result == null)
        throw new ArgumentException("combo not found");

    return result.Item3;
}

```

With the new tuples can become:

```cs
private readonly List<(string firstThingy, string secondThingyLabel, string foundValue)> labels = new List<(string firstThingy, string secondThingyLabel, string foundValue)>()
{
    ("test1", "test2", "Value"),
    ("test1", "test1", "Value2"),
    ("test2", "test2", "Value3"),
}

public string FindMatchingValue(string firstElement, string secondElement)
{
    var result = labels
        .Where(w => w.firstThingy == firstElement && w.secondThingyLabel == secondElement)
        .FirstOrDefault();

    if (result == null)
        throw new ArgumentException("combo not found");

    return result.foundValue;
}

```

Though the naming on the example tuple above is pretty generic, the idea of relevant labels allows for a deeper understanding of what is being attempted in the code over referencing "item1", "item2", and "item3".

### Differences between ValueTuple and Tuple

The primary reason for introduction of `ValueTuple` is performance.

|Type name|`ValueTuple`|`Tuple`
|---|---|---|---|---|---|---|---|---|---
|Class or structure|`struct`|`class`
|Mutability (changing values after creation)|mutable|immutable
|Naming members and other language support|yes|no ([TBD](https://github.com/dotnet/roslyn/issues/11031))

### References

- [Original Tuples language feature proposal on GitHub](https://github.com/dotnet/roslyn/issues/347)
- [A runnable VS 15 solution for C# 7.0 features](https://code.msdn.microsoft.com/Introduce-new-C-70-features-c639ed88)
- [NuGet Tuple Package](https://www.nuget.org/packages/System.ValueTuple/)



## Local functions


Local functions are defined within a method and aren't available outside of it. They have access to all local variables and support iterators, `async`/`await` and lambda syntax. This way, repetitions specific to a function can be functionalized without crowding the class. As a side effect, this improves intellisense suggestion performance.

### Example

```cs
double GetCylinderVolume(double radius, double height)
{
    return getVolume();

    double getVolume()
    {
        // You can declare inner-local functions in a local function 
        double GetCircleArea(double r) => Math.PI * r * r;

        // ALL parents' variables are accessible even though parent doesn't have any input. 
        return GetCircleArea(radius) * height;
    }
}

```

Local functions considerably simplify code for LINQ operators, where you usually have to separate argument checks from actual logic to make argument checks instant, not delayed until after iteration started.

### Example

```cs
public static IEnumerable<TSource> Where<TSource>(
    this IEnumerable<TSource> source, 
    Func<TSource, bool> predicate)
{
    if (source == null) throw new ArgumentNullException(nameof(source));
    if (predicate == null) throw new ArgumentNullException(nameof(predicate));

    return iterator();

    IEnumerable<TSource> iterator()
    {
        foreach (TSource element in source)
            if (predicate(element))
                yield return element;
    }
}

```

Local functions also support the `async` and `await` keywords.

### Example

```cs
async Task WriteEmailsAsync()
{
    var emailRegex = new Regex(@"(?i)[a-z0-9_.+-]+@[a-z0-9-]+\.[a-z0-9-.]+");
    IEnumerable<string> emails1 = await getEmailsFromFileAsync("input1.txt");
    IEnumerable<string> emails2 = await getEmailsFromFileAsync("input2.txt");
    await writeLinesToFileAsync(emails1.Concat(emails2), "output.txt");

    async Task<IEnumerable<string>> getEmailsFromFileAsync(string fileName)
    {
        string text;

        using (StreamReader reader = File.OpenText(fileName))
        {
            text = await reader.ReadToEndAsync();
        }

        return from Match emailMatch in emailRegex.Matches(text) select emailMatch.Value;
    }

    async Task writeLinesToFileAsync(IEnumerable<string> lines, string fileName)
    {
        using (StreamWriter writer = File.CreateText(fileName))
        {
            foreach (string line in lines)
            {
                await writer.WriteLineAsync(line);
            }
        }
    }
}

```

One important thing that you may have noticed is that local functions can be defined under the `return` statement, they do **not** need to be defined above it. Additionally, local functions typically follow the "lowerCamelCase" naming convention as to more easily differentiate themselves from class scope functions.



## out var declaration


A common pattern in C# is using `bool TryParse(object input, out object value)` to safely parse objects.

The `out var` declaration is a simple feature to improve readability. It allows a variable to be declared at the same time that is it passed as an out parameter.

A variable declared this way is scoped to the remainder of the body at the point in which it is declared.

### Example

Using `TryParse` prior to C# 7.0, you must declare a variable to receive the value before calling the function:

```cs
int value;
if (int.TryParse(input, out value)) 
{
    Foo(value); // ok
}
else
{
    Foo(value); // value is zero
}

Foo(value); // ok

```

In C# 7.0, you can inline the declaration of the variable passed to the `out` parameter, eliminating the need for a separate variable declaration:

```cs
if (int.TryParse(input, out var value)) 
{
    Foo(value); // ok
}
else
{
    Foo(value); // value is zero
}

Foo(value); // still ok, the value in scope within the remainder of the body

```

If some of the parameters that a function returns in `out` is not needed you can use the **discard** operator `_`.

```cs
p.GetCoordinates(out var x, out _); // I only care about x

```

An `out var` declaration can be used with any existing function which already has `out` parameters. The function declaration syntax remains the same, and no additional requirements are needed to make the function compatible with an `out var` declaration. This feature is simply syntactic sugar.

Another feature of `out var` declaration is that it can be used with anonymous types.

```cs
var a = new[] { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
var groupedByMod2 = a.Select(x => new
                                  {
                                      Source = x,
                                      Mod2 = x % 2
                                  })
                     .GroupBy(x => x.Mod2)
                     .ToDictionary(g => g.Key, g => g.ToArray());
if (groupedByMod2.TryGetValue(1, out var oddElements))
{
    Console.WriteLine(oddElements.Length);
}

```

In this code we create a `Dictionary` with `int` key and array of anonymous type value. In the previous version of C# it was impossible to use `TryGetValue` method here since it required you to declare the `out` variable (which is of anonymous type!). However, with `out var` we do not need to explicitly specify the type of the `out` variable.

### Limitations

Note that out var declarations are of limited use in LINQ queries as expressions are interpreted as expression lambda bodies, so the scope of the introduced variables is limited to these lambdas. For example, the following code will not work:

```cs
var nums = 
    from item in seq
    let success = int.TryParse(item, out var tmp)
    select success ? tmp : 0; // Error: The name 'tmp' does not exist in the current context

```

### References

- [Original out var declaration proposal on GitHub](https://github.com/dotnet/roslyn/issues/6183)



## Pattern Matching


Pattern matching extensions for C# enable many of the benefits of pattern matching from functional languages, but in a way that smoothly integrates with the feel of the underlying language

### **`switch` expression**

Pattern matching extends the `switch` statement to switch on types:

```cs
class Geometry {} 

class Triangle : Geometry
{
    public int Width { get; set; }
    public int Height { get; set; }
    public int Base { get; set; }
}

class Rectangle : Geometry
{
    public int Width { get; set; }
    public int Height { get; set; }
}

class Square : Geometry
{
    public int Width { get; set; }
}

public static void PatternMatching()
{
    Geometry g = new Square { Width = 5 }; 
    
    switch (g)
    {
        case Triangle t:
            Console.WriteLine($"{t.Width} {t.Height} {t.Base}");
            break;
        case Rectangle sq when sq.Width == sq.Height:
            Console.WriteLine($"Square rectangle: {sq.Width} {sq.Height}");
            break;
        case Rectangle r:
            Console.WriteLine($"{r.Width} {r.Height}");
            break;
        case Square s:
            Console.WriteLine($"{s.Width}");
            break;
        default:
            Console.WriteLine("<other>");
            break;
    }
}

```

### **`is` expression**

Pattern matching extends the `is` operator to check for a type and declare a new variable at the same time.

### Example

```cs
string s = o as string;
if(s != null)
{
    // do something with s
}

```

can be rewritten as:

```cs
if(o is string s)
{
    //Do something with s
};

```

Also note that the scope of the pattern variable `s` is extended to outside the `if` block reaching the end of the enclosing scope, example:

```cs
if(someCondition)
{
   if(o is string s)
   {
      //Do something with s
   }
   else
   {
     // s is unassigned here, but accessible 
   }

   // s is unassigned here, but accessible 
}
// s is not accessible here

```



## Digit separators


The underscore `_` may be used as a digit separator. Being able to group digits in large numeric literals has a significant impact on readability.

The underscore may occur anywhere in a numeric literal except as noted below. Different groupings may make sense in different scenarios or with different numeric bases.

Any sequence of digits may be separated by one or more underscores. The `_` is allowed in decimals as well as exponents. The separators have no semantic impact - they are simply ignored.

```cs
int bin = 0b1001_1010_0001_0100;
int hex = 0x1b_a0_44_fe;
int dec = 33_554_432;
int weird = 1_2__3___4____5_____6______7_______8________9;
double real = 1_000.111_1e-1_000;

```

**Where the `_` digit separator may not be used:**

- at the beginning of the value (`_121`)
- at the end of the value (`121_` or `121.05_`)
- next to the decimal (`10_.0`)
- next to the exponent character (`1.1e_1`)
- next to the type specifier (`10_f`)
- immediately following the `0x` or `0b` in binary and hexadecimal literals ([might be changed to allow e.g. 0b_1001_1000](https://github.com/dotnet/roslyn/issues/12680))



## Binary literals


The **0b** prefix can be used to represent Binary literals.

Binary literals allow constructing numbers from zeroes and ones, which makes seeing which bits are set in the binary representation of a number much easier. This can be useful for working with binary flags.

The following are equivalent ways of specifying an `int` with value `34` (=2<sup>5</sup> + 2<sup>1</sup>):

```cs
// Using a binary literal:
//   bits: 76543210
int a1 = 0b00100010;          // binary: explicitly specify bits

// Existing methods:
int a2 = 0x22;                // hexadecimal: every digit corresponds to 4 bits
int a3 = 34;                  // decimal: hard to visualise which bits are set
int a4 = (1 << 5) | (1 << 1); // bitwise arithmetic: combining non-zero bits

```

### Flags enumerations

Before, specifying flag values for an `enum` could only be done using one of the three methods in this example:

```cs
[Flags]
public enum DaysOfWeek
{
    // Previously available methods:
    //          decimal        hex       bit shifting
    Monday    =  1,    //    = 0x01    = 1 << 0
    Tuesday   =  2,    //    = 0x02    = 1 << 1
    Wednesday =  4,    //    = 0x04    = 1 << 2
    Thursday  =  8,    //    = 0x08    = 1 << 3
    Friday    = 16,    //    = 0x10    = 1 << 4
    Saturday  = 32,    //    = 0x20    = 1 << 5
    Sunday    = 64,    //    = 0x40    = 1 << 6

    Weekdays = Monday | Tuesday | Wednesday | Thursday | Friday,
    Weekends = Saturday | Sunday
}

```

With binary literals it is more obvious which bits are set, and using them does not require understanding hexadecimal numbers and bitwise arithmetic:

```cs
[Flags]
public enum DaysOfWeek
{
    Monday    = 0b00000001,
    Tuesday   = 0b00000010,
    Wednesday = 0b00000100,
    Thursday  = 0b00001000,
    Friday    = 0b00010000,
    Saturday  = 0b00100000,
    Sunday    = 0b01000000,

    Weekdays = Monday | Tuesday | Wednesday | Thursday | Friday,
    Weekends = Saturday | Sunday
}

```



## throw expressions


C# 7.0 allows throwing as an expression in certain places:

```cs
class Person
{
    public string Name { get; }

    public Person(string name) => Name = name ?? throw new ArgumentNullException(nameof(name));

    public string GetFirstName()
    {
        var parts = Name.Split(' ');
        return (parts.Length > 0) ? parts[0] : throw new InvalidOperationException("No name!");
    }

    public string GetLastName() => throw new NotImplementedException();
}

```

Prior to C# 7.0, if you wanted to throw an exception from an expression body you would have to:

```cs
var spoons = "dinner,desert,soup".Split(',');

var spoonsArray = spoons.Length > 0 ? spoons : null;

if (spoonsArray == null) 
{
    throw new Exception("There are no spoons");
}

```

Or

```cs
var spoonsArray = spoons.Length > 0 
    ? spoons 
    : new Func<string[]>(() => 
      {
          throw new Exception("There are no spoons");
      })();

```

In C# 7.0 the above is now simplified to:

```cs
var spoonsArray = spoons.Length > 0 ? spoons : throw new Exception("There are no spoons");

```



## Extended expression bodied members list


C# 7.0 adds accessors, constructors and finalizers to the list of things that can have expression bodies:

```cs
class Person
{
    private static ConcurrentDictionary<int, string> names = new ConcurrentDictionary<int, string>();

    private int id = GetId();

    public Person(string name) => names.TryAdd(id, name); // constructors

    ~Person() => names.TryRemove(id, out _);              // finalizers

    public string Name
    {
        get => names[id];                                 // getters
        set => names[id] = value;                         // setters
    }
}

```

Also see the [out var declaration](http://stackoverflow.com/documentation/c%23/1936/c-sharp-7-0-features/6326/out-var-declaration) section for the discard operator.



## ref return and ref local


Ref returns and ref locals are useful for manipulating and returning references to blocks of memory instead of copying memory without resorting to unsafe pointers.

### Ref Return

```cs
public static ref TValue Choose<TValue>(
    Func<bool> condition, ref TValue left, ref TValue right)
{
    return condition() ? ref left : ref right;
}

```

With this you can pass two values by reference with one of them being returned based on some condition:

```cs
Matrix3D left = …, right = …;
Choose(chooser, ref left, ref right).M20 = 1.0;

```

### Ref Local

```cs
public static ref int Max(ref int first, ref int second, ref int third)
{
    ref int max = first > second ? ref first : ref second;
    return max > third ? ref max : ref third;
}
…
int a = 1, b = 2, c = 3;
Max(ref a, ref b, ref c) = 4;
Debug.Assert(a == 1); // true
Debug.Assert(b == 2); // true
Debug.Assert(c == 4); // true

```

### Unsafe Ref Operations

In `System.Runtime.CompilerServices.Unsafe` a set of unsafe operations have been defined that allow you to manipulate `ref` values as if they were pointers, basically.

For example, reinterpreting a memory address (`ref`) as a different type:

```cs
byte[] b = new byte[4] { 0x42, 0x42, 0x42, 0x42 };

ref int r = ref Unsafe.As<byte, int>(ref b[0]);
Assert.Equal(0x42424242, r);

0x0EF00EF0;
Assert.Equal(0xFE, b[0] | b[1] | b[2] | b[3]);

```

Beware of [endianness](https://en.wikipedia.org/wiki/Endianness) when doing this, though, e.g. check `BitConverter.IsLittleEndian` if needed and handle accordingly.

Or iterate over an array in an unsafe manner:

```cs
int[] a = new int[] { 0x123, 0x234, 0x345, 0x456 };

ref int r1 = ref Unsafe.Add(ref a[0], 1);
Assert.Equal(0x234, r1);

ref int r2 = ref Unsafe.Add(ref r1, 2);
Assert.Equal(0x456, r2);

ref int r3 = ref Unsafe.Add(ref r2, -3);
Assert.Equal(0x123, r3);

```

Or the similar `Subtract`:

```cs
string[] a = new string[] { "abc", "def", "ghi", "jkl" };

ref string r1 = ref Unsafe.Subtract(ref a[0], -2);
Assert.Equal("ghi", r1);

ref string r2 = ref Unsafe.Subtract(ref r1, -1);
Assert.Equal("jkl", r2);

ref string r3 = ref Unsafe.Subtract(ref r2, 3);
Assert.Equal("abc", r3);

```

Additionally, one can check if two `ref` values are the same i.e. same address:

```cs
long[] a = new long[2];

Assert.True(Unsafe.AreSame(ref a[0], ref a[0]));
Assert.False(Unsafe.AreSame(ref a[0], ref a[1]));

```

### Links

[Roslyn Github Issue](https://github.com/dotnet/roslyn/issues/118)

[System.Runtime.CompilerServices.Unsafe on github](https://github.com/dotnet/corefx/tree/master/src/System.Runtime.CompilerServices.Unsafe)



## ValueTask<T>


`Task<T>` is a **class** and causes the unnecessary overhead of its allocation when the result is immediately available.

`ValueTask<T>` is a **structure** and has been introduced to prevent the allocation of a `Task` object in case the result of the **async** operation is already available at the time of awaiting.

So `ValueTask<T>` provides two benefits:

### 1. Performance increase

Here's a `Task<T>` example:

- Requires heap allocation
- Takes 120ns with JIT

```cs
async Task<int> TestTask(int d)
{
    await Task.Delay(d);
    return 10;
}

```

Here's the analog `ValueTask<T>` example:

- No heap allocation if the result is known synchronously (which it is not in this case because of the `Task.Delay`, but often is in many real-world `async`/`await` scenarios)
- Takes 65ns with JIT

```cs
async ValueTask<int> TestValueTask(int d)
{
    await Task.Delay(d);
    return 10;
}

```

### 2. Increased implementation flexibility

Implementations of an async interface wishing to be synchronous would otherwise be forced to use either `Task.Run` or `Task.FromResult` (resulting in the performance penalty discussed above). Thus there's some pressure against synchronous implementations.

But with `ValueTask<T>`, implementations are more free to choose between being synchronous or asynchronous without impacting callers.

For example, here's an interface with an asynchronous method:

```cs
interface IFoo<T>
{
    ValueTask<T> BarAsync();
}

```

...and here's how that method might be called:

```cs
IFoo<T> thing = getThing();
var x = await thing.BarAsync();

```

With `ValueTask`, the above code will work with **either synchronous or asynchronous implementations**:

### Synchronous implementation:

```cs
class SynchronousFoo<T> : IFoo<T>
{
    public ValueTask<T> BarAsync()
    {
        var value = default(T);
        return new ValueTask<T>(value);
    }
}

```

### Asynchronous implementation

```cs
class AsynchronousFoo<T> : IFoo<T>
{
    public async ValueTask<T> BarAsync()
    {
        var value = default(T);
        await Task.Delay(1);
        return value;
    }
}

```

### Notes

Although `ValueTask` struct was being planned to be added to [C# 7.0](https://blogs.msdn.microsoft.com/dotnet/2016/08/24/whats-new-in-csharp-7-0/), it has been kept as another library for the time being.
[ValueTask<T>](http://stackoverflow.com/documentation/c%23/1936/c-sharp-7-0-features/28612/valuetaskt#)
`System.Threading.Tasks.Extensions` package can be downloaded from [Nuget Gallery](https://www.nuget.org/packages/System.Threading.Tasks.Extensions/)

