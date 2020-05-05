---
metaTitle: "C# | Nullable types"
description: "Initialising a nullable, Check if a Nullable has a value, Get the value of a nullable type, Getting a default value from a nullable, Check if a generic type parameter is a nullable type, Default value of nullable types is null, Effective usage of underlying Nullable<T> argument"
---

# Nullable types



## Initialising a nullable


For `null` values:

```cs
Nullable<int> i = null;

```

Or:

```cs
int? i = null;

```

Or:

```cs
var i = (int?)null;

```

For non-null values:

```cs
Nullable<int> i = 0;

```

Or:

```cs
int? i = 0;

```



## Check if a Nullable has a value


```cs
int? i = null;

if (i != null)
{
    Console.WriteLine("i is not null");
}
else
{
    Console.WriteLine("i is null");
}

```

Which is the same as:

```cs
if (i.HasValue)
{
    Console.WriteLine("i is not null");
}
else
{
    Console.WriteLine("i is null");
}

```



## Get the value of a nullable type


Given following nullable `int`

```cs
int? i = 10;

```

In case default value is needed, you can assign one using [null coalescing operator](http://stackoverflow.com/documentation/c%23/37/null-coalescing-operator#t=201512031945577057448), `GetValueOrDefault` method or check if nullable int `HasValue` before assignment.

```cs
int j = i ?? 0;
int j = i.GetValueOrDefault(0);
int j = i.HasValue ? i.Value : 0;

```

The following usage is always **unsafe**. If `i` is null at runtime, a `System.InvalidOperationException` will be thrown. At design time, if a value is not set, you'll get a `Use of unassigned local variable 'i'` error.

```cs
int j = i.Value;

```



## Getting a default value from a nullable


The `.GetValueOrDefault()` method returns a value even if the `.HasValue` property is false (unlike the Value property, which throws an exception).

```cs
class Program
{
    static void Main()
    {
        int? nullableExample = null;
        int result = nullableExample.GetValueOrDefault();
        Console.WriteLine(result); // will output the default value for int - 0
        int secondResult = nullableExample.GetValueOrDefault(1);
        Console.WriteLine(secondResult) // will output our specified default - 1
        int thirdResult = nullableExample ?? 1;
        Console.WriteLine(secondResult) // same as the GetValueOrDefault but a bit shorter
    }
}

```

Output:

```cs
0
1

```



## Check if a generic type parameter is a nullable type


```cs
public bool IsTypeNullable<T>()
{
    return Nullable.GetUnderlyingType( typeof(T) )!=null;
}

```



## Default value of nullable types is null


```cs
public class NullableTypesExample
{
    static int? _testValue;

    public static void Main()
    {
        if(_testValue == null)
            Console.WriteLine("null");
        else
            Console.WriteLine(_testValue.ToString());
    }
}

```

Output:

> 
null




## Effective usage of underlying Nullable<T> argument


Any nullable type is a **generic** type. And any nullable type is a **value** type.

There are some tricks which allow to **effectively use** the result of the [Nullable.GetUnderlyingType](https://msdn.microsoft.com/en-us/library/system.nullable.getunderlyingtype(v=vs.110).aspx) method when creating code related to [reflection](http://stackoverflow.com/documentation/c%23/28/reflection)/code-generation purposes:

```cs
public static class TypesHelper {
    public static bool IsNullable(this Type type) {
        Type underlyingType;
        return IsNullable(type, out underlyingType);
    }
    public static bool IsNullable(this Type type, out Type underlyingType) {
        underlyingType = Nullable.GetUnderlyingType(type);
        return underlyingType != null;
    }
    public static Type GetNullable(Type type) {
        Type underlyingType;
        return IsNullable(type, out underlyingType) ? type : NullableTypesCache.Get(type);
    }
    public static bool IsExactOrNullable(this Type type, Func<Type, bool> predicate) {
        Type underlyingType;
        if(IsNullable(type, out underlyingType))
            return IsExactOrNullable(underlyingType, predicate);
        return predicate(type);
    }
    public static bool IsExactOrNullable<T>(this Type type)
        where T : struct {
        return IsExactOrNullable(type, t => Equals(t, typeof(T)));
    }
}

```

The usage:

```cs
Type type = typeof(int).GetNullable();
Console.WriteLine(type.ToString());

if(type.IsNullable())
    Console.WriteLine("Type is nullable.");
Type underlyingType;
if(type.IsNullable(out underlyingType))
    Console.WriteLine("The underlying type is " + underlyingType.Name + ".");
if(type.IsExactOrNullable<int>())
    Console.WriteLine("Type is either exact or nullable Int32.");
if(!type.IsExactOrNullable(t => t.IsEnum))
    Console.WriteLine("Type is neither exact nor nullable enum.");

```

Output:

```cs
System.Nullable`1[System.Int32]
Type is nullable.
The underlying type is Int32.
Type is either exact or nullable Int32.
Type is neither exact nor nullable enum.

```

PS. The `NullableTypesCache` is defined as follows:

```cs
static class NullableTypesCache {
    readonly static ConcurrentDictionary<Type, Type> cache = new ConcurrentDictionary<Type, Type>();
    static NullableTypesCache() {
        cache.TryAdd(typeof(byte), typeof(Nullable<byte>));
        cache.TryAdd(typeof(short), typeof(Nullable<short>));
        cache.TryAdd(typeof(int), typeof(Nullable<int>));
        cache.TryAdd(typeof(long), typeof(Nullable<long>));
        cache.TryAdd(typeof(float), typeof(Nullable<float>));
        cache.TryAdd(typeof(double), typeof(Nullable<double>));
        cache.TryAdd(typeof(decimal), typeof(Nullable<decimal>));
        cache.TryAdd(typeof(sbyte), typeof(Nullable<sbyte>));
        cache.TryAdd(typeof(ushort), typeof(Nullable<ushort>));
        cache.TryAdd(typeof(uint), typeof(Nullable<uint>));
        cache.TryAdd(typeof(ulong), typeof(Nullable<ulong>));
        //... 
    }
    readonly static Type NullableBase = typeof(Nullable<>);
    internal static Type Get(Type type) {
        // Try to avoid the expensive MakeGenericType method call
        return cache.GetOrAdd(type, t => NullableBase.MakeGenericType(t)); 
    }
}

```



#### Syntax


- `Nullable<int> i = 10;`
- int? j = 11;
- int? k = null;
- DateTime? DateOfBirth = DateTime.Now;
- decimal? Amount = 1.0m;
- bool? IsAvailable = true;
- char? Letter = 'a';
- (type)? variableName



#### Remarks


Nullable types can represent all the values of an underlying type as well as `null`.

The syntax **`T?`** is shorthand for `Nullable<T>`

Nullable values are `System.ValueType` objects actually, so they can be boxed and unboxed. Also, `null` value of a nullable object is not the same as `null` value of a reference object, it's just a flag.

When a nullable object boxing, the null value is converted to `null` reference, and non-null value is converted to non-nullable underlying type.

```cs
DateTime? dt = null;
var o = (object)dt;
var result = (o == null); // is true

DateTime? dt = new DateTime(2015, 12, 11);
var o = (object)dt;
var dt2 = (DateTime)dt; // correct cause o contains DateTime value

```

The second rule leads to correct, but paradoxical code:

```cs
DateTime? dt = new DateTime(2015, 12, 11);
var o = (object)dt;
var type = o.GetType(); // is DateTime, not Nullable<DateTime>

```

In short form:

```cs
DateTime? dt = new DateTime(2015, 12, 11);
var type = dt.GetType(); // is DateTime, not Nullable<DateTime>

```

