---
metaTitle: "C# | Anonymous types"
description: "Anonymous vs dynamic, Creating an anonymous type, Anonymous type equality, Generic methods with anonymous types, Instantiating generic types with anonymous types, Implicitly typed arrays"
---

# Anonymous types



## Anonymous vs dynamic


Anonymous types allow the creation of objects without having to explicitly define their types ahead of time, while maintaining static type checking.

```cs
var anon = new { Value = 1 };
Console.WriteLine(anon.Id); // compile time error

```

Conversely, `dynamic` has dynamic type checking, opting for runtime errors, instead of compile-time errors.

```cs
dynamic val = "foo";
Console.WriteLine(val.Id); // compiles, but throws runtime error

```



## Creating an anonymous type


Since anonymous types are not named, variables of those types must be implicitly typed (`var`).

```cs
var anon = new { Foo = 1, Bar = 2 };
// anon.Foo == 1
// anon.Bar == 2

```

If the member names are not specified, they are set to the name of the property/variable used to initialize the object.

```cs
int foo = 1;
int bar = 2;
var anon2 = new { foo, bar };
// anon2.foo == 1
// anon2.bar == 2

```

Note that names can only be omitted when the expression in the anonymous type declaration is a simple property access; for method calls or more complex expressions, a property name must be specified.

```cs
string foo = "some string";
var anon3 = new { foo.Length };
// anon3.Length == 11
var anon4 = new { foo.Length <= 10 ? "short string" : "long string" };
// compiler error - Invalid anonymous type member declarator.
var anon5 = new { Description = foo.Length <= 10 ? "short string" : "long string" };
// OK

```



## Anonymous type equality


Anonymous type equality is given by the `Equals` instance method. Two objects are equal if they have the same type and equal values (through `a.Prop.Equals(b.Prop)`) for every property.

```cs
var anon = new { Foo = 1, Bar = 2 };
var anon2 = new { Foo = 1, Bar = 2 };
var anon3 = new { Foo = 5, Bar = 10 };
var anon3 = new { Foo = 5, Bar = 10 };
var anon4 = new { Bar = 2, Foo = 1 };
// anon.Equals(anon2) == true
// anon.Equals(anon3) == false
// anon.Equals(anon4) == false (anon and anon4 have different types, see below)

```

Two anonymous types are considered the same if and only if their properties have the same name and type and appear in the same order.

```cs
var anon = new { Foo = 1, Bar = 2 };
var anon2 = new { Foo = 7, Bar = 1 };
var anon3 = new { Bar = 1, Foo = 3 };
var anon4 = new { Fa = 1, Bar = 2 };
// anon and anon2 have the same type
// anon and anon3 have diferent types (Bar and Foo appear in different orders)
// anon and anon4 have different types (property names are different)

```



## Generic methods with anonymous types


Generic methods allow the use of anonymous types through type inference.

```cs
void Log<T>(T obj) {
    // ...
}
Log(new { Value = 10 });

```

This means LINQ expressions can be used with anonymous types:

```cs
var products = new[] {
    new { Amount = 10, Id = 0 },
    new { Amount = 20, Id = 1 },
    new { Amount = 15, Id = 2 }
};
var idsByAmount = products.OrderBy(x => x.Amount).Select(x => x.Id);
// idsByAmount: 0, 2, 1

```



## Instantiating generic types with anonymous types


Using generic constructors would require the anonymous types to be named, which is not possible. Alternatively, generic methods may be used to allow type inference to occur.

```cs
var anon = new { Foo = 1, Bar = 2 };
var anon2 = new { Foo = 5, Bar = 10 };
List<T> CreateList<T>(params T[] items) {
    return new List<T>(items);
}

var list1 = CreateList(anon, anon2);

```

In the case of `List<T>`, implicitly typed arrays may be converted to a `List<T>` through the `ToList` LINQ method:

```cs
var list2 = new[] {anon, anon2}.ToList();

```



## Implicitly typed arrays


Arrays of anonymous types may be created with implicit typing.

```cs
var arr = new[] {
    new { Id = 0 },
    new { Id = 1 }
};

```

