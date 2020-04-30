---
metaTitle: "Lambda Expressions"
description: "Using lambda syntax to create a closure, Basic lambda expressions, Basic lambda expressions with LINQ, Lambda syntax with statement block body, Lambda expressions with System.Linq.Expressions"
---

# Lambda Expressions



## Using lambda syntax to create a closure


See remarks for discussion of closures. Suppose we have an interface:

```cs
public interface IMachine<TState, TInput>
{
    TState State { get; }
    public void Input(TInput input);
}

```

and then the following is executed:

```cs
IMachine<int, int> machine = ...;
Func<int, int> machineClosure = i => {
    machine.Input(i);
    return machine.State;
};

```

Now `machineClosure` refers to a function from `int` to `int`, which behind the scenes uses the `IMachine` instance which `machine` refers to in order to carry out the computation. Even if the reference `machine` goes out of scope, as long as the `machineClosure` object is maintained, the original `IMachine` instance will be retained as part of a 'closure', automatically defined by the compiler.

Warning: this can mean that the same function call returns different values at different times (e.g. In this example if the machine keeps a sum of its inputs). In lots of cases, this may be unexpected and is to be avoided for any code in a functional style - accidental and unexpected closures can be a source of bugs.



## Basic lambda expressions


```cs
Func<int, int> add1 = i => i + 1;

Func<int, int, int> add = (i, j) => i + j;

// Behaviourally equivalent to:

int Add1(int i)
{
    return i + 1;
}

int Add(int i, int j)
{
    return i + j;
}

...

Console.WriteLine(add1(42)); //43
Console.WriteLine(Add1(42)); //43
Console.WriteLine(add(100, 250)); //350
Console.WriteLine(Add(100, 250)); //350

```



## Basic lambda expressions with LINQ


```cs
// assume source is {0, 1, 2, ..., 10}

var evens = source.Where(n => n%2 == 0);
// evens = {0, 2, 4, ... 10}

var strings = source.Select(n => n.ToString());
// strings = {"0", "1", ..., "10"}

```



## Lambda syntax with statement block body


```cs
Func<int, string> doubleThenAddElevenThenQuote = i => {
    var doubled = 2 * i;
    var addedEleven = 11 + doubled;
    return $"'{addedEleven}'";
};

```



## Lambda expressions with System.Linq.Expressions


```cs
Expression<Func<int, bool>> checkEvenExpression = i => i%2 == 0;
// lambda expression is automatically converted to an Expression<Func<int, bool>>

```



#### Remarks


### Closures

Lambda expressions will implicitly [capture variables used and create a closure](http://csharpindepth.com/Articles/Chapter5/Closures.aspx). A closure is a function along with some state context. The compiler will generate a closure whenever a lambda expression 'encloses' a value from its surrounding context.

E.g. when the following is executed

```cs
Func<object, bool> safeApplyFiltererPredicate = o => (o != null) && filterer.Predicate(i);

```

`safeApplyFilterPredicate` refers to a newly created object which has a private reference to the current value of `filterer`, and whose `Invoke` method behaves like

```cs
o => (o != null) && filterer.Predicate(i);

```

This can be important, because as long as the reference to the value now in `safeApplyFilterPredicate` is maintained, there will be a reference to the object which `filterer` currently refers to. This has an effect on garbage collection, and may cause unexpected behaviour if the object which `filterer` currently refers to is mutated.

On the other hand, closures can be used to deliberate effect to encapsulate a behaviour which involves references to other objects.

E.g.

```cs
var logger = new Logger();
Func<int, int> Add1AndLog = i => {
    logger.Log("adding 1 to " + i);
    return (i + 1);
};

```

Closures can also be used to model state machines:

```cs
Func<int, int> MyAddingMachine() {
    var i = 0;
    return x => i += x;
};

```

