---
metaTitle: "C# | Reflection"
description: "Get the members of a type, Get a method and invoke it, Creating an instance of a Type, Get a generic method and invoke it, Get a Strongly-Typed Delegate to a Method or Property via Reflection, Get a System.Type, Getting and setting properties, Custom Attributes,  Create an instance of a Generic Type and invoke it's method, Instantiating classes that implement an interface (e.g. plugin activation), Determining generic arguments of instances of generic types, Get a Type by name with namespace, Looping through all the properties of a class"
---

# Reflection


Reflection is a C# language mechanism for accessing dynamic object properties on runtime. Typically, reflection is used to fetch the information about dynamic object type and object attribute values. In REST application, for example, reflection could be used to iterate through serialized response object.

Remark:
According to MS guidelines performance critical code should avoid reflection. See [https://msdn.microsoft.com/en-us/library/ff647790.aspx](https://msdn.microsoft.com/en-us/library/ff647790.aspx)



## Get the members of a type


```cs
using System;
using System.Reflection;
using System.Linq;
                
public class Program
{
  public static void Main()
  {
    var members = typeof(object)
                    .GetMembers(BindingFlags.Public |
                                BindingFlags.Static |
                                BindingFlags.Instance);
    
    foreach (var member in members)
    {
      bool inherited = member.DeclaringType.Equals( typeof(object).Name );
      Console.WriteLine($"{member.Name} is a {member.MemberType}, " +
                        $"it has {(inherited ? "":"not")} been inherited.");
    }
  }
}

```

Output (**see note about output order further down**):

We can also use the `GetMembers()` without passing any `BindingFlags`. This will return **all** public members of that specific type.

One thing to note that `GetMembers` does not return the members in any particular order, so never rely on the order that `GetMembers` returns you.

[View Demo](https://dotnetfiddle.net/bJczwn)



## Get a method and invoke it


**Get Instance method and invoke it**

```cs
using System;
                
public class Program
{
    public static void Main()
    {
        var theString = "hello";
        var method = theString
                     .GetType()
                     .GetMethod("Substring",
                                new[] {typeof(int), typeof(int)}); //The types of the method arguments
         var result = method.Invoke(theString, new object[] {0, 4});
         Console.WriteLine(result);
    }
}

```

**Output:**

> 
hell


[View Demo](https://dotnetfiddle.net/AF8RVe)

**Get Static method and invoke it**

On the other hand, if the method is static, you do not need an instance to call it.

```cs
var method = typeof(Math).GetMethod("Exp");
var result = method.Invoke(null, new object[] {2});//Pass null as the first argument (no need for an instance)
Console.WriteLine(result); //You'll get e^2

```

**Output:**

> 
7.38905609893065


[View Demo](https://dotnetfiddle.net/vNEsyk)



## Creating an instance of a Type


The simplest way is to use the `Activator` class.

However, even though `Activator` performance have been improved since .NET 3.5, using `Activator.CreateInstance()` is bad option sometimes, due to (relatively) low performance: [Test 1](https://blogs.msdn.microsoft.com/haibo_luo/2005/11/17/activator-createinstance-and-beyond/), [Test 2](https://codingsolution.wordpress.com/2013/07/12/activator-createinstance-is-slow/), [Test 3](http://stackoverflow.com/questions/6069661/does-system-activator-createinstancet-have-performance-issues-big-enough-to-di)...

### With `Activator` class

```cs
Type type = typeof(BigInteger);
object result = Activator.CreateInstance(type); //Requires parameterless constructor.
Console.WriteLine(result); //Output: 0
result = Activator.CreateInstance(type, 123); //Requires a constructor which can receive an 'int' compatible argument.
Console.WriteLine(result); //Output: 123

```

You can pass an object array to `Activator.CreateInstance` if you have more than one parameter.

```cs
// With a constructor such as MyClass(int, int, string)
Activator.CreateInstance(typeof(MyClass), new object[] { 1, 2, "Hello World" });

Type type = typeof(someObject);
var instance = Activator.CreateInstance(type);

```

**For a generic type**

The `MakeGenericType` method turns an open generic type (like `List<>`) into a concrete type (like `List<string>`) by applying type arguments to it.

```cs
// generic List with no parameters
Type openType = typeof(List<>);

// To create a List<string>
Type[] tArgs = { typeof(string) };
Type target = openType.MakeGenericType(tArgs);

// Create an instance - Activator.CreateInstance will call the default constructor.
// This is equivalent to calling new List<string>().
List<string> result = (List<string>)Activator.CreateInstance(target);

```

The `List<>` syntax is not permitted outside of a `typeof` expression.

### Without `Activator` class

**Using `new` keyword (will do for parameterless constructors)**

```cs
T GetInstance<T>() where T : new()
{
    T instance = new T();
    return instance;
}

```

**Using Invoke method**

```cs
// Get the instance of the desired constructor (here it takes a string as a parameter).
ConstructorInfo c = typeof(T).GetConstructor(new[] { typeof(string) }); 
// Don't forget to check if such constructor exists
if (c == null) 
    throw new InvalidOperationException(string.Format("A constructor for type '{0}' was not found.", typeof(T)));
T instance = (T)c.Invoke(new object[] { "test" });

```

**Using Expression trees**

Expression trees represent code in a tree-like data structure, where each node is an expression.
As [MSDN](https://msdn.microsoft.com/en-us/library/ms173144.aspx) explains:

> 
<p>Expression is a sequence of one or more operands and zero or more
operators that can be evaluated to a single value, object, method, or
namespace. Expressions can consist of a literal value, a method
invocation, an operator and its operands, or a simple name. Simple
names can be the name of a variable, type member, method parameter,
namespace or type.</p>


```cs
public class GenericFactory<TKey, TType>
    {
       private readonly Dictionary<TKey, Func<object[], TType>> _registeredTypes; // dictionary, that holds constructor functions.
       private object _locker = new object(); // object for locking dictionary, to guarantee thread safety

        public GenericFactory()
        {
            _registeredTypes = new Dictionary<TKey, Func<object[], TType>>();
        }

        /// <summary>
        /// Find and register suitable constructor for type
        /// </summary>
        /// <typeparam name="TType"></typeparam>
        /// <param name="key">Key for this constructor</param>
        /// <param name="parameters">Parameters</param>
        public void Register(TKey key, params Type[] parameters)
        {
            ConstructorInfo ci = typeof(TType).GetConstructor(BindingFlags.Public | BindingFlags.Instance, null, CallingConventions.HasThis, parameters, new ParameterModifier[] { }); // Get the instance of ctor.
            if (ci == null)
                throw new InvalidOperationException(string.Format("Constructor for type '{0}' was not found.", typeof(TType)));

            Func<object[], TType> ctor;

            lock (_locker)
            {
                if (!_registeredTypes.TryGetValue(key, out ctor)) // check if such ctor already been registered
                {
                    var pExp = Expression.Parameter(typeof(object[]), "arguments"); // create parameter Expression
                    var ctorParams = ci.GetParameters(); // get parameter info from constructor

                    var argExpressions = new Expression[ctorParams.Length]; // array that will contains parameter expessions
                    for (var i = 0; i < parameters.Length; i++)
                    {

                        var indexedAcccess = Expression.ArrayIndex(pExp, Expression.Constant(i));

                        if (!parameters[i].IsClass && !parameters[i].IsInterface) // check if parameter is a value type
                        {
                            var localVariable = Expression.Variable(parameters[i], "localVariable"); // if so - we should create local variable that will store paraameter value

                            var block = Expression.Block(new[] { localVariable },
                                    Expression.IfThenElse(Expression.Equal(indexedAcccess, Expression.Constant(null)),
                                        Expression.Assign(localVariable, Expression.Default(parameters[i])),
                                        Expression.Assign(localVariable, Expression.Convert(indexedAcccess, parameters[i]))
                                    ),
                                    localVariable
                                );

                            argExpressions[i] = block;

                        }
                        else
                            argExpressions[i] = Expression.Convert(indexedAcccess, parameters[i]);
                    }
                    var newExpr = Expression.New(ci, argExpressions); // create expression that represents call to specified ctor with the specified arguments.
  
                    _registeredTypes.Add(key, Expression.Lambda(newExpr, new[] { pExp }).Compile() as Func<object[], TType>); // compile expression to create delegate, and add fucntion to dictionary
                }
            }
        }

        /// <summary>
        /// Returns instance of registered type by key.
        /// </summary>
        /// <typeparam name="TType"></typeparam>
        /// <param name="key"></param>
        /// <param name="args"></param>
        /// <returns></returns>
        public TType Create(TKey key, params object[] args)
        {
            Func<object[], TType> foo;
            if (_registeredTypes.TryGetValue(key, out foo))
            {
                return (TType)foo(args);
            }

            throw new ArgumentException("No type registered for this key.");
        }
    }

```

Could be used like this:

```cs

public class TestClass
 {
        public TestClass(string parameter)
        {
            Console.Write(parameter);
        }
 } 


public void TestMethod()
{
       var factory = new GenericFactory<string, TestClass>();
       factory.Register("key", typeof(string));
       TestClass newInstance = factory.Create("key", "testParameter");
}

```

**Using FormatterServices.GetUninitializedObject**

```cs
T instance = (T)FormatterServices.GetUninitializedObject(typeof(T));

```

In case of using `FormatterServices.GetUninitializedObject`
constructors and field initializers will not be called. It is meant to be used in serializers and remoting engines



## Get a generic method and invoke it


Let's say you have class with generic methods. And you need to call its functions with reflection.

```cs
public class Sample
{
    public void GenericMethod<T>()
    {
        // ...
    }

    public static void StaticMethod<T>()
    {
        //...
    }
}

```

Let's say we want to call the GenericMethod with type string.

```cs
Sample sample = new Sample();//or you can get an instance via reflection

MethodInfo method = typeof(Sample).GetMethod("GenericMethod");
MethodInfo generic = method.MakeGenericMethod(typeof(string));
generic.Invoke(sample, null);//Since there are no arguments, we are passing null

```

For the static method you do not need an instance. Therefore the first argument will also be null.

```cs
MethodInfo method = typeof(Sample).GetMethod("StaticMethod");
MethodInfo generic = method.MakeGenericMethod(typeof(string));
generic.Invoke(null, null);

```



## Get a Strongly-Typed Delegate to a Method or Property via Reflection


When performance is a concern, invoking a method via reflection (i.e. via the `MethodInfo.Invoke` method) is not ideal.  However, it is relatively straightforward to obtain a more performant strongly-typed delegate using the `Delegate.CreateDelegate` function.  The performance penalty for using reflection is incurred only during the delegate-creation process.  Once the delegate is created, there is little-to-no performance penalty for invoking it:

```cs
// Get a MethodInfo for the Math.Max(int, int) method...
var maxMethod = typeof(Math).GetMethod("Max", new Type[] { typeof(int), typeof(int) });
// Now get a strongly-typed delegate for Math.Max(int, int)...
var stronglyTypedDelegate = (Func<int, int, int>)Delegate.CreateDelegate(typeof(Func<int, int, int>), null, maxMethod);
// Invoke the Math.Max(int, int) method using the strongly-typed delegate...
Console.WriteLine("Max of 3 and 5 is: {0}", stronglyTypedDelegate(3, 5));

```

This technique can be extended to properties as well.  If we have a class named `MyClass` with an `int` property named `MyIntProperty`, the code to get a strongly-typed getter would be (the following example assumes 'target' is a valid instance of `MyClass`):

```cs
// Get a MethodInfo for the MyClass.MyIntProperty getter...
var theProperty = typeof(MyClass).GetProperty("MyIntProperty");
var theGetter = theProperty.GetGetMethod();
// Now get a strongly-typed delegate for MyIntProperty that can be executed against any MyClass instance...
var stronglyTypedGetter = (Func<MyClass, int>)Delegate.CreateDelegate(typeof(Func<MyClass, int>), theGetter);
// Invoke the MyIntProperty getter against MyClass instance 'target'...
Console.WriteLine("target.MyIntProperty is: {0}", stronglyTypedGetter(target));

```

...and the same can be done for the setter:

```cs
// Get a MethodInfo for the MyClass.MyIntProperty setter...
var theProperty = typeof(MyClass).GetProperty("MyIntProperty");
var theSetter = theProperty.GetSetMethod();
// Now get a strongly-typed delegate for MyIntProperty that can be executed against any MyClass instance...
var stronglyTypedSetter = (Action<MyClass, int>)Delegate.CreateDelegate(typeof(Action<MyClass, int>), theSetter);
// Set MyIntProperty to 5...
stronglyTypedSetter(target, 5);

```



## Get a System.Type


For an instance of a type:

```cs
var theString = "hello";
var theType = theString.GetType();

```

From the type itself:

```cs
var theType = typeof(string);

```



## Getting and setting properties


Basic usage:

```cs
PropertyInfo prop = myInstance.GetType().GetProperty("myProperty");
// get the value myInstance.myProperty
object value = prop.GetValue(myInstance);

int newValue = 1;
// set the value myInstance.myProperty to newValue
prop.setValue(myInstance, newValue);

```

Setting read-only automatically-implemented properties can be done through it's backing field (in .NET Framework name of backing field is "k__BackingField"):

```cs
// get backing field info
FieldInfo fieldInfo = myInstance.GetType()
    .GetField("<myProperty>k__BackingField", BindingFlags.Instance | BindingFlags.NonPublic);

int newValue = 1;
// set the value of myInstance.myProperty backing field to newValue
fieldInfo.SetValue(myInstance, newValue);

```



## Custom Attributes


**Find properties with a custom attribute** - `MyAttribute`

```cs
var props = t.GetProperties(BindingFlags.NonPublic | BindingFlags.Public | 
            BindingFlags.Instance).Where(
            prop => Attribute.IsDefined(prop, typeof(MyAttribute)));

```

**Find all custom attributes on a given property**

```cs
var attributes = typeof(t).GetProperty("Name").GetCustomAttributes(false);

```

**Enumerate all classes with custom attribute** - `MyAttribute`

```cs
static IEnumerable<Type> GetTypesWithAttribute(Assembly assembly) {
    foreach(Type type in assembly.GetTypes()) {
        if (type.GetCustomAttributes(typeof(MyAttribute), true).Length > 0) {
            yield return type;
        }
    }
}

```

**Read value of a custom attribute at runtime**

```cs
public static class AttributeExtensions
{

        /// <summary>
        /// Returns the value of a member attribute for any member in a class.
        ///     (a member is a Field, Property, Method, etc...)    
        /// <remarks>
        /// If there is more than one member of the same name in the class, it will return the first one (this applies to overloaded methods)
        /// </remarks>
        /// <example>
        /// Read System.ComponentModel Description Attribute from method 'MyMethodName' in class 'MyClass': 
        ///     var Attribute = typeof(MyClass).GetAttribute("MyMethodName", (DescriptionAttribute d) => d.Description);
        /// </example>
        /// <param name="type">The class that contains the member as a type</param>
        /// <param name="MemberName">Name of the member in the class</param>
        /// <param name="valueSelector">Attribute type and property to get (will return first instance if there are multiple attributes of the same type)</param>
        /// <param name="inherit">true to search this member's inheritance chain to find the attributes; otherwise, false. This parameter is ignored for properties and events</param>
        /// </summary>    
        public static TValue GetAttribute<TAttribute, TValue>(this Type type, string MemberName, Func<TAttribute, TValue> valueSelector, bool inherit = false) where TAttribute : Attribute
        {
            var att = type.GetMember(MemberName).FirstOrDefault().GetCustomAttributes(typeof(TAttribute), inherit).FirstOrDefault() as TAttribute;
            if (att != null)
            {
                return valueSelector(att);
            }
            return default(TValue);
        }
    }

```

Usage

```cs
//Read System.ComponentModel Description Attribute from method 'MyMethodName' in class 'MyClass'
var Attribute = typeof(MyClass).GetAttribute("MyMethodName", (DescriptionAttribute d) => d.Description);

```



##  Create an instance of a Generic Type and invoke it's method


```cs
var baseType = typeof(List<>);
var genericType = baseType.MakeGenericType(typeof(String));
var instance = Activator.CreateInstance(genericType);
var method = genericType.GetMethod("GetHashCode");
var result = method.Invoke(instance, new object[] { });

```



## Instantiating classes that implement an interface (e.g. plugin activation)


If you want your application to support a plug-in system, for example to load plug-ins from assemblies located in `plugins` folder:

```cs
interface IPlugin
{
    string PluginDescription { get; }
    void DoWork();
}

```

This class would be located in a separate dll

```cs
class HelloPlugin : IPlugin
{
    public string PluginDescription => "A plugin that says Hello";
    public void DoWork()
    {
        Console.WriteLine("Hello");
    }
}

```

Your application's plugin loader would find the dll files, get all types in those assemblies that implement `IPlugin`, and create instances of those.

```

   public IEnumerable<IPlugin> InstantiatePlugins(string directory)
    {
        var pluginAssemblyNames = Directory.GetFiles(directory, "*.addin.dll").Select(name => new FileInfo(name).FullName).ToArray();
        //load the assemblies into the current AppDomain, so we can instantiate the types later
        foreach (var fileName in pluginAssemblyNames)
            AppDomain.CurrentDomain.Load(File.ReadAllBytes(fileName));
        var assemblies = pluginAssemblyNames.Select(System.Reflection.Assembly.LoadFile);
        var typesInAssembly = assemblies.SelectMany(asm => asm.GetTypes());
        var pluginTypes = typesInAssembly.Where(type => typeof (IPlugin).IsAssignableFrom(type));
        return pluginTypes.Select(Activator.CreateInstance).Cast<IPlugin>(); 
    }

```



## Determining generic arguments of instances of generic types


If you have an instance of a generic type but for some reason don't know the specific type, you might want to determine the generic arguments that were used to create this instance.

Let's say someone created an instance of `List<T>` like that and passes it to a method:

```cs
var myList = new List<int>();
ShowGenericArguments(myList);

```

where `ShowGenericArguments` has this signature:

```cs
public void ShowGenericArguments(object o)

```

so at compile time you don't have any idea what generic arguments have been used to create `o`. [Reflection](https://msdn.microsoft.com/en-us/library/system.type(v=vs.110).aspx) provides a lot of methods to inspect generic types. At first, we can determine if the type of `o` is a generic type at all:

```cs
public void ShowGenericArguments(object o)
{
    if (o == null) return;

    Type t = o.GetType();
    if (!t.IsGenericType) return;
    ...

```

[`Type.IsGenericType`](https://msdn.microsoft.com/en-us/library/system.type.isgenerictype(v=vs.110).aspx) returns `true` if the type is a generic type and `false` if not.

But this is not all we want to know. `List<>` itself is a generic type, too. But we only want to examine instances of specific **constructed generic** types. A constructed generic type is for example a `List<int>` that has a specific type **argument** for all its generic **parameters**.

The `Type` class provides two more properties, [`IsConstructedGenericType`](https://msdn.microsoft.com/en-us/library/system.type.isconstructedgenerictype(v=vs.110).aspx) and [`IsGenericTypeDefinition`](https://msdn.microsoft.com/en-us/library/system.type.isgenerictypedefinition(v=vs.110).aspx), to distinguish these constructed generic types from generic type definitions:

```cs
typeof(List<>).IsGenericType // true
typeof(List<>).IsGenericTypeDefinition // true
typeof(List<>).IsConstructedGenericType// false

typeof(List<int>).IsGenericType // true
typeof(List<int>).IsGenericTypeDefinition // false
typeof(List<int>).IsConstructedGenericType// true

```

To enumerate the generic arguments of an instance, we can use the [`GetGenericArguments()`](https://msdn.microsoft.com/en-us/library/system.type.getgenericarguments(v=vs.110).aspx) method that returns an `Type` array containing the generic type arguments:

```cs
public void ShowGenericArguments(object o)
{
    if (o == null) return;   
    Type t = o.GetType();
    if (!t.IsConstructedGenericType) return;

    foreach(Type genericTypeArgument in t.GetGenericArguments())
        Console.WriteLine(genericTypeArgument.Name);
}

```

So the call from above (`ShowGenericArguments(myList)`) results in this output:

```cs
Int32

```



## Get a Type by name with namespace


To do this you need a reference to the assembly which contains the type. If you have another type available which you know is in the same assembly as the one you want you can do this:

```cs
typeof(KnownType).Assembly.GetType(typeName);

```


<li>where `typeName` is the name of the type you are looking for (including the namespace)
, and `KnownType` is the type you know is in the same assembly.</li>

Less efficient but more general is as follows:

```cs
Type t = null;
foreach (Assembly ass in AppDomain.CurrentDomain.GetAssemblies())
{
    if (ass.FullName.StartsWith("System."))
        continue;
    t = ass.GetType(typeName);
    if (t != null)
        break;
}

```

Notice the check to exclude scanning System namespace assemblies to speed up the search.  If your type may actually be a CLR type, you will have to delete these two lines.

If you happen to have the fully assembly-qualified type name including the assembly you can simply get it with

```cs
Type.GetType(fullyQualifiedName);

```



## Looping through all the properties of a class


```cs
Type type = obj.GetType();
//To restrict return properties. If all properties are required don't provide flag.
BindingFlags flags = BindingFlags.Public | BindingFlags.Instance; 
PropertyInfo[] properties = type.GetProperties(flags);

foreach (PropertyInfo property in properties)
{
    Console.WriteLine("Name: " + property.Name + ", Value: " + property.GetValue(obj, null));
}

```



#### Remarks


[Reflection](https://msdn.microsoft.com/en-us/library/mt656691.aspx) allows code to access information about the assemblies, modules and types at run-time (program execution). This can then be further used to dynamically create, modify or access types. Types include properties, methods, fields and attributes.

Further Reading :

[Reflection(C#)](https://msdn.microsoft.com/en-us/library/mt656691.aspx)

[Reflection in .Net Framework](https://msdn.microsoft.com/en-us/library/f7ykdhsy%28v=vs.110%29.aspx)

