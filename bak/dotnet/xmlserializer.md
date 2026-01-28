---
metaTitle: ".NET Framework - XmlSerializer"
description: "Formatting: Custom DateTime format, Serialize object, Deserialize object, Behaviour: Map array name to property (XmlArray), Behaviour: Map Element name to Property, Efficiently building multiple serializers with derived types specified dynamically"
---

# XmlSerializer



## Formatting: Custom DateTime format


```dotnet
public class Dog
{
    private const string _birthStringFormat = "yyyy-MM-dd";

    [XmlIgnore]
    public DateTime Birth {get; set;}

    [XmlElement(ElementName="Birth")]
    public string BirthString
    {
        get { return Birth.ToString(_birthStringFormat); }
        set { Birth = DateTime.ParseExact(value, _birthStringFormat, CultureInfo.InvariantCulture); }
    }
}

```



## Serialize object


```dotnet
public void SerializeFoo(string fileName, Foo foo)
{
    var serializer = new XmlSerializer(typeof(Foo));
    using (var stream = File.Open(fileName, FileMode.Create))
    {
        serializer.Serialize(stream, foo);
    }
}

```



## Deserialize object


```dotnet
public Foo DeserializeFoo(string fileName)
{
    var serializer = new XmlSerializer(typeof(Foo));
    using (var stream = File.OpenRead(fileName))
    {
        return (Foo)serializer.Deserialize(stream);
    }
}

```



## Behaviour: Map array name to property (XmlArray)


```dotnet
<Store>
    <Articles>
        <Product/>
        <Product/>
    </Articles>
</Store>

```


- 

```dotnet
public class Store
{
    [XmlArray("Articles")]
    public List<Product> Products {get; set; }
}

```



## Behaviour: Map Element name to Property


```dotnet
<Foo>
    <Dog/>
</Foo>

```


- 

```dotnet
public class Foo
{
    // Using XmlElement
    [XmlElement(Name="Dog")]
    public Animal Cat { get; set; }
}

```



## Efficiently building multiple serializers with derived types specified dynamically


### Where we came from

Sometimes we can't provide all of the required metadata needed for the XmlSerializer framework in attribute. Suppose we have a base class of serialized objects, and some of the derived classes are unknown to the base class. We can't place an attribute for all of the classes which are not know at the design time of the base type. We could have another team developing some of the derived classes.

### What can we do

We can use `new XmlSerializer(type, knownTypes)`, but that would be a O(N^2) operation for N serializers, at least to discover all of the types supplied in arguments:

```dotnet
// Beware of the N^2 in terms of the number of types.
var allSerializers = allTypes.Select(t => new XmlSerializer(t, allTypes));
var serializerDictionary = Enumerable.Range(0, allTypes.Length)
    .ToDictionary (i => allTypes[i], i => allSerializers[i])

```

In this example, the the Base type is not aware of it's derived types, which is normal in OOP.

### Doing it efficiently

Luckily, there is a method which addresses this particular problem - supplying known types for multiple serializers efficiently:

[System.Xml.Serialization.XmlSerializer.FromTypes Method (Type[])](https://msdn.microsoft.com/en-us/library/system.xml.serialization.xmlserializer.fromtypes(v=vs.110).aspx#Anchor_1)

> 
The FromTypes method allows you to efficiently create an array of XmlSerializer objects for processing an array of Type objects.


```dotnet
var allSerializers = XmlSerializer.FromTypes(allTypes);
var serializerDictionary = Enumerable.Range(0, allTypes.Length)
    .ToDictionary(i => allTypes[i], i => allSerializers[i]);

```

Here is a complete code sample:

```dotnet
using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using System.Linq;
using System.Linq;
                    
public class Program
{
    public class Container
    {
        public Base Base { get; set; }
    }
    
    public class Base
    {
        public int JustSomePropInBase { get; set; }
    }
    
    public class Derived : Base
    {
        public int JustSomePropInDerived { get; set; }
    }
    
    public void Main()
    {
        var sampleObject = new Container { Base = new Derived() };
        var allTypes = new[] { typeof(Container), typeof(Base), typeof(Derived) };
        
        Console.WriteLine("Trying to serialize without a derived class metadata:");
        SetupSerializers(allTypes.Except(new[] { typeof(Derived) }).ToArray());
        try
        {
            Serialize(sampleObject);
        }
        catch (InvalidOperationException e)
        {
            Console.WriteLine();
            Console.WriteLine("This error was anticipated,");
            Console.WriteLine("we have not supplied a derived class.");
            Console.WriteLine(e);
        }
        Console.WriteLine("Now trying to serialize with all of the type information:");
        SetupSerializers(allTypes);
        Serialize(sampleObject);
        Console.WriteLine();
        Console.WriteLine("Slides down well this time!");
    }

    static void Serialize<T>(T o)
    {
        serializerDictionary[typeof(T)].Serialize(Console.Out, o);
    }

    private static Dictionary<Type, XmlSerializer> serializerDictionary;
    
    static void SetupSerializers(Type[] allTypes)
    {
        var allSerializers = XmlSerializer.FromTypes(allTypes);
        serializerDictionary = Enumerable.Range(0, allTypes.Length)
            .ToDictionary(i => allTypes[i], i => allSerializers[i]);
    }
}

```

Output:

```dotnet
Trying to serialize without a derived class metadata:
<?xml version="1.0" encoding="utf-16"?>
<Container xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
This error was anticipated,
we have not supplied a derived class.
System.InvalidOperationException: There was an error generating the XML document. ---> System.InvalidOperationException: The type Program+Derived was not expected. Use the XmlInclude or SoapInclude attribute to specify types that are not known statically.
   at Microsoft.Xml.Serialization.GeneratedAssembly.XmlSerializationWriter1.Write2_Base(String n, String ns, Base o, Boolean isNullable, Boolean needType)
   at Microsoft.Xml.Serialization.GeneratedAssembly.XmlSerializationWriter1.Write3_Container(String n, String ns, Container o, Boolean isNullable, Boolean needType)
   at Microsoft.Xml.Serialization.GeneratedAssembly.XmlSerializationWriter1.Write4_Container(Object o)
   at System.Xml.Serialization.XmlSerializer.Serialize(XmlWriter xmlWriter, Object o, XmlSerializerNamespaces namespaces, String encodingStyle, String id)
   --- End of inner exception stack trace ---
   at System.Xml.Serialization.XmlSerializer.Serialize(XmlWriter xmlWriter, Object o, XmlSerializerNamespaces namespaces, String encodingStyle, String id)
   at System.Xml.Serialization.XmlSerializer.Serialize(XmlWriter xmlWriter, Object o, XmlSerializerNamespaces namespaces, String encodingStyle)
   at System.Xml.Serialization.XmlSerializer.Serialize(XmlWriter xmlWriter, Object o, XmlSerializerNamespaces namespaces)
   at Program.Serialize[T](T o)
   at Program.Main()
Now trying to serialize with all of the type information:
<?xml version="1.0" encoding="utf-16"?>
<Container xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <Base xsi:type="Derived">
    <JustSomePropInBase>0</JustSomePropInBase>
    <JustSomePropInDerived>0</JustSomePropInDerived>
  </Base>
</Container>
Slides down well this time!

```

### What's in the output

This error message recommends what we tried to avoid (or what we can not do in some scenarios) - referencing derived types from base class:

`Use the XmlInclude or SoapInclude attribute to specify types that are not known statically.`

This is how we get our derived class in the XML:

`<Base xsi:type="Derived">`

`Base` corresponds to the property type declared in the `Container` type, and `Derived` being the type of the instance actually supplied.

Here is a working [example fiddle](https://dotnetfiddle.net/hufepI)



#### Remarks


Do not use the `XmlSerializer` to parse `HTML`. For this, special libraries are available like the [HTML Agility Pack](https://htmlagilitypack.codeplex.com)

