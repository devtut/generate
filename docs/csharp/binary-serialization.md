---
metaTitle: "Binary Serialization"
description: "Serialization Binder, Controlling serialization behavior with attributes, Some gotchas in backward compatibility, Making an object serializable, Serialization surrogates (Implementing ISerializationSurrogate), Adding more control by implementing ISerializable"
---

# Binary Serialization



## Serialization Binder


The binder gives you an opportunity to inspect what types are being loaded in your application domain

Create a class inherited from SerializationBinder

```cs
class MyBinder : SerializationBinder
{
    public override Type BindToType(string assemblyName, string typeName)
    {
        if (typeName.Equals("BinarySerializationExample.Item"))
            return typeof(Item);
        return null;
    }
}

```

Now we can check what types are loading and on this basis to decide what we really want to receive

For using a binder, you must add it to the BinaryFormatter.

```cs
object DeserializeData(byte[] bytes)
{
    var binaryFormatter = new BinaryFormatter();
    binaryFormatter.Binder = new MyBinder();

    using (var memoryStream = new MemoryStream(bytes))
        return binaryFormatter.Deserialize(memoryStream);
}

```

The complete solution

```cs
using System;
using System.IO;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;

namespace BinarySerializationExample
{
    class MyBinder : SerializationBinder
    {
        public override Type BindToType(string assemblyName, string typeName)
        {
            if (typeName.Equals("BinarySerializationExample.Item"))
                return typeof(Item);
            return null;
        }
    }

    [Serializable]
    public class Item
    {
        private string _name;

        public string Name
        {
            get { return _name; }
            set { _name = value; }
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            var item = new Item
            {
                Name = "Orange"
            };

            var bytes = SerializeData(item);    
            var deserializedData = (Item)DeserializeData(bytes);
        }

        private static byte[] SerializeData(object obj)
        {
            var binaryFormatter = new BinaryFormatter();
            using (var memoryStream = new MemoryStream())
            {
                binaryFormatter.Serialize(memoryStream, obj);
                return memoryStream.ToArray();
            }
        }

        private static object DeserializeData(byte[] bytes)
        {
            var binaryFormatter = new BinaryFormatter
            {
                Binder = new MyBinder()
            };

            using (var memoryStream = new MemoryStream(bytes))
                return binaryFormatter.Deserialize(memoryStream);
        }
    }
}

```



## Controlling serialization behavior with attributes


If you use the `[NonSerialized]` attribute, then that member will always have its default value after deserialization (ex. 0 for an `int`, null for `string`, false for a `bool`, etc.), regardless of any initialization done in the object itself (constructors, declarations, etc.).  To compensate, the attributes `[OnDeserializing]` (called just BEFORE deserializing) and `[OnDeserialized]` (called just AFTER deserializing) together with their counterparts, `[OnSerializing]` and `[OnSerialized]` are provided.

Assume we want to add a "Rating" to our Vector and we want to make sure the value always starts at 1.  The way it is written below, it will be 0 after being deserialized:

```cs
[Serializable]
public class Vector
{
    public int X;
    public int Y;
    public int Z;

    [NonSerialized]
    public decimal Rating = 1M;

    public Vector()
    {
        Rating = 1M;
    }

    public Vector(decimal initialRating)
    {
        Rating = initialRating;
    }
}

```

To fix this problem, we can simply add the following method inside of the class to set it to 1:

```cs
[OnDeserializing]
void OnDeserializing(StreamingContext context)
{
    Rating = 1M;
}

```

Or, if we want to set it to a calculated value, we can wait for it to be finished deserializing and then set it:

```cs
[OnDeserialized]
void OnDeserialized(StreamingContext context)
{
    Rating = 1 + ((X+Y+Z)/3);
}

```

Similarly, we can control how things are written out by using `[OnSerializing]` and `[OnSerialized]`.



## Some gotchas in backward compatibility


This small example shows how you can lose backward compatibility in your programs if you do not take care in advance about this. And ways to get more control of serialization process

At first, we will write an example of the first version of the program:

Version 1

```cs
[Serializable]
class Data
{
    [OptionalField]
    private int _version;
    
    public int Version
    {
        get { return _version; }
        set { _version = value; }
    }
}

```

And now, let us assume that in the second version of the program added a new class. And we need to store it in an array.

Now code will look like this:

Version 2

```cs
[Serializable]
class NewItem
{
    [OptionalField]
    private string _name;

    public string Name
    {
        get { return _name; }
        set { _name = value; }
    }
}

[Serializable]
class Data
{
    [OptionalField]
    private int _version;

    public int Version
    {
        get { return _version; }
        set { _version = value; }
    }

    [OptionalField]
    private List<NewItem> _newItems;

    public List<NewItem> NewItems
    {
        get { return _newItems; }
        set { _newItems = value; }
    }
}

```

And code for serialize and deserialize

```cs
private static byte[] SerializeData(object obj)
{
    var binaryFormatter = new BinaryFormatter();
    using (var memoryStream = new MemoryStream())
    {
        binaryFormatter.Serialize(memoryStream, obj);
        return memoryStream.ToArray();
    }
}

private static object DeserializeData(byte[] bytes)
{
    var binaryFormatter = new BinaryFormatter();
    using (var memoryStream = new MemoryStream(bytes))
        return binaryFormatter.Deserialize(memoryStream);
}

```

And so, what would happen when you serialize the data in the program of v2 and will try to deserialize them in the program of v1?

You get an exception:

```cs
System.Runtime.Serialization.SerializationException was unhandled
Message=The ObjectManager found an invalid number of fixups. This usually indicates a problem in the Formatter.Source=mscorlib
StackTrace:
   at System.Runtime.Serialization.ObjectManager.DoFixups()
   at System.Runtime.Serialization.Formatters.Binary.ObjectReader.Deserialize(HeaderHandler handler, __BinaryParser serParser, Boolean fCheck, Boolean isCrossAppDomain, IMethodCallMessage methodCallMessage)
   at System.Runtime.Serialization.Formatters.Binary.BinaryFormatter.Deserialize(Stream serializationStream, HeaderHandler handler, Boolean fCheck, Boolean isCrossAppDomain, IMethodCallMessage methodCallMessage)
   at System.Runtime.Serialization.Formatters.Binary.BinaryFormatter.Deserialize(Stream serializationStream)
   at Microsoft.Samples.TestV1.Main(String[] args) in c:\Users\andrew\Documents\Visual Studio 2013\Projects\vts\CS\V1 Application\TestV1Part2\TestV1Part2.cs:line 29
   at System.AppDomain._nExecuteAssembly(Assembly assembly, String[] args)
   at Microsoft.VisualStudio.HostingProcess.HostProc.RunUsersAssembly()
   at System.Threading.ExecutionContext.Run(ExecutionContext executionContext, ContextCallback callback, Object state)
   at System.Threading.ThreadHelper.ThreadStart()

```

Why?

The ObjectManager has a different logic to resolve dependencies for arrays and for reference and value types. We added an array of new the reference type which is absent in our assembly.

When ObjectManager attempts to resolve dependencies it builds the graph. When it sees the array, it can not fix it immediately, so that it creates a dummy reference and then fixes the array later.

And since this type is not in the assembly and dependencies can’t be fixed. For some reason, it does not remove the array from the list of elements for the fixes and at the end, it throws an exception “IncorrectNumberOfFixups”.

It is some ‘gotchas’ in the process of serialization. For some reason, it does not work correctly only for arrays of new reference types.

```cs
A Note:
Similar code will work correctly if you do not use arrays with new classes

```

And the first way to fix it and maintain compatibility?

<li>Use a collection of new structures rather than classes or use a
dictionary(possible classes), because a dictionary it’s a collection
of keyvaluepair(it’s structure)</li>
- Use ISerializable, if you can't change the old code



## Making an object serializable


Add the `[Serializable]` attribute to mark an entire object for binary serialization:

```cs
[Serializable]
public class Vector
{
    public int X;
    public int Y;
    public int Z;

    [NonSerialized]
    public decimal DontSerializeThis;

    [OptionalField]
    public string Name;
}

```

All members will be serialized unless we explicitly opt-out using the `[NonSerialized]` attribute.  In our example, `X`, `Y`, `Z`, and `Name` are all serialized.

All members are required to be present on deserialization unless marked with `[NonSerialized]` or `[OptionalField]`.  In our example, `X`, `Y`, and `Z` are all required and deserialization will fail if they are not present in the stream.  `DontSerializeThis` will always be set to `default(decimal)` (which is 0).  If `Name` is present in the stream, then it will be set to that value, otherwise it will be set to `default(string)` (which is null).  The purpose of `[OptionalField]` is to provide a bit of version tolerance.



## Serialization surrogates (Implementing ISerializationSurrogate)


Implements a serialization surrogate selector that allows one object to perform serialization and deserialization of another

As well allows to properly serialize or deserialize a class that is not itself serializable

Implement ISerializationSurrogate interface

```cs
public class ItemSurrogate : ISerializationSurrogate
{
    public void GetObjectData(object obj, SerializationInfo info, StreamingContext context)
    {
        var item = (Item)obj;
        info.AddValue("_name", item.Name);
    }

    public object SetObjectData(object obj, SerializationInfo info, StreamingContext context, ISurrogateSelector selector)
    {
        var item = (Item)obj;
        item.Name = (string)info.GetValue("_name", typeof(string));
        return item;
    }
}

```

Then you need to let your IFormatter know about the surrogates by defining and initializing a SurrogateSelector and assigning it to your IFormatter

```cs
var surrogateSelector = new SurrogateSelector();
surrogateSelector.AddSurrogate(typeof(Item), new StreamingContext(StreamingContextStates.All), new ItemSurrogate());    
var binaryFormatter = new BinaryFormatter
{
    SurrogateSelector = surrogateSelector
};

```

Even if the class is not marked serializable.

```cs
//this class is not serializable
public class Item
{
    private string _name;

    public string Name
    {
        get { return _name; }
        set { _name = value; }
    }
}

```

The complete solution

```cs
using System;
using System.IO;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;

namespace BinarySerializationExample
{
    class Item
    {
        private string _name;

        public string Name
        {
            get { return _name; }
            set { _name = value; }
        }
    }

    class ItemSurrogate : ISerializationSurrogate
    {
        public void GetObjectData(object obj, SerializationInfo info, StreamingContext context)
        {
            var item = (Item)obj;
            info.AddValue("_name", item.Name);
        }

        public object SetObjectData(object obj, SerializationInfo info, StreamingContext context, ISurrogateSelector selector)
        {
            var item = (Item)obj;
            item.Name = (string)info.GetValue("_name", typeof(string));
            return item;
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            var item = new Item
            {
                Name = "Orange"
            };

            var bytes = SerializeData(item);
            var deserializedData = (Item)DeserializeData(bytes);
        }

        private static byte[] SerializeData(object obj)
        {
            var surrogateSelector = new SurrogateSelector();
            surrogateSelector.AddSurrogate(typeof(Item), new StreamingContext(StreamingContextStates.All), new ItemSurrogate());

            var binaryFormatter = new BinaryFormatter
            {
                SurrogateSelector = surrogateSelector
            };

            using (var memoryStream = new MemoryStream())
            {
                binaryFormatter.Serialize(memoryStream, obj);
                return memoryStream.ToArray();
            }
        }

        private static object DeserializeData(byte[] bytes)
        {
            var surrogateSelector = new SurrogateSelector();
            surrogateSelector.AddSurrogate(typeof(Item), new StreamingContext(StreamingContextStates.All), new ItemSurrogate());

            var binaryFormatter = new BinaryFormatter
            {
                SurrogateSelector = surrogateSelector
            };

            using (var memoryStream = new MemoryStream(bytes))
                return binaryFormatter.Deserialize(memoryStream);
        }
    }
}

```



## Adding more control by implementing ISerializable


That would get more control over serialization, how to save and load types

Implement ISerializable interface and create an empty constructor to compile

```cs
[Serializable]
public class Item : ISerializable
{
    private string _name;

    public string Name
    {
        get { return _name; }
        set { _name = value; }
    }

    public Item ()
    {

    }

    protected Item (SerializationInfo info, StreamingContext context)
    {
        _name = (string)info.GetValue("_name", typeof(string));
    }

    public void GetObjectData(SerializationInfo info, StreamingContext context)
    {
        info.AddValue("_name", _name, typeof(string));
    }
}

```

For data serialization, you can specify the desired name and the desired type

```cs
info.AddValue("_name", _name, typeof(string));

```

When the data is deserialized, you will be able to read the desired type

```cs
_name = (string)info.GetValue("_name", typeof(string));

```



#### Remarks


The binary serialization engine is part of the .NET framework, but the examples given here are specific to C#.  As compared to other serialization engines built into the .NET framework, the binary serializer is fast and efficient and usually requires very little extra code to get it to work.  However, it is also less tolerant to code changes; that is, if you serialize an object and then make a slight change to the object's definition, it likely will not deserialize correctly.

