---
metaTitle: ".NET Framework - JSON Serialization"
description: "Deserialization using System.Web.Script.Serialization.JavaScriptSerializer, Deserialization using Json.NET, Serialization using Json.NET, Serialization-Deserialization using Newtonsoft.Json, Dynamic binding, Serialization using Json.NET with JsonSerializerSettings"
---

# JSON Serialization




## Deserialization using System.Web.Script.Serialization.JavaScriptSerializer


The `JavaScriptSerializer.Deserialize<T>(input)` method attempts to deserialize a string of valid JSON into an object of the specified type `<T>`, using the default mappings natively supported by `JavaScriptSerializer`.

```dotnet
using System.Collections;
using System.Web.Script.Serialization;

// ...

string rawJSON = "{\"Name\":\"Fibonacci Sequence\",\"Numbers\":[0, 1, 1, 2, 3, 5, 8, 13]}";

JavaScriptSerializer JSS = new JavaScriptSerializer(); 
Dictionary<string, object> parsedObj = JSS.Deserialize<Dictionary<string, object>>(rawJSON);

string name = parsedObj["Name"].toString();
ArrayList numbers = (ArrayList)parsedObj["Numbers"]

```

Note: The `JavaScriptSerializer` object was introduced in .NET version 3.5



## Deserialization using Json.NET


```dotnet
internal class Sequence{
    public string Name;
    public List<int> Numbers;
}    

// ...

string rawJSON = "{\"Name\":\"Fibonacci Sequence\",\"Numbers\":[0, 1, 1, 2, 3, 5, 8, 13]}";

Sequence sequence = JsonConvert.DeserializeObject<Sequence>(rawJSON);

```

For more information, refer to the [Json.NET official site](http://www.newtonsoft.com/json).

Note: Json.NET supports .NET version 2 and higher.



## Serialization using Json.NET


```dotnet
[JsonObject("person")]
public class Person
{
    [JsonProperty("name")]
    public string PersonName { get; set; }
    [JsonProperty("age")]
    public int PersonAge { get; set; }
    [JsonIgnore]
    public string Address { get; set; }
}

Person person = new Person { PersonName = "Andrius", PersonAge = 99, Address = "Some address" };
string rawJson = JsonConvert.SerializeObject(person);

Console.WriteLine(rawJson); // {"name":"Andrius","age":99}

```

Notice how properties (and classes) can be decorated with attributes to change their appearance in resulting json string or to remove them from json string at all (JsonIgnore).

More information about Json.NET serialization attributes can be found [here](http://www.newtonsoft.com/json/help/html/serializationattributes.htm).

In C#, public identifiers are written in **PascalCase** by convention. In JSON, the convention is to use **camelCase** for all names. You can use a contract resolver to convert between the two.

```dotnet
using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;

public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
    [JsonIgnore]
    public string Address { get; set; }
}

public void ToJson() {
    Person person = new Person { Name = "Andrius", Age = 99, Address = "Some address" };
    var resolver = new CamelCasePropertyNamesContractResolver();
    var settings = new JsonSerializerSettings { ContractResolver = resolver };
    string json = JsonConvert.SerializeObject(person, settings);

    Console.WriteLine(json); // {"name":"Andrius","age":99}
}

```



## Serialization-Deserialization using Newtonsoft.Json


Unlike the other helpers, this one uses static class helpers to serialize and deserialize, hence it is a little bit easier than the others to use.

```dotnet
using Newtonsoft.Json;

var rawJSON      = "{\"Name\":\"Fibonacci Sequence\",\"Numbers\":[0, 1, 1, 2, 3, 5, 8, 13]}";
var fibo         = JsonConvert.DeserializeObject<Dictionary<string, object>>(rawJSON);
var rawJSON2     = JsonConvert.SerializeObject(fibo);

```



## Dynamic binding


Newtonsoft's Json.NET allows you to bind json dynamically (using ExpandoObject / Dynamic objects) without the need to create the type explicitly.

**Serialization**

```dotnet
dynamic jsonObject = new ExpandoObject();
jsonObject.Title   = "Merchent of Venice";
jsonObject.Author  = "William Shakespeare";
Console.WriteLine(JsonConvert.SerializeObject(jsonObject));

```

**De-serialization**

```dotnet
var rawJson = "{\"Name\":\"Fibonacci Sequence\",\"Numbers\":[0, 1, 1, 2, 3, 5, 8, 13]}";
dynamic parsedJson = JObject.Parse(rawJson);
Console.WriteLine("Name: " + parsedJson.Name);
Console.WriteLine("Name: " + parsedJson.Numbers.Length);

```

Notice that the keys in the rawJson object have been turned into member variables in the dynamic object.

This is useful in cases where an application can accept/ produce varying formats of JSON. It is however suggested to use an extra level of validation for the Json string or to the dynamic object generated as a result of serialization/ de-serialization.



## Serialization using Json.NET with JsonSerializerSettings


This serializer has some nice features that the default .net json serializer doesn't have, like Null value handling, you just need to create the `JsonSerializerSettings` :

```dotnet
public static string Serialize(T obj)
{
   string result = JsonConvert.SerializeObject(obj, new JsonSerializerSettings { NullValueHandling = NullValueHandling.Ignore});
   return result;
}

```

Another serious serializer issue in .net is the self referencing loop. In the case of a student that is enrolled in a course, its instance has a course property and a course has a collection of students that means a `List<Student>` which will create a reference loop. You can handle this with `JsonSerializerSettings` :

```dotnet
public static string Serialize(T obj)
{
   string result = JsonConvert.SerializeObject(obj, new JsonSerializerSettings { ReferenceLoopHandling = ReferenceLoopHandling.Ignore});
   return result;
}

```

You can put various serializations option like this:

```dotnet
public static string Serialize(T obj)
{
   string result = JsonConvert.SerializeObject(obj, new JsonSerializerSettings { NullValueHandling = NullValueHandling.Ignore, ReferenceLoopHandling = ReferenceLoopHandling.Ignore});
   return result;
}

```



#### Remarks


**JavaScriptSerializer vs Json.NET**

The [`JavaScriptSerializer` class](https://msdn.microsoft.com/en-us/library/system.web.script.serialization.javascriptserializer(v=vs.110).aspx) was introducted in .NET 3.5 and is used internally by .NET's asynchronous communication layer for AJAX-enabled applications. It can be used to work with JSON in managed code.

Despite the existence of the `JavaScriptSerializer` class, Microsoft recommends using the open source [Json.NET library](http://www.newtonsoft.com/json) for serialization and deserialization. Json.NET offers better performance and a friendlier interface for mapping JSON to custom classes (a custom [`JavaScriptConverter` object](https://msdn.microsoft.com/en-us/library/system.web.script.serialization.javascriptconverter(v=vs.110).aspx) would be needed to accomplish the same with  `JavaScriptSerializer`).

