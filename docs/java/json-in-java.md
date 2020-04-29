---
metaTitle: "JSON in Java"
description: "Using Jackson Object Mapper, JSON To Object  (Gson Library), JSONObject.NULL, JSON Builder - chaining methods, Object To JSON (Gson Library), JSON Iteration, optXXX vs getXXX methods, Encoding data as JSON, Decoding JSON data, Extract single element from JSON, JsonArray to Java List (Gson Library), Deserialize JSON collection to collection of Objects using Jackson"
---

# JSON in Java


JSON (JavaScript Object Notation) is a lightweight, text-based, language-independent data exchange format that is easy for humans and machines to read and write. JSON can represent two structured types: objects and arrays. JSON is often used in Ajax applications, configurations, databases, and RESTful web services.
[The Java API for JSON Processing](http://www.oracle.com/technetwork/articles/java/json-1973242.html) provides portable APIs to parse, generate, transform, and query JSON.



## Using Jackson Object Mapper


Pojo Model

```java
public class Model {
    private String firstName;
    private String lastName;
    private int age;
    /* Getters and setters not shown for brevity */        
}

```

Example: String to Object

```java
Model outputObject = objectMapper.readValue(
     "{\"firstName\":\"John\",\"lastName\":\"Doe\",\"age\":23}",
     Model.class);
System.out.println(outputObject.getFirstName());
//result: John

```

Example: Object to String

```java
String jsonString = objectMapper.writeValueAsString(inputObject));
//result: {"firstName":"John","lastName":"Doe","age":23}

```

### Details

Import statement needed:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

```

[Maven dependency: jackson-databind](https://mvnrepository.com/artifact/com.fasterxml.jackson.core/jackson-databind)

### `ObjectMapper` instance

```java
//creating one
ObjectMapper objectMapper = new ObjectMapper();

```


- `ObjectMapper` is threadsafe
- recommended: have a shared, static instance

### Deserialization:

```java
<T> T readValue(String content, Class<T> valueType)  

```


- `valueType` needs to be specified -- the return will be of this type
<li>Throws
<ul>
- `IOException` - in case of a low-level I/O problem
- `JsonParseException` - if underlying input contains invalid content
- `JsonMappingException` - if the input JSON structure does not match object structure

Usage example (jsonString is the input string):

```java
Model fromJson = objectMapper.readValue(jsonString, Model.class);

```

### Method for serialization:

String writeValueAsString(Object value)

<li>Throws
<ul>
- `JsonProcessingException` in case of an error
- Note: prior to version 2.1, throws clause included IOException; 2.1 removed it.



## JSON To Object  (Gson Library)


Lets assume you have a class called `Person` with just `name`

```java
private class Person {
    public String name;

    public Person(String name) {
        this.name = name;
    }
}

```

**Code:**

```java
Gson gson = new Gson();
String json = "{\"name\": \"John\"}";

Person person = gson.fromJson(json, Person.class);
System.out.println(person.name); //John

```

You must have [gson library](https://mvnrepository.com/artifact/com.google.code.gson/gson) in your classpath.



## JSONObject.NULL


If you need to add a property with a `null` value, you should use the predefined static final `JSONObject.NULL` and not the standard Java `null` reference.

`JSONObject.NULL` is a sentinel value used to explicitly define a property with an empty value.

```java
JSONObject obj = new JSONObject();
obj.put("some", JSONObject.NULL);   //Creates: {"some":null}
System.out.println(obj.get("some"));//prints: null

```

**Note**

```java
JSONObject.NULL.equals(null); //returns true

```

Which is a **clear violation** of [`Java.equals()`](https://docs.oracle.com/javase/7/docs/api/java/lang/Object.html#equals(java.lang.Object)) contract:

> 
For any non-null reference value x, x.equals(null) should return false




## JSON Builder - chaining methods


You can use [method chaining](https://en.wikipedia.org/wiki/Method_chaining) while working with `JSONObject` and `JSONArray`.

**JSONObject example**

```java
JSONObject obj = new JSONObject();//Initialize an empty JSON object 
//Before: {}
obj.put("name","Nikita").put("age","30").put("isMarried","true");
//After: {"name":"Nikita","age":30,"isMarried":true}

```

**JSONArray**

```java
JSONArray arr = new JSONArray();//Initialize an empty array
//Before: []
arr.put("Stack").put("Over").put("Flow");
//After: ["Stack","Over","Flow"]

```



## Object To JSON (Gson Library)


Lets assume you have a class called `Person` with just `name`

```java
private class Person {
    public String name;

    public Person(String name) {
        this.name = name;
    }
}

```

**Code:**

```java
Gson g = new Gson();

Person person = new Person("John");
System.out.println(g.toJson(person)); // {"name":"John"}

```

Of course the [Gson](http://central.maven.org/maven2/com/google/code/gson/gson/2.3.1/gson-2.3.1.jar) jar must be on the classpath.



## JSON Iteration


**Iterate over `JSONObject` properties**

```java
JSONObject obj = new JSONObject("{\"isMarried\":\"true\", \"name\":\"Nikita\", \"age\":\"30\"}");
Iterator<String> keys = obj.keys();//all keys: isMarried, name & age
while (keys.hasNext()) {                      //as long as there is another key
      String key = keys.next();               //get next key 
      Object value = obj.get(key);            //get next value by key
      System.out.println(key + " : " + value);//print key : value
}

```

**Iterate over `JSONArray` values**

```java
JSONArray arr = new JSONArray();        //Initialize an empty array
//push (append) some values in:
arr.put("Stack");
arr.put("Over");
arr.put("Flow");
for (int i = 0; i < arr.length(); i++) {//iterate over all values
    Object value = arr.get(i);          //get value
    System.out.println(value);          //print each value
}

```



## optXXX vs getXXX methods


`JSONObject` and `JSONArray` have a few methods that are very useful while dealing with a possibility that a value your are trying to get does not exist or is of another type.

```java
JSONObject obj = new JSONObject();
obj.putString("foo", "bar");

// For existing properties of the correct type, there is no difference
obj.getString("foo");        // returns "bar"
obj.optString("foo");        // returns "bar"
obj.optString("foo", "tux"); // returns "bar"

// However, if a value cannot be coerced to the required type, the behavior differs
obj.getInt("foo");      // throws JSONException
obj.optInt("foo");      // returns 0
obj.optInt("foo", 123); // returns 123

// Same if a property does not exist
obj.getString("undefined");        // throws JSONException
obj.optString("undefined");        // returns ""
obj.optString("undefined", "tux"); // returns "tux"

```

The same rules apply to the `getXXX` / `optXXX` methods of `JSONArray`.



## Encoding data as JSON


If you need to create a `JSONObject` and put data in it, consider the following example:

```java
// Create a new javax.json.JSONObject instance.
JSONObject first = new JSONObject();

first.put("foo", "bar");
first.put("temperature", 21.5);
first.put("year", 2016);

// Add a second object.
JSONObject second = new JSONObject();
second.put("Hello", "world");
first.put("message", second);

// Create a new JSONArray with some values
JSONArray someMonths = new JSONArray(new String[] { "January", "February" });
someMonths.put("March");
// Add another month as the fifth element, leaving the 4th element unset.
someMonths.put(4, "May");

// Add the array to our object
object.put("months", someMonths);

// Encode
String json = object.toString();

// An exercise for the reader: Add pretty-printing!
/* {
       "foo":"bar",
       "temperature":21.5,
       "year":2016,
       "message":{"Hello":"world"},
       "months":["January","February","March",null,"May"]
   }
*/

```



## Decoding JSON data


If you need to get data from a `JSONObject`, consider the following example:

```java
String json = "{\"foo\":\"bar\",\"temperature\":21.5,\"year\":2016,\"message\":{\"Hello\":\"world\"},\"months\":[\"January\",\"February\",\"March\",null,\"May\"]}";

// Decode the JSON-encoded string
JSONObject object = new JSONObject(json);

// Retrieve some values
String foo = object.getString("foo");
double temperature = object.getDouble("temperature");
int year = object.getInt("year");

// Retrieve another object
JSONObject secondary = object.getJSONObject("message");
String world = secondary.getString("Hello");

// Retrieve an array
JSONArray someMonths = object.getJSONArray("months");
// Get some values from the array
int nMonths = someMonths.length();
String february = someMonths.getString(1);

```



## Extract single element from JSON


```java
String json = "{\"name\": \"John\", \"age\":21}";

JsonObject jsonObject = new JsonParser().parse(json).getAsJsonObject();

System.out.println(jsonObject.get("name").getAsString()); //John
System.out.println(jsonObject.get("age").getAsInt()); //21

```



## JsonArray to Java List (Gson Library)


Here is a simple JsonArray which you would like to convert to a Java `ArrayList`:

```java
{
    "list": [
                "Test_String_1",
                "Test_String_2"
            ] 
}

```

Now pass the `JsonArray` 'list' to the following method which returns a corresponding Java `ArrayList`:

```java
public ArrayList<String> getListString(String jsonList){
    Type listType = new TypeToken<List<String>>() {}.getType();
    //make sure the name 'list' matches the name of 'JsonArray' in your 'Json'.
    ArrayList<String> list = new Gson().fromJson(jsonList, listType);    
    return list;
}

```

You should add the following maven dependency to your `POM.xml` file:

```java
<!-- https://mvnrepository.com/artifact/com.google.code.gson/gson -->
<dependency>
    <groupId>com.google.code.gson</groupId>
    <artifactId>gson</artifactId>
    <version>2.7</version>
</dependency>

```

Or you should have the jar `com.google.code.gson:gson:jar:<version>` in your classpath.



## Deserialize JSON collection to collection of Objects using Jackson


Suppose you have a pojo class `Person`

```java
public class Person {
    public String name;

    public Person(String name) {
        this.name = name;
    }
}

```

And you want to parse it into a JSON array or a map of Person objects. Due to type erasure you cannot construct classes of `List<Person>` and `Map<String, Person>` at runtime directly **(and thus use them to deserialize JSON)**. To overcome this limitation jackson provides two approaches - `TypeFactory` and `TypeReference`.

**TypeFactory**

The approach taken here is to use a factory (and its static utility function) to build your type for you. The parameters it takes are the collection you want to use (list, set, etc.) and the class you want to store in that collection.

**TypeReference**

The type reference approach seems simpler because it saves you a bit of typing and looks cleaner. TypeReference accepts a type parameter, where you pass the desired type `List<Person>`. You simply instantiate this TypeReference object and use it as your type container.

Now let's look at how to actually deserialize your JSON into a Java object. If your JSON is formatted as an array, you can deserialize it as a List. If there is a more complex nested structure, you will want to deserialize to a Map. We will look at examples of both.

### Deserializing JSON array

```java
String jsonString = "[{\"name\": \"Alice\"}, {\"name\": \"Bob\"}]"

```

### TypeFactory approach

```java
CollectionType listType = 
    factory.constructCollectionType(List.class, Person.class);
List<Preson> list = mapper.readValue(jsonString, listType);

```

### TypeReference approach

```java
TypeReference<Person> listType = new TypeReference<List<Person>>() {};
List<Person> list = mapper.readValue(jsonString, listType);

```

### Deserializing JSON map

```java
String jsonString = "{\"0\": {\"name\": \"Alice\"}, \"1\": {\"name\": \"Bob\"}}"

```

### TypeFactory approach

```java
CollectionType mapType = 
    factory.constructMapLikeType(Map.class, String.class, Person.class);
List<Person> list = mapper.readValue(jsonString, mapType);

```

### TypeReference approach

```java
TypeReference<Person> mapType = new TypeReference<Map<String, Person>>() {};
Map<String, Person> list = mapper.readValue(jsonString, mapType);

```

### Details

Import statement used:

```java
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.type.CollectionType;

```

Instances used:

```java
ObjectMapper mapper = new ObjectMapper();
TypeFactory factory = mapper.getTypeFactory();

```

### Note

While `TypeReference` approach may look better it has several drawbacks:

1. `TypeReference` should be instantiated using anonymous class
1. You should provide generic explicity

Failing to do so may lead to loss of generic type argument which will lead to deserialization failure.



#### Remarks


This example focuses on parsing and creating JSON in Java using various libraries such as the **[Google Gson](https://github.com/google/gson)** library, Jackson Object Mapper, and others..

Examples using other libraries could be found here: [How to parse JSON in Java](http://stackoverflow.com/q/2591098/5020253)

