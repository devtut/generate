---
metaTitle: "Android - Gson"
description: "Parsing JSON with Gson, Adding a custom Converter to Gson, Parsing a List<String> with Gson, Parsing JSON to Generic Class Object with Gson, Adding Gson to your project, Parsing JSON property to enum with Gson, JSON Serialization/Deserialization with AutoValue and Gson, Using Gson to load a JSON file from disk., Using Gson as serializer with Retrofit, Parsing json array to generic class using Gson, Custom JSON Deserializer using Gson, Using Gson with inheritance"
---

# Gson


**Gson** is a Java library that can be used to convert Java Objects into their JSON representation. Gson considers both of these as very important design goals.

**Gson Features:**

Provide simple `toJson()` and `fromJson()` methods to convert Java objects to JSON and vice-versa

Allow pre-existing unmodifiable objects to be converted to and from JSON

Extensive support of Java Generics

Support arbitrarily complex objects (with deep inheritance hierarchies and extensive use of generic types)



## Parsing JSON with Gson


The example shows parsing a JSON object using the [Gson library from Google](https://github.com/google/gson).

**Parsing objects:**

```java
class Robot {
    //OPTIONAL - this annotation allows for the key to be different from the field name, and can be omitted if key and field name are same . Also this is good coding practice as it decouple your variable names with server keys name 
    @SerializedName("version") 
    private String version;

    @SerializedName("age")
    private int age;
    
    @SerializedName("robotName")
    private String name;
    
    // optional : Benefit it allows to set default values and retain them, even if key is missing from Json response. Not required for primitive data types. 

    public Robot{
       version = "";
       name = "";
    }


}

```

Then where parsing needs to occur, use the following:

```java
String robotJson = "{
                        \"version\": \"JellyBean\",
                        \"age\": 3,
                        \"robotName\": \"Droid\"
                   }";

Gson gson = new Gson();
Robot robot = gson.fromJson(robotJson, Robot.class);

```

**Parsing a list:**

When retrieving a list of JSON objects, often you will want to parse them and convert them into Java objects.

The JSON string that we will try to convert is the following:

```java
{
  "owned_dogs": [
    {
      "name": "Ron",
      "age": 12,
      "breed": "terrier"
    },
    {
      "name": "Bob",
      "age": 4,
      "breed": "bulldog"
    },
    {
      "name": "Johny",
      "age": 3,
      "breed": "golden retriever"
    }
  ]
}

```

This particular JSON array contains three objects. In our Java code we'll want to map these objects to `Dog` objects. A Dog object would look like this:

```java
private class Dog {
    public String name;
    public int age;

    @SerializedName("breed")
    public String breedName;
}

```

To convert the JSON array to a `Dog[]`:

```java
Dog[] arrayOfDogs = gson.fromJson(jsonArrayString, Dog[].class);

```

Converting a `Dog[]` to a JSON string:

```java
String jsonArray = gson.toJson(arrayOfDogs, Dog[].class);

```

To convert the JSON array to an `ArrayList<Dog>` we can do the following:

```java
Type typeListOfDogs = new TypeToken<List<Dog>>(){}.getType();
List<Dog> listOfDogs = gson.fromJson(jsonArrayString, typeListOfDogs);

```

The Type object `typeListOfDogs` defines what a list of `Dog` objects would look like. GSON can use this type object to map the JSON array to the right values.

Alternatively, converting a `List<Dog>` to a JSON array can be done in a similar manner.

```java
String jsonArray = gson.toJson(listOfDogs, typeListOfDogs);

```



## Adding a custom Converter to Gson


Sometimes you need to serialize or deserialize some fields in a desired format, for example your backend may use the format "YYYY-MM-dd HH:mm" for dates and you want your POJOS to use the DateTime class in Joda Time.

In order to automatically convert these strings into DateTimes object, you can use a custom converter.

```java
/**
 * Gson serialiser/deserialiser for converting Joda {@link DateTime} objects.
 */
public class DateTimeConverter implements JsonSerializer<DateTime>, JsonDeserializer<DateTime> {

    private final DateTimeFormatter dateTimeFormatter;

    @Inject
    public DateTimeConverter() {
        this.dateTimeFormatter = DateTimeFormat.forPattern("YYYY-MM-dd HH:mm");
    }

    @Override
    public JsonElement serialize(DateTime src, Type typeOfSrc, JsonSerializationContext context) {
        return new JsonPrimitive(dateTimeFormatter.print(src));
    }

    @Override
    public DateTime deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context)
            throws JsonParseException {
        
        if (json.getAsString() == null || json.getAsString().isEmpty()) {
            return null;
        }

        return dateTimeFormatter.parseDateTime(json.getAsString());
    }
}

```

To make Gson use the newly created converter you need to assign it when creating the Gson object:

```

   DateTimeConverter dateTimeConverter = new DateTimeConverter();
    Gson gson = new GsonBuilder().registerTypeAdapter(DateTime.class, dateTimeConverter)
            .create();

    String s = gson.toJson(DateTime.now());
    // this will show the date in the desired format

```

In order to deserialize the date in that format you only have to define a field in the DateTime format:

```java
public class SomePojo {
    private DateTime someDate;    
} 

```

When Gson encounters a field of type DateTime, it will call your converter in order to deserialize the field.



## Parsing a List<String> with Gson


**Method 1**

```java
Gson gson = new Gson();
String json = "[ \"Adam\", \"John\", \"Mary\" ]";

Type type = new TypeToken<List<String>>(){}.getType();
List<String> members = gson.fromJson(json, type);
Log.v("Members", members.toString());

```

This is useful for most generic container classes, since you can't get the class of a parameterized type (ie: you can't call `List<String>.class`).

**Method 2**

```java
public class StringList extends ArrayList<String> { }

...

List<String> members = gson.fromJson(json, StringList.class);

```

Alternatively, you can always subclass the type you want, and then pass in that class. However this isn't always best practice, since it will return to you an object of type `StringList`;



## Parsing JSON to Generic Class Object with Gson


Suppose we have a JSON string：

```java
["first","second","third"]

```

We can parse this JSON string into a `String` array :

```java
Gson gson = new Gson();
String jsonArray = "[\"first\",\"second\",\"third\"]";
String[] strings = gson.fromJson(jsonArray, String[].class);

```

But if we want parse it into a `List<String>` object, we must use `TypeToken`.

Here is the sample :

```java
Gson gson = new Gson();
String jsonArray = "[\"first\",\"second\",\"third\"]";
List<String> stringList = gson.fromJson(jsonArray, new TypeToken<List<String>>() {}.getType());

```

Suppose we have two classes below:

```java
public class Outer<T> {
    public int index;
    public T data;
}

public class Person {
    public String firstName;
    public String lastName;
}

```

and we have a JSON string that should be parsed to a `Outer<Person>` object.

This example shows how to parse this JSON string to the related generic class object:

```java
String json = "......";
Type userType = new TypeToken<Outer<Person>>(){}.getType();
Result<User> userResult = gson.fromJson(json,userType);

```

If the JSON string should be parsed to a `Outer<List<Person>>` object :

```java
Type userListType = new TypeToken<Outer<List<Person>>>(){}.getType();
Result<List<User>> userListResult = gson.fromJson(json,userListType); 

```



## Adding Gson to your project


```java
dependencies {
    compile 'com.google.code.gson:gson:2.8.1'
}

```

**To use latest version of Gson**<br />

The below line will compile latest version of gson library everytime you compile, you do not have to change version.

**Pros:**  You can use latest features, speed and less bugs.<br />
**Cons:**  It might break compatibility with your code.

```java
compile 'com.google.code.gson:gson:+'

```



## Parsing JSON property to enum with Gson


If you want to parse a String to enum with Gson:

> 
{"status" : "open"}


```java
public enum Status {
    @SerializedName("open")
    OPEN,
    @SerializedName("waiting")
    WAITING,
    @SerializedName("confirm")
    CONFIRM,
    @SerializedName("ready")
    READY
}

```



## JSON Serialization/Deserialization with AutoValue and Gson


Import in your gradle root file

```java
classpath 'com.neenbedankt.gradle.plugins:android-apt:1.8'

```

Import in your gradle app file

```java
apt 'com.google.auto.value:auto-value:1.2'  
apt 'com.ryanharter.auto.value:auto-value-gson:0.3.1'  
provided 'com.jakewharton.auto.value:auto-value-annotations:1.2-update1'  
provided 'org.glassfish:javax.annotation:10.0-b28'

```

Create object with autovalue:

```java
@AutoValue public abstract class SignIn {    
    @SerializedName("signin_token") public abstract String signinToken();
    public abstract String username();

    public static TypeAdapter<SignIn> typeAdapter(Gson gson) {
        return new AutoValue_SignIn.GsonTypeAdapter(gson);
    }

    public static SignIn create(String signin, String username) {
        return new AutoValue_SignIn(signin, username);
    }
}

```

Create your Gson converter with your GsonBuilder

```java
Gson gson = new GsonBuilder()
                .registerTypeAdapterFactory(
                    new AutoValueGsonTypeAdapterFactory())
                .create());

```

**Deserialize**

```java
String myJsonData = "{
    \"signin_token\": \"mySigninToken\",
    \"username\": \"myUsername\" }";
SignIn signInData = gson.fromJson(myJsonData, Signin.class);

```

**Serialize**

```java
Signin myData = SignIn.create("myTokenData", "myUsername");
String myJsonData = gson.toJson(myData);

```

Using Gson is a great way to simplify Serialization and Deserialization code by using POJO objects. The side effect is that reflection is costly performance wise. That's why using AutoValue-Gson to generate CustomTypeAdapter will avoid this reflection cost while staying very simple to update when an api change is happening.



## Using Gson to load a JSON file from disk.


This will load a JSON file from disk and convert it to the given type.

```java
public static <T> T getFile(String fileName, Class<T> type) throws FileNotFoundException {
    Gson gson = new GsonBuilder()
            .create();
    FileReader json = new FileReader(fileName);
    return gson.fromJson(json, type);
}

```



## Using Gson as serializer with Retrofit


First of all you need to add the `GsonConverterFactory` to your build.gradle file

```java
compile 'com.squareup.retrofit2:converter-gson:2.1.0'

```

Then, you have to add the converter factory when creating the Retrofit Service:

```java
Gson gson = new GsonBuilder().create();
new Retrofit.Builder()
        .baseUrl(someUrl)
        .addConverterFactory(GsonConverterFactory.create(gson))
        .build()
        .create(RetrofitService.class);

```

You can add custom converters when creating the Gson object that you are passing to the factory. Allowing you to create custom type conversions.



## Parsing json array to generic class using Gson


Suppose we have a json ：

```java
{
  "total_count": 132,
  "page_size": 2,
  "page_index": 1,
  "twitter_posts": [
    {
      "created_on": 1465935152,
      "tweet_id": 210462857140252672,
      "tweet": "Along with our new #Twitterbird, we've also updated our Display Guidelines",
      "url": "https://twitter.com/twitterapi/status/210462857140252672"
    },
    {
      "created_on": 1465995741,
      "tweet_id": 735128881808691200,
      "tweet": "Information on the upcoming changes to Tweets is now on the developer site",
      "url": "https://twitter.com/twitterapi/status/735128881808691200"
    }
  ]
}

```

We can parse this array into a Custom Tweets (tweets list container) object manually, but it is easier to do it with `fromJson` method:

```java
Gson gson = new Gson();
String jsonArray = "....";
Tweets tweets = gson.fromJson(jsonArray, Tweets.class);

```

Suppose we have two classes below:

```java
class Tweets {
    @SerializedName("total_count")
    int totalCount;
    @SerializedName("page_size")
    int pageSize;
    @SerializedName("page_index")
    int pageIndex;
    // all you need to do it is just define List variable with correct name 
    @SerializedName("twitter_posts")
    List<Tweet> tweets;
}

class Tweet {
    @SerializedName("created_on")
    long createdOn;
    @SerializedName("tweet_id")
    String tweetId;
    @SerializedName("tweet")
    String tweetBody;
    @SerializedName("url")
    String url;
}

```

and if you need just parse a json array you can use this code in your parsing:

```java
String tweetsJsonArray = "[{.....},{.....}]"
List<Tweet> tweets = gson.fromJson(tweetsJsonArray, new TypeToken<List<Tweet>>() {}.getType());

```



## Custom JSON Deserializer using Gson


Imagine you have all dates in all responses in some custom format, for instance `/Date(1465935152)/` and you want apply general rule to deserialize all Json dates to java `Date` instances. In this case you need to implement custom Json Deserializer.

Example of json:

```java
{
  "id": 1,
  "created_on": "Date(1465935152)",
  "updated_on": "Date(1465968945)",
  "name": "Oleksandr"
}

```

Suppose we have this class below:

```java
class User {
    @SerializedName("id")
    long id;
    @SerializedName("created_on")
    Date createdOn;
    @SerializedName("updated_on")
    Date updatedOn;
    @SerializedName("name")
    String name;
}

```

Custom deserializer:

```java
class DateDeSerializer implements JsonDeserializer<Date> {
    private static final String DATE_PREFIX = "/Date(";
    private static final String DATE_SUFFIX = ")/";

    @Override
    public Date deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context) throws JsonParseException {
        String dateString = json.getAsString();
        if (dateString.startsWith(DATE_PREFIX) && dateString.endsWith(DATE_SUFFIX)) {
            dateString = dateString.substring(DATE_PREFIX.length(), dateString.length() - DATE_SUFFIX.length());
        } else {
            throw new JsonParseException("Wrong date format: " + dateString);
        }
        return new Date(Long.parseLong(dateString) - TimeZone.getDefault().getRawOffset());
    }
}

```

And the usage:

```java
Gson gson = new GsonBuilder()
                .registerTypeAdapter(Date.class, new DateDeSerializer())
                .create();
String json = "....";
User user = gson.fromJson(json, User.class);

```

**Serialize and deserialize Jackson JSON strings with Date types**

This also applies to the case where you want to make Gson Date conversion compatible with Jackson, for example.

Jackson usually serializes Date to "milliseconds since epoch" whereas Gson uses a readable format like `Aug 31, 2016 10:26:17` to represent Date. This leads to JsonSyntaxExceptions in Gson when you try to deserialize a Jackson format Date.

To circumvent this, you can add a custom serializer and a custom deserializer:

```java
JsonSerializer<Date> ser = new JsonSerializer<Date>() {
    @Override
    public JsonElement serialize(Date src, Type typeOfSrc, JsonSerializationContext
                context) {
        return src == null ? null : new JsonPrimitive(src.getTime());
    }
};

JsonDeserializer<Date> deser = new JsonDeserializer<Date>() {
    @Override
    public Date deserialize(JsonElement json, Type typeOfT,
                JsonDeserializationContext context) throws JsonParseException {
        return json == null ? null : new Date(json.getAsLong());
    }
};

Gson gson = new GsonBuilder()
                .registerTypeAdapter(Date.class, ser)
                .registerTypeAdapter(Date.class, deser)
                .create();

```



## Using Gson with inheritance


Gson does not support inheritance out of the box.

Let's say we have the following class hierarchy:

```java
public class BaseClass {
    int a;
 
    public int getInt() {
        return a;
   }
}
 
public class DerivedClass1 extends BaseClass {
     int b;
 
     @Override
     public int getInt() {
         return b;
     }
 }
 
public class DerivedClass2 extends BaseClass {
    int c;
 
    @Override
    public int getInt() {
        return c;
    }
}

```

And now we want to serialize an instance of `DerivedClass1` to a JSON string

```java
DerivedClass1 derivedClass1 = new DerivedClass1();
derivedClass1.b = 5;
derivedClass1.a = 10;
 
Gson gson = new Gson();
String derivedClass1Json = gson.toJson(derivedClass1);

```

Now, in another place, we receive this json string and want to deserialize it - but in compile time we only know it is supposed to be an instance of `BaseClass`:

```java
BaseClass maybeDerivedClass1 = gson.fromJson(derivedClass1Json, BaseClass.class);
System.out.println(maybeDerivedClass1.getInt());

```

But GSON does not know `derivedClass1Json` was originally an instance of `DerivedClass1`, so this will print out 10.

**How to solve this?**

You need to build your own `JsonDeserializer`, that handles such cases. The solution is not perfectly clean, but I could not come up with a better one.

First, add the following field to your base class

```java
@SerializedName("type")
private String typeName;

```

And initialize it in the base class constructor

```java
public BaseClass() {
    typeName = getClass().getName();
}

```

Now add the following class:

```java
public class JsonDeserializerWithInheritance<T> implements JsonDeserializer<T> {
 
 @Override
 public T deserialize(
     JsonElement json, Type typeOfT, JsonDeserializationContext context)
     throws JsonParseException {
     JsonObject jsonObject = json.getAsJsonObject();
     JsonPrimitive classNamePrimitive = (JsonPrimitive) jsonObject.get("type");
 
     String className = classNamePrimitive.getAsString();
 
     Class<?> clazz;
     try {
     clazz = Class.forName(className);
     } catch (ClassNotFoundException e) {
     throw new JsonParseException(e.getMessage());
     }
     return context.deserialize(jsonObject, clazz);
 }
}

```

All there is left to do is hook everything up -

```java
GsonBuilder builder = new GsonBuilder();
 builder
 .registerTypeAdapter(BaseClass.class, new JsonDeserializerWithInheritance<BaseClass>());
 Gson gson = builder.create();

```

And now, running the following code-

```

DerivedClass1 derivedClass1 = new DerivedClass1();
 derivedClass1.b = 5;
 derivedClass1.a = 10;
 String derivedClass1Json = gson.toJson(derivedClass1);
 
 BaseClass maybeDerivedClass1 = gson.fromJson(derivedClass1Json, BaseClass.class);
 System.out.println(maybeDerivedClass1.getInt());

```

Will print out 5.



#### Syntax


- Excluder excluder()
- FieldNamingStrategy fieldNamingStrategy()
- <T> T fromJson(JsonElement json, Class<T> classOfT)
- <T> T fromJson(JsonElement json, Type typeOfT)
- <T> T fromJson(JsonReader reader, Type typeOfT)
- <T> T fromJson(Reader json, Class<T> classOfT)
- <T> T fromJson(Reader json, Type typeOfT)
- <T> T fromJson(String json, Class<T> classOfT)
- <T> T fromJson(String json, Type typeOfT)
- <T> TypeAdapter<T> getAdapter(Class<T> type)
- <T> TypeAdapter<T> getAdapter(TypeToken<T> type)
- <T> TypeAdapter<T> getDelegateAdapter(TypeAdapterFactory skipPast, TypeToken<T> type)
- JsonReader newJsonReader(Reader reader)
- JsonWriter newJsonWriter(Writer writer)
- JsonElement toJsonTree(Object src)
- JsonElement toJsonTree(Object src, Type typeOfSrc)
- boolean serializeNulls()
- boolean htmlSafe()
- String toJson(JsonElement jsonElement)
- String toJson(Object src)
- String toJson(Object src, Type typeOfSrc)
- String toString()
- void toJson(Object src, Type typeOfSrc, Appendable writer)
- void toJson(Object src, Type typeOfSrc, JsonWriter writer)
- void toJson(JsonElement jsonElement, Appendable writer)
- void toJson(JsonElement jsonElement, JsonWriter writer)
- void toJson(Object src, Appendable writer)

