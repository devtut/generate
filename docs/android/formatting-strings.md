---
metaTitle: "Android - Formatting Strings"
description: "Format a string resource, Formatting data types to String and vise versa, Format a timestamp to string"
---

# Formatting Strings



## Format a string resource


You can add wildcards in string resources and populate them at runtime:

<li>
Edit strings.xml

```java
<string name="my_string">This is %1$s</string>

```


</li>
<li>
Format string as needed

```java
String fun = "fun";
context.getString(R.string.my_string, fun);

```


</li>



## Formatting data types to String and vise versa


**Data types to string formatting**

Data types like int, float, double, long, boolean can be formatted to string using String.valueOf().

```java
String.valueOf(1); //Output -> "1"
String.valueOf(1.0); //Output -> "1.0"
String.valueOf(1.2345); //Output -> "1.2345"
String.valueOf(true); //Output -> "true"

```

Vise versa of this, formatting string to other data type

```java
Integer.parseInt("1"); //Output -> 1
Float.parseFloat("1.2"); //Output -> 1.2
Boolean.parseBoolean("true"); //Output -> true

```



## Format a timestamp to string


For full description of patterns, see [SimpleDateFormat reference](https://developer.android.com/reference/java/text/SimpleDateFormat.html)

```java
Date now = new Date();
long timestamp = now.getTime();
SimpleDateFormat sdf = new SimpleDateFormat("MM/dd/yyyy", Locale.US);
String dateStr = sdf.format(timestamp);

```

