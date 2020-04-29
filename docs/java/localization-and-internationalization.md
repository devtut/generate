---
metaTitle: "Localization and Internationalization"
description: "Automatically formatted Dates using locale, String Comparison, Locale"
---

# Localization and Internationalization




## Automatically formatted Dates using "locale"


`SimpleDateFormatter` is great in a pinch, but like the name suggests it doesn't scale well.

If you hard-code `"MM/dd/yyyy"` all over your application your international users won't be happy.

### Let Java do the work for you

Use the `static` methods in [`DateFormat`](https://docs.oracle.com/javase/8/docs/api/java/text/DateFormat.html) to retrieve the right formatting for your user. For a desktop application (where you'll rely on the [default locale](https://docs.oracle.com/javase/8/docs/api/java/util/Locale.html#getDefault--)), simply call:

```java
String localizedDate = DateFormat.getDateInstance(style).format(date);

```

Where `style` is one of the formatting constants (`FULL`, `LONG`, `MEDIUM`, `SHORT`, etc.) specified in `DateFormat`.

For a server-side application where the user specifies their locale as part of the request, you should pass it explicitly to `getDateInstance()` instead:

```java
String localizedDate =
    DateFormat.getDateInstance(style, request.getLocale()).format(date);

```



## String Comparison


Compare two Strings ignoring case:

```java
"School".equalsIgnoreCase("school"); // true

```

Don't use

```java
text1.toLowerCase().equals(text2.toLowerCase());

```

Languages have different rules for converting upper and lower case. A 'I' would be converted to 'i' in English. But in Turkish a 'I' becomes a 'ı'. If you have to use `toLowerCase()` use the overload which expects a `Locale`: `String.toLowerCase(Locale)`.

Comparing two Strings ignoring minor differences:

```java
Collator collator = Collator.getInstance(Locale.GERMAN);
collator.setStrength(Collator.PRIMARY);
collator.equals("Gärten", "gaerten"); // returns true

```

Sort Strings respecting natural language order, ignoring case (use collation key to:

```java
String[] texts = new String[] {"Birne", "äther", "Apfel"};
Collator collator = Collator.getInstance(Locale.GERMAN);
collator.setStrength(Collator.SECONDARY); // ignore case
Arrays.sort(texts, collator::compare); // will return {"Apfel", "äther", "Birne"}

```



## Locale


The `java.util.Locale` class is used to represent a "geographical, political or cultural" region to localize a given text, number, date or operation to. A Locale object may thus contain a country, region, language, and also a variant of a language, for instance a dialect spoken in a certain region of a country, or spoken in a different country than the country from which the language originates.

The Locale instance is handed to components that need to localize their actions, whether it is converting the input, output, or just need it for internal operations. The Locale class cannot do any internationalization or localization by itself

### Language

The language must be an ISO 639 2 or 3 character language code, or a registered language subtag of up to 8 characters. In case a language has both a 2 and 3 character language code, use the 2 character code. A full list of language codes can be found in the IANA Language Subtag Registry.

Language codes are case insensitive, but the Locale class always use lowercase versions of the language codes

### Creating a Locale

Creating a `java.util.Locale` instance can be done in four different ways:

```java
Locale constants
Locale constructors
Locale.Builder class
Locale.forLanguageTag factory method 

```

### Java ResourceBundle

You create a ResourceBundle instance like this:

```java
Locale locale = new Locale("en", "US");
ResourceBundle labels = ResourceBundle.getBundle("i18n.properties");
System.out.println(labels.getString("message"));

```

Consider I have a property file `i18n.properties`:

```java
message=This is locale

```

Output:

```java
This is locale

```

### Setting Locale

If you want to reproduce the state using other languages, you can use **`setDefault()`** method.
Its usage:

```

setDefault(Locale.JAPANESE); //Set Japanese

```



#### Remarks


Java comes with a powerful and flexible mechanism for localizing your applications, but it's also easy to misuse and wind up with a program that disregards or mangles the user's locale, and therefore how they expect your program to behave.

Your users will expect to see data localized to the formats they're used to, and attempting to support this manually is a fools errand. Here is just a small example of the different ways users expect to see content you might assume is "always" displayed a certain way:

||Dates|Numbers|Local Currency|Foreign Currency|Distances
|------
|Brazil|||||
|China|||||
|Egypt|||||
|Mexico|20/3/16|1.234,56|$1,000.50|1,000.50 USD|
|UK|20/3/16|1,234.56|£1,000.50||100 km
|USA|3/20/16|1,234.56|$1,000.50|1,000.50 MXN|60 mi

### General Resources

- Wikipedia: [Internationalization and Localization](https://en.wikipedia.org/wiki/Internationalization_and_localization)

### Java Resources

- Java Tutorial: [Internationalization](https://docs.oracle.com/javase/tutorial/i18n/index.html)
- Oracle: [Internationalization: Understanding Locale in the Java Platform](http://www.oracle.com/us/technologies/java/locale-140624.html)
- JavaDoc: [`Locale`](https://docs.oracle.com/javase/8/docs/api/java/util/Locale.html)

