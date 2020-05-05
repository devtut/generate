---
metaTitle: "C# | StringBuilder"
description: "What a StringBuilder is and when to use one, Use StringBuilder to create string from a large number of records"
---

# StringBuilder



## What a StringBuilder is and when to use one


A [`StringBuilder`](https://msdn.microsoft.com/en-us/library/system.text.stringbuilder(v=vs.110).aspx) represents a series of characters, which unlike a normal string, are mutable. Often times there is a need to modify strings that we've already made, but the standard string object is not mutable. This means that each time a string is modified, a new string object needs to be created, copied to, and then reassigned.

```cs
string myString = "Apples";
mystring += " are my favorite fruit";

```

In the above example, `myString` initially only has the value `"Apples"`. However, when we concatenate `" are my favorite fruit"', what the string class does internally needs to do involves:

- Creating a new array of characters equal to the length of `myString` and the new string we are appending.
- Copying all of the characters of `myString` into the beginning of our new array and copying the new string into the end of the array.
- Create a new string object in memory and reassign it to `myString`.

For a single concatenation, this is relatively trivial. However, what if needed to perform many append operations, say, in a loop?

```cs
String myString = "";
for (int i = 0; i < 10000; i++)
    myString += " "; // puts 10,000 spaces into our string

```

Due to the repeated copying and object creation, this will bring significantly degrade the performance of our program. We can avoid this by instead using a `StringBuilder`.

```cs
StringBuilder myStringBuilder = new StringBuilder();    
for (int i = 0; i < 10000; i++)
    myStringBuilder.Append(' ');

```

Now when the same loop is run, the performance and speed of the execution time of the program will be significantly faster than using a normal string. To make the `StringBuilder` back into a normal string, we can simply call the `ToString()` method of `StringBuilder`.

However, this isn't the only optimization `StringBuilder` has. In order to further optimize functions, we can take advantage of other properties that help improve performance.

```cs
StringBuilder sb = new StringBuilder(10000); // initializes the capacity to 10000

```

If we know in advance how long our `StringBuilder` needs to be, we can specify its size ahead of time, which will prevent it from needing to resize the character array it has internally.

```cs
sb.Append('k', 2000);

```

Though using `StringBuilder` for appending is much faster than a string, it can run even faster if you only need to add a single character many times.

Once you have completed building your string, you may use the `ToString()` method on the `StringBuilder` to convert it to a basic `string`. This is often necessary because the `StringBuilder` class does not inherit from `string`.

For example, here is how you can use a `StringBuilder` to create a `string`:

```cs
string RepeatCharacterTimes(char character, int times)
{
    StringBuilder builder = new StringBuilder("");
    for (int counter = 0; counter < times; counter++)
    {
        //Append one instance of the character to the StringBuilder.
        builder.Append(character);
    }
    //Convert the result to string and return it.
    return builder.ToString();
}

```

In conclusion, `StringBuilder` should be used in place of string when many modifications to a string need to be made with performance in mind.



## Use StringBuilder to create string from a large number of records


```cs
public string GetCustomerNamesCsv()
{
    List<CustomerData> customerDataRecords = GetCustomerData(); // Returns a large number of records, say, 10000+

    StringBuilder customerNamesCsv = new StringBuilder();
    foreach (CustomerData record in customerDataRecords)
    {
       customerNamesCsv
           .Append(record.LastName)
           .Append(',')
           .Append(record.FirstName)
           .Append(Environment.NewLine);
    }

    return customerNamesCsv.ToString();
}

```

