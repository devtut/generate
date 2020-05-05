---
metaTitle: "C# | Handling FormatException when converting string to other types"
description: "Converting string to integer"
---

# Handling FormatException when converting string to other types



## Converting string to integer


There are various methods available for explicitly converting a `string` to an `integer`, such as:

<li>
`Convert.ToInt16();`
</li>
<li>
`Convert.ToInt32();`
</li>
<li>
`Convert.ToInt64();`
</li>
<li>
`int.Parse();`
</li>

But all these methods will throw a `FormatException`, if the input string contains non-numeric characters. For this, we need to write an additional exception handling(`try..catch`) to deal them in such cases.

---


**Explanation with Examples:**

So, let our input be:

```cs
string inputString = "10.2";

```

**Example 1:** `Convert.ToInt32()`

```cs
int convertedInt = Convert.ToInt32(inputString); // Failed to Convert 
// Throws an Exception "Input string was not in a correct format."

```

****Note:** Same goes for the other mentioned methods namely - `Convert.ToInt16();` and `Convert.ToInt64();`**
 

**Example 2:** `int.Parse()`

```cs
int convertedInt = int.Parse(inputString); // Same result "Input string was not in a correct format.

```

****How do we circumvent this?****

As told earlier, for handling the exceptions we usually need a `try..catch` as shown below:

```cs
try
{
    string inputString = "10.2";
    int convertedInt = int.Parse(inputString);
}
catch (Exception Ex)
{
    //Display some message, that the conversion has failed.         
}

```

But, using the `try..catch` everywhere will not be a good practice, and there may be some scenarios where we wanted to give `0` if the input is wrong, **(If we follow the above method we need to assign `0` to `convertedInt` from the catch block).**
To handle such scenarios we can make use of a special method called `.TryParse()`.

The `.TryParse()` method having an internal Exception handling, which will give you the output to the `out` parameter, and returns a Boolean value indicating the conversion status **(`true` if the conversion was successful; `false` if it failed).** Based on the return value we can determine the conversion status. Lets see one Example:

**Usage 1:** Store the return value in a Boolean variable

```

int convertedInt; // Be the required integer
 bool isSuccessConversion = int.TryParse(inputString, out convertedInt);

```

We can check The variable `isSuccessConversion` after the Execution to check the conversion status. If it is false then the value of `convertedInt` will be `0`**(no need to check the return value if you want `0` for conversion failure).**

**Usage 2:** Check the return value with `if`

```cs
if (int.TryParse(inputString, out convertedInt))
{
    // convertedInt will have the converted value
    // Proceed with that
}
else 
{
 // Display an error message
}

```

**Usage 3:** Without checking the return value
you can use the following, if you don't care about the return value **(converted or not, `0` will be ok)**

```cs
int.TryParse(inputString, out convertedInt);
// use the value of convertedInt
// But it will be 0 if not converted

```

