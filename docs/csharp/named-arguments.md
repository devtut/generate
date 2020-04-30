---
metaTitle: "Named Arguments"
description: "Argument order is not necessary, Named Arguments can make your code more clear, Named arguments and optional paramaters, Named Arguments avoids bugs on optional parameters"
---

# Named Arguments




## Argument order is not necessary


You can place named arguments in any order you want.

Sample Method:

```cs
public static string Sample(string left, string right)
{
     return string.Join("-",left,right);
}

```

Call Sample:

```cs
Console.WriteLine (Sample(left:"A",right:"B"));
Console.WriteLine (Sample(right:"A",left:"B"));

```

Results:

```cs
A-B
B-A

```



## Named Arguments can make your code more clear


Consider this simple class:

```cs
class SmsUtil
{
    public bool SendMessage(string from, string to, string message, int retryCount, object attachment)
    {
         // Some code
    }
}

```

Before C# 3.0 it was:

```cs
var result = SmsUtil.SendMessage("Mehran", "Maryam", "Hello there!", 12, null);

```

you can make this method call even more clear with **named arguments**:

```cs
var result = SmsUtil.SendMessage(
    from: "Mehran",
    to:  "Maryam",
    message "Hello there!",
    retryCount: 12,
    attachment: null);

```



## Named arguments and optional paramaters


You can combine named arguments with optional parameters.

Let see this method:

```cs
public sealed class SmsUtil
{
    public static bool SendMessage(string from, string to, string message, int retryCount = 5, object attachment = null)
    {
         // Some code
    }
}

```

When you want to call this method **without** set `retryCount` argument  :

```cs
var result = SmsUtil.SendMessage(
                        from       : "Cihan",
                        to         : "Yakar",
                        message    : "Hello there!",
                        attachment : new object());

```



## Named Arguments avoids bugs on optional parameters


Always use Named Arguments to optional parameters, to avoid potential bugs when the method is modified.

```cs
class Employee
{
    public string Name { get; private set; }

    public string Title { get; set; }

    public Employee(string name = "<No Name>", string title = "<No Title>")
    {
        this.Name = name;
        this.Title = title;
    }
}

var jack = new Employee("Jack", "Associate");   //bad practice in this line

```

The above code compiles and works fine, until the constructor is changed some day like:

```cs
//Evil Code: add optional parameters between existing optional parameters
public Employee(string name = "<No Name>", string department = "intern", string title = "<No Title>")
{
    this.Name = name;
    this.Department = department;
    this.Title = title;
}

//the below code still compiles, but now "Associate" is an argument of "department"
var jack = new Employee("Jack", "Associate");

```

Best practice to avoid bugs when "someone else in the team" made mistakes:

```cs
var jack = new Employee(name: "Jack", title: "Associate");

```

