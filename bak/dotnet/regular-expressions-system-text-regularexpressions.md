---
metaTitle: ".NET Framework - Regular Expressions (System.Text.RegularExpressions)"
description: "Check if pattern matches input, Passing Options, Match into groups, Remove non alphanumeric characters from string, Simple match and replace, Find all matches"
---

# Regular Expressions (System.Text.RegularExpressions)



## Check if pattern matches input


```dotnet
public bool Check()
{
    string input = "Hello World!";
    string pattern = @"H.ll. W.rld!";

    // true
    return Regex.IsMatch(input, pattern);
}

```



## Passing Options


```dotnet
public bool Check()
{
    string input = "Hello World!";
    string pattern = @"H.ll. W.rld!";

    // true
    return Regex.IsMatch(input, pattern, RegexOptions.IgnoreCase | RegexOptions.Singleline);
}

```



## Match into groups


```dotnet
public string Check()
{
    string input = "Hello World!";
    string pattern = @"H.ll. (?<Subject>W.rld)!";

    Match match = Regex.Match(input, pattern);

    // World
    return match.Groups["Subject"].Value;
}

```



## Remove non alphanumeric characters from string


```dotnet
public string Remove()
{
    string input = "Hello./!";
    
    return Regex.Replace(input, "[^a-zA-Z0-9]", "");
}

```



## Simple match and replace


```dotnet
public string Check()
{
    string input = "Hello World!";
    string pattern = @"W.rld";

    // Hello Stack Overflow!
    return Regex.Replace(input, pattern, "Stack Overflow");
}

```



## Find all matches


### Using

```dotnet
using System.Text.RegularExpressions;

```

### Code

```dotnet
static void Main(string[] args)
{
    string input = "Carrot Banana Apple Cherry Clementine Grape";
    // Find words that start with uppercase 'C'
    string pattern = @"\bC\w*\b";

    MatchCollection matches = Regex.Matches(input, pattern);
    foreach (Match m in matches)
        Console.WriteLine(m.Value);
}

```

### Output

```dotnet
Carrot
Cherry
Clementine

```

