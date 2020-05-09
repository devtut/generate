---
metaTitle: "PowerShell - Regular Expressions"
description: "Single match, Replace, Replace text with dynamic value using a MatchEvalutor, Escape special characters, Multiple matches"
---

# Regular Expressions



## Single match


You can quickly determine if a text includes a specific pattern using Regex. There are multiple ways to work with Regex in PowerShell.

```powershell
#Sample text
$text = @"
This is (a) sample
text, this is
a (sample text)
"@

#Sample pattern: Content wrapped in ()
$pattern = '\(.*?\)'

```

### Using the -Match operator

To determine if a string matches a pattern using the built-in `-matches` operator, use the syntax `'input' -match 'pattern'`. This will return `true` or `false` depending on the result of the search. If there was match you can view the match and groups (if defined in pattern) by accessing the `$Matches`-variable.

```powershell
> $text -match $pattern
True

> $Matches

Name Value
---- -----
0    (a)  

```

You can also use `-match` to filter through an array of strings and only return the strings containing a match.

```powershell
> $textarray = @"
This is (a) sample
text, this is
a (sample text)
"@ -split "`n"

> $textarray -match $pattern
This is (a) sample
a (sample text)

```

### Using Select-String

PowerShell 2.0 introduced a new cmdlet for searching through text using regex. It returns a `MatchInfo` object per textinput that contains a match. You can access it's properties to find matching groups etc.

```powershell
> $m = Select-String -InputObject $text -Pattern $pattern

> $m

This is (a) sample
text, this is
a (sample text)

> $m | Format-List *

IgnoreCase : True
LineNumber : 1
Line       : This is (a) sample
             text, this is
             a (sample text)
Filename   : InputStream
Path       : InputStream
Pattern    : \(.*?\)
Context    : 
Matches    : {(a)}

```

Like `-match`, `Select-String` can also be used to filter through an array of strings by piping an array to it. It creates a `MatchInfo`-object per string that includes a match.

```powershell
> $textarray | Select-String -Pattern $pattern

This is (a) sample
a (sample text)

#You can also access the matches, groups etc.
> $textarray | Select-String -Pattern $pattern | fl *


IgnoreCase : True
LineNumber : 1
Line       : This is (a) sample
Filename   : InputStream
Path       : InputStream
Pattern    : \(.*?\)
Context    : 
Matches    : {(a)}

IgnoreCase : True
LineNumber : 3
Line       : a (sample text)
Filename   : InputStream
Path       : InputStream
Pattern    : \(.*?\)
Context    : 
Matches    : {(sample text)}

```

`Select-String` can also search using a normal text-pattern (no regex) by adding the `-SimpleMatch` switch.

### Using [RegEx]::Match()

You can also use the static `Match()` method available in the .NET `[RegEx]`-class.

```powershell
> [regex]::Match($text,$pattern)

Groups   : {(a)}
Success  : True
Captures : {(a)}
Index    : 8
Length   : 3
Value    : (a)

> [regex]::Match($text,$pattern) | Select-Object -ExpandProperty Value
(a)

```



## Replace


A common task for regex is to replace text that matches a pattern with a new value.

```powershell
#Sample text
$text = @"
This is (a) sample
text, this is
a (sample text)
"@

#Sample pattern: Text wrapped in ()
$pattern = '\(.*?\)'

#Replace matches with:
$newvalue = 'test'

```

### Using -Replace operator

The `-replace` operator in PowerShell can be used to replace text matching a pattern with a new value using the syntax `'input' -replace 'pattern', 'newvalue'`.

```powershell
> $text -replace $pattern, $newvalue
This is test sample
text, this is
a test

```

### Using [RegEx]::Replace() method

Replacing matches can also be done using the `Replace()` method in the `[RegEx]` .NET class.

```powershell
[regex]::Replace($text, $pattern, 'test')
This is test sample
text, this is
a test

```



## Replace text with dynamic value using a MatchEvalutor


Sometimes you need to replace a value matching a pattern with a new value that's based on that specific match, making it impossible to predict the new value. For these types of scenarios, a `MatchEvaluator` can be very useful.

In PowerShell, a `MatchEvaluator` is as simple as a scriptblock with a single paramter that contains a [`Match`](https://msdn.microsoft.com/en-us/library/system.text.regularexpressions.match(v=vs.110).aspx)-object for the current match. The output of the action will be the new value for that specific match. `MatchEvalutor` can be used with the `[Regex]::Replace()` static method.

**Example**: Replacing the text inside `()` with it's length

```powershell
#Sample text
$text = @"
This is (a) sample
text, this is
a (sample text)
"@
    
#Sample pattern: Content wrapped in ()
$pattern = '(?<=\().*?(?=\))'

$MatchEvalutor = {
    param($match)

    #Replace content with length of content
    $match.Value.Length

}

```

Output:

```powershell
> [regex]::Replace($text, $pattern, $MatchEvalutor)

This is 1 sample
text, this is
a 11

```

**Example:** Make `sample` upper-case

```powershell
#Sample pattern: "Sample"
$pattern = 'sample'

$MatchEvalutor = {
    param($match)

    #Return match in upper-case
    $match.Value.ToUpper()

}

```

Output:

```powershell
> [regex]::Replace($text, $pattern, $MatchEvalutor)

This is (a) SAMPLE
text, this is
a (SAMPLE text)

```



## Escape special characters


A regex-pattern uses many special characters to describe a pattern. Ex., `.` means "any character", `+` is "one or more" etc.

To use these characters, as a `.`,`+` etc., in a pattern, you need to escape them to remove their special meaning. This is done by using the escape character which is a backslash `\` in regex. Example: To search for `+`, you would use the pattern `\+`.

It can be hard to remember all special characters in regex, so to escape every special character in a string you want to search for, you could use the `[RegEx]::Escape("input")` method.

```powershell
> [regex]::Escape("(foo)")
\(foo\)

> [regex]::Escape("1+1.2=2.2")
1\+1\.2=2\.2

```



## Multiple matches


There are multiple ways to find all matches for a pattern in a text.

```powershell
#Sample text
$text = @"
This is (a) sample
text, this is
a (sample text)
"@

#Sample pattern: Content wrapped in ()
$pattern = '\(.*?\)'

```

### Using Select-String

You can find all matches (global match) by adding the `-AllMatches` switch to `Select-String`.

```powershell
> $m = Select-String -InputObject $text -Pattern $pattern -AllMatches

> $m | Format-List *

IgnoreCase : True
LineNumber : 1
Line       : This is (a) sample
             text, this is
             a (sample text)
Filename   : InputStream
Path       : InputStream
Pattern    : \(.*?\)
Context    : 
Matches    : {(a), (sample text)}

#List all matches
> $m.Matches

Groups   : {(a)}
Success  : True
Captures : {(a)}
Index    : 8
Length   : 3
Value    : (a)

Groups   : {(sample text)}
Success  : True
Captures : {(sample text)}
Index    : 37
Length   : 13
Value    : (sample text)

#Get matched text
> $m.Matches | Select-Object -ExpandProperty Value
(a)
(sample text)

```

### Using [RegEx]::Matches()

The `Matches()` method in the .NET `[regex]-class can also be used to do a global search for multiple matches.

```powershell
> [regex]::Matches($text,$pattern)

Groups   : {(a)}
Success  : True
Captures : {(a)}
Index    : 8
Length   : 3
Value    : (a)

Groups   : {(sample text)}
Success  : True
Captures : {(sample text)}
Index    : 37
Length   : 13
Value    : (sample text)

> [regex]::Matches($text,$pattern) | Select-Object -ExpandProperty Value

(a)
(sample text)

```



#### Syntax


- 'text' -match 'RegExPattern'
- 'text' -replace 'RegExPattern', 'newvalue'
- [regex]::Match("text","pattern")    #Single match
- [regex]::Matches("text","pattern")    #Multiple matches
- [regex]::Replace("text","pattern","newvalue")
- [regex]::Replace("text","pattern", {param($m) }) #MatchEvaluator
- [regex]::Escape("input") #Escape special characters

