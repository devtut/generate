---
metaTitle: "PowerShell - Strings"
description: "Creating a basic string, Format string, Multiline string, Here-string, Concatenating strings, Special characters"
---

# Strings



## Creating a basic string


### String

Strings are created by wrapping the text with double quotes. Double-quoted strings can evalute variables and special characters.

```powershell
$myString = "Some basic text"
$mySecondString = "String with a $variable"

```

To use a double quote inside a string it needs to be escaped using the escape character, backtick (```powershell). Single quotes can be used inside a double-quoted string.

```powershell
$myString = "A `"double quoted`" string which also has 'single quotes'."

```

### Literal string

Literal strings are strings that doesn't evaluate variables and special characters. It's created using single quotes.

```powershell
$myLiteralString = 'Simple text including special characters (`n) and a $variable-reference'

```

To use single quotes inside a literal string, use double single quotes or a literal here-string. Double qutoes can be used safely inside a literal string

```powershell
$myLiteralString = 'Simple string with ''single quotes'' and "double quotes".'

```



## Format string


```powershell
$hash = @{ city = 'Berlin' }

$result = 'You should really visit {0}' -f $hash.city
Write-Host $result #prints "You should really visit Berlin"

```

Format strings can be used with the `-f` operator or the static `[String]::Format(string format, args)` .NET method.



## Multiline string


There are multiple ways to create a multiline string in PowerShell:

<li>
You can use the special characters for carriage return and/or newline manually or use the `NewLine`-environment variable to insert the systems "newline" value)

```powershell
"Hello`r`nWorld"
"Hello{0}World" -f [environment]::NewLine

```


</li>
<li>
Create a linebreak while defining a string (before closing quote)

```powershell
"Hello
World"

```


</li>
<li>
Using a here-string. **This is the most common technique.**

```powershell
@"
Hello
World
"@

```


</li>



## Here-string


Here-strings are very useful when creating multiline strings. One of the biggest benefits compared to other multiline strings are that you can use quotes without having to escape them using a backtick.

### Here-string

Here-strings begin with `@"` and a linebreak and end with `"@` on it's own line (**`"@`must be first characters on the line, not even whitespace/tab**).

```powershell
@"
Simple
    Multiline string 
with "quotes"
"@

```

### Literal here-string

You could also create a literal here-string by using single quotes, when you don't want any expressions to be expanded just like a normal literal string.

```powershell
@'
The following line won't be expanded
$(Get-Date)
because this is a literal here-string
'@

```



## Concatenating strings


### Using variables in a string

You can concatenate strings using variables inside a double-quoted string. This does not work with properties.

```powershell
$string1 = "Power"
$string2 = "Shell"
"Greetings from $string1$string2"

```

### Using the `+` operator

You can also join strings using the `+` operator.

```powershell
$string1 = "Greetings from"
$string2 = "PowerShell"
$string1 + " " + $string2

```

This also works with properties of objects.

```powershell
"The title of this console is '" + $host.Name + "'"

```

### Using subexpressions

The output/result of a subexpressions `$()` can be used in a string. This is useful when accessing propeties of an object or performing a complex expression. Subexpressions can contain multiple statements separated by semicolon `;`

```powershell
"Tomorrow is $((Get-Date).AddDays(1).DayOfWeek)"

```



## Special characters


When used inside a double-quoted string, the escape character (backtick ```powershell) reperesents a special character.

```powershell
`0    #Null
`a    #Alert/Beep
`b    #Backspace
`f    #Form feed (used for printer output)
`n    #New line
`r    #Carriage return
`t    #Horizontal tab
`v    #Vertical tab (used for printer output)

```

Example:

```powershell
> "This`tuses`ttab`r`nThis is on a second line"
This    uses    tab
This is on a second line

```

You can also escape special characters with special meanings:

```powershell
`#    #Comment-operator
`$    #Variable operator
``    #Escape character
`'    #Single quote
`"    #Double quote

```



#### Syntax


<li>
"(Double-quoted) String"
</li>
<li>
'Literal string'
</li>
<li>
<p>@"<br />
Here-string<br />
"@</p>
</li>
<li>
<p>@'<br />
Literal here-string<br />
'@</p>
</li>



#### Remarks


Strings are objects representing text.

