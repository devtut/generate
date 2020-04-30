---
metaTitle: "String Escape Sequences"
description: "Escaping special symbols in string literals, Unicode character escape sequences, Escaping special symbols in character literals, Using escape sequences in identifiers, Unrecognized escape sequences produce compile-time errors"
---

# String Escape Sequences




## Escaping special symbols in string literals


**Backslash**

```cs
// The filename will be c:\myfile.txt in both cases
string filename = "c:\\myfile.txt";
string filename = @"c:\myfile.txt";

```

The second example uses a [verbatim string literal](http://stackoverflow.com/documentation/c%23/16/verbatim-strings#t=20151122021216101385), which doesn't treat the backslash as an escape character.

**Quotes**

```cs
string text = "\"Hello World!\", said the quick brown fox.";
string verbatimText = @"""Hello World!"", said the quick brown fox.";

```

Both variables will contain the same text.

> 
"Hello World!", said the quick brown fox.


**Newlines**

Verbatim string literals can contain newlines:

```cs
string text = "Hello\r\nWorld!";
string verbatimText = @"Hello
World!";

```

Both variables will contain the same text.



## Unicode character escape sequences


```cs
string sqrt = "\u221A";      // √
string emoji = "\U0001F601"; // 😁
string text = "\u0022Hello World\u0022"; // "Hello World"
string variableWidth = "\x22Hello World\x22"; // "Hello World"

```



## Escaping special symbols in character literals


**Apostrophes**

```cs
char apostrophe = '\'';

```

**Backslash**

```cs
char oneBackslash = '\\';

```



## Using escape sequences in identifiers


Escape sequences are not restricted to `string` and `char` literals.

Suppose you need to override a third-party method:

```cs
protected abstract IEnumerable<Texte> ObtenirŒuvres();

```

and suppose the character `Œ` is not available in the character encoding you use for your C# source files. You are lucky, it is permitted to use escapes of the type `\u####` or `\U########` in ****identifiers**** in the code. So it is legal to write:

```cs
protected override IEnumerable<Texte> Obtenir\u0152uvres()
{
    // ...
}

```

and the C# compiler will know `Œ` and `\u0152` are the same character.

(However, it might be a good idea to switch to UTF-8 or a similar encoding that can handle all characters.)



## Unrecognized escape sequences produce compile-time errors


The following examples will not compile:

```cs
string s = "\c";
char c = '\c';

```

Instead, they will produce the error `Unrecognized escape sequence` at compile time.



#### Syntax


- \' — single quote (0x0027)
- \" — double quote (0x0022)
- \\ — backslash (0x005C)
- \0 — null (0x0000)
- \a — alert (0x0007)
- \b — backspace (0x0008)
- \f — form feed (0x000C)
- \n — new line (0x000A)
- \r — carriage return (0x000D)
- \t — horizontal tab (0x0009)
- \v — vertical tab (0x000B)
- \u0000 - \uFFFF — Unicode character
- \x0 - \xFFFF — Unicode character (code with variable length)
- \U00000000 - \U0010FFFF — Unicode character (for generating surrogates)



#### Remarks


String escape sequences are transformed to the corresponding character at **compile time**. Ordinary strings that happen to contain backwards slashes are **not** transformed.

For example, the strings `notEscaped` and `notEscaped2` below are not transformed to a newline character, but will stay as two different characters (`'\'` and `'n'`).

```cs
string escaped = "\n";
string notEscaped = "\\" + "n";
string notEscaped2 = "\\n";

Console.WriteLine(escaped.Length); // 1
Console.WriteLine(notEscaped.Length); // 2            
Console.WriteLine(notEscaped2.Length); // 2

```

