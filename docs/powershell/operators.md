---
metaTitle: "PowerShell - Operators"
description: "Comparison Operators, Arithmetic Operators, Assignment Operators, Redirection Operators, Mixing operand types : the type of the left operand dictates the behavior., Logical Operators, String Manipulation Operators"
---

# Operators


An operator is a character that represents an action. It tells the compiler/interpreter to perform specific mathematical, relational or logical operation and produce final result. PowerShell interprets in a specific way and categorizes accordingly like arithmetic operators perform operations primarily on numbers, but they also affect strings and other data types. Along with the basic operators,PowerShell has a number of operators that save time and coding effort(eg: -like,-match,-replace,etc).



## Comparison Operators


PowerShell comparison operators are comprised of a leading dash (`-`) followed by a name (`eq` for `equal`, `gt` for `greater than`, etc...).

Names can be preceded by special characters to modify the behavior of the operator:

```powershell
i # Case-Insensitive Explicit (-ieq)
c # Case-Sensitive Explicit (-ceq)

```

Case-Insensitive is the default if not specified, ("a" -eq "A") same as ("a" -ieq "A").

Simple comparison operators:

```powershell
2 -eq 2    # Equal to (==)
2 -ne 4    # Not equal to (!=)
5 -gt 2    # Greater-than (>)
5 -ge 5    # Greater-than or equal to (>=)
5 -lt 10   # Less-than (<)
5 -le 5    # Less-than or equal to (<=)

```

String comparison operators:

```powershell
"MyString" -like "*String"            # Match using the wildcard character (*)
"MyString" -notlike "Other*"          # Does not match using the wildcard character (*)
"MyString" -match "$String^"          # Matches a string using regular expressions
"MyString" -notmatch "$Other^"        # Does not match a string using regular expressions

```

Collection comparison operators:

```powershell
"abc", "def" -contains "def"            # Returns true when the value (right) is present
                                        # in the array (left)
"abc", "def" -notcontains "123"         # Returns true when the value (right) is not present
                                        # in the array (left)
"def" -in "abc", "def"                  # Returns true when the value (left) is present
                                        # in the array (right)
"123" -notin "abc", "def"               # Returns true when the value (left) is not present
                                        # in the array (right)

```



## Arithmetic Operators


```powershell
1 + 2      # Addition
1 - 2      # Subtraction
-1         # Set negative value
1 * 2      # Multiplication
1 / 2      # Division
1 % 2      # Modulus
100 -shl 2 # Bitwise Shift-left
100 -shr 1 # Bitwise Shift-right

```



## Assignment Operators


Simple arithmetic:

```powershell
$var = 1      # Assignment. Sets the value of a variable to the specified value
$var += 2     # Addition. Increases the value of a variable by the specified value
$var -= 1     # Subtraction. Decreases the value of a variable by the specified value
$var *= 2     # Multiplication. Multiplies the value of a variable by the specified value
$var /= 2     # Division. Divides the value of a variable by the specified value
$var %= 2     # Modulus. Divides the value of a variable by the specified value and then
              # assigns the remainder (modulus) to the variable

```

Increment and decrement:

```powershell
$var++   # Increases the value of a variable, assignable property, or array element by 1
$var--   # Decreases the value of a variable, assignable property, or array element by 1

```



## Redirection Operators


Success output stream:

```powershell
cmdlet > file     # Send success output to file, overwriting existing content
cmdlet >> file    # Send success output to file, appending to existing content
cmdlet 1>&2       # Send success and error output to error stream

```

Error output stream:

```powershell
cmdlet 2> file    # Send error output to file, overwriting existing content
cmdlet 2>> file   # Send error output to file, appending to existing content
cmdlet 2>&1       # Send success and error output to success output stream

```

Warning output stream: (PowerShell 3.0+)

```powershell
cmdlet 3> file    # Send warning output to file, overwriting existing content
cmdlet 3>> file   # Send warning output to file, appending to existing content
cmdlet 3>&1       # Send success and warning output to success output stream

```

Verbose output stream: (PowerShell 3.0+)

```powershell
cmdlet 4> file    # Send verbose output to file, overwriting existing content
cmdlet 4>> file   # Send verbose output to file, appending to existing content
cmdlet 4>&1       # Send success and verbose output to success output stream

```

Debug output stream: (PowerShell 3.0+)

```powershell
cmdlet 5> file    # Send debug output to file, overwriting existing content
cmdlet 5>> file   # Send debug output to file, appending to existing content
cmdlet 5>&1       # Send success and debug output to success output stream

```

Information output stream: (PowerShell 5.0+)

```powershell
cmdlet 6> file    # Send information output to file, overwriting existing content
cmdlet 6>> file   # Send information output to file, appending to existing content
cmdlet 6>&1       # Send success and information output to success output stream 

```

All output streams:

```powershell
cmdlet *> file    # Send all output streams to file, overwriting existing content
cmdlet *>> file   # Send all output streams to file, appending to existing content
cmdlet *>&1       # Send all output streams to success output stream

```

Differences to the pipe operator (`|`)

Redirection operators only redirect streams to files or streams to streams. The pipe operator pumps an object down the pipeline to a cmdlet or the output. How the pipeline works differs in general from how redirection works and can be read on [Working with the PowerShell pipeline](http://stackoverflow.com/documentation/powershell/3937/working-with-the-powershell-pipeline#t=201704260811349712597)



## Mixing operand types : the type of the left operand dictates the behavior.


**For Addition**

```powershell
"4" + 2         # Gives "42"
4 + "2"         # Gives 6
1,2,3 + "Hello" # Gives 1,2,3,"Hello"
"Hello" + 1,2,3 # Gives "Hello1 2 3"

```

**For Multiplication**

```powershell
"3" * 2   # Gives "33"
2 * "3"   # Gives 6
1,2,3 * 2 # Gives 1,2,3,1,2,3
2 * 1,2,3 # Gives an error op_Multiply is missing

```

The impact may have hidden consequences on comparison operators :

```powershell
$a = Read-Host "Enter a number"
Enter a number : 33
$a -gt 5
False

```



## Logical Operators


```powershell
-and # Logical and
-or  # Logical or
-xor # Logical exclusive or
-not # Logical not
!    # Logical not

```



## String Manipulation Operators


Replace operator:

The `-replace` operator replaces a pattern in an input value using a regular expression. This operator uses two arguments (separated by a comma): a regular expression pattern and its replacement value (which is optional and an empty string by default).

```powershell
"The rain in Seattle" -replace 'rain','hail'        #Returns: The hail in Seattle
"kenmyer@contoso.com" -replace '^[\w]+@(.+)', '$1'  #Returns: contoso.com

```

Split and Join operators:

The `-split` operator splits a string into an array of sub-strings.

```powershell
"A B C" -split " "      #Returns an array string collection object containing A,B and C.

```

The `-join` operator joins an array of strings into a single string.

```powershell
"E","F","G" -join ":"   #Returns a single string: E:F:G

```

