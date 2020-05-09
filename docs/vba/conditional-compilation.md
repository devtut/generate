---
metaTitle: "VBA - Conditional Compilation"
description: "Changing code behavior at compile time, Using Declare Imports that work on all versions of Office"
---

# Conditional Compilation



## Changing code behavior at compile time


The `#Const` directive is used to define a custom preprocessor constant. These can later be used by `#If` to control which blocks of code get compiled and executed.

```vb
#Const DEBUGMODE = 1

#If DEBUGMODE Then
    Const filepath As String = "C:\Users\UserName\Path\To\File.txt"
#Else
    Const filepath As String = "\\server\share\path\to\file.txt"
#End If

```

This results in the value of `filepath` being set to `"C:\Users\UserName\Path\To\File.txt"`. Removing the `#Const` line, or changing it to `#Const DEBUGMODE = 0` would result in the `filepath` being set to `"\\server\share\path\to\file.txt"`.

**#Const Scope**

The `#Const` directive is only effective for a single code file (module or class). It must be declared for each and every file you wish to use your custom constant in. Alternatively, you can declare a `#Const` globally for your project by going to Tools >> [Your Project Name] Project Properties. This will bring up the project properties dialog box where we’ll enter the constant declaration. In the “Conditional Compilation Arguments” box, type in `[constName] = [value]`. You can enter more than 1 constant by separating them with a colon, like `[constName1] = [value1] : [constName2] = [value2]`.

[<img src="http://i.stack.imgur.com/rEY6K.png" alt="VBA Project Properties Dialog" />](http://i.stack.imgur.com/rEY6K.png)

**Pre-defined Constants**

Some compilation constants are already pre-defined. Which ones exist will depend on the bitness of the office version you're running VBA in. Note that Vba7 was introduced alongside Office 2010 to support 64 bit versions of Office.

|Constant|16 bit|32 bit|64 bit
|---|---|---|---|---|---|---|---|---|---
|Vba6|False|If Vba6|False
|Vba7|False|If Vba7|True
|Win16|True|False|False
|Win32|False|True|True
|Win64|False|False|True
|Mac|False|If Mac|If Mac

Note that Win64/Win32 refer to the Office version, not the Windows version.
For example Win32 = TRUE in 32-bit Office, even if the OS is a 64-bit version of Windows.



## Using Declare Imports that work on all versions of Office


```vb
#If Vba7 Then
    ' It's important to check for Win64 first, 
    ' because Win32 will also return true when Win64 does.

    #If Win64 Then
        Declare PtrSafe Function GetFoo64 Lib "exampleLib32" () As LongLong
    #Else
        Declare PtrSafe Function GetFoo Lib "exampleLib32" () As Long
    #End If
#Else 
    ' Must be Vba6, the PtrSafe keyword didn't exist back then,
    ' so we need to declare Win32 imports a bit differently than above.

    #If Win32 Then
        Declare Function GetFoo Lib "exampleLib32"() As Long
    #Else
        Declare Function GetFoo Lib "exampleLib"() As Integer
    #End If
#End If

```

This can be simplified a bit depending on what versions of office you need to support. For example, not many people are still supporting 16 bit versions of Office. [The last version of 16 bit office was version 4.3, released in 1994](https://en.wikipedia.org/wiki/History_of_Microsoft_Office), so the following declaration is sufficient for nearly all modern cases (including Office 2007).

```vb
#If Vba7 Then
    ' It's important to check for Win64 first, 
    ' because Win32 will also return true when Win64 does.

    #If Win64 Then
        Declare PtrSafe Function GetFoo64 Lib "exampleLib32" () As LongLong
    #Else
        Declare PtrSafe Function GetFoo Lib "exampleLib32" () As Long
    #End If
#Else 
    ' Must be Vba6. We don't support 16 bit office, so must be Win32. 

    Declare Function GetFoo Lib "exampleLib32"() As Long
#End If

```

If you don't have to support anything older than Office 2010, this declaration works just fine.

```vb
' We only have 2010 installs, so we already know we have Vba7.

#If Win64 Then
    Declare PtrSafe Function GetFoo64 Lib "exampleLib32" () As LongLong
#Else
    Declare PtrSafe Function GetFoo Lib "exampleLib32" () As Long
#End If

```

