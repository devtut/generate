---
metaTitle: "Excel VBA - Getting started with excel-vba"
description: "Opening the Visual Basic Editor (VBE), Declaring Variables, Adding a new Object Library Reference, Hello World, Getting Started with the Excel Object Model"
---

# Getting started with excel-vba



## Opening the Visual Basic Editor (VBE)


**Step 1: Open a Workbook**

[<img src="https://i.stack.imgur.com/MHMA9.png" alt="enter image description here" />](https://i.stack.imgur.com/MHMA9.png)

**Step 2 Option A: Press <kbd>Alt</kbd> + <kbd>F11</kbd>**

This is the standard shortcut to open the VBE.

**Step 2 Option B: Developer Tab --> View Code**

First, the Developer Tab must be added to the ribbon.  Go to File -> Options -> Customize Ribbon, then check the box for developer.

[<img src="https://i.stack.imgur.com/8WoiR.png" alt="enter image description here" />](https://i.stack.imgur.com/8WoiR.png)

Then, go to the developer tab and click "View Code" or "Visual Basic"

[<img src="https://i.stack.imgur.com/388eU.png" alt="enter image description here" />](https://i.stack.imgur.com/388eU.png)

**Step 2 Option C: View tab > Macros > Click Edit button to open an Existing Macro**

All three of these options will open the Visual Basic Editor (VBE):

[<img src="https://i.stack.imgur.com/azT5a.png" alt="enter image description here" />](https://i.stack.imgur.com/azT5a.png)



## Declaring Variables


To explicitly declare variables in VBA, use the `Dim` statement, followed by the variable name and type. If a variable is used without being declared, or if no type is specified, it will be assigned the type `Variant`.

Use the `Option Explicit` statement on first line of a module to force all variables to be declared before usage (see [ALWAYS Use "Option Explicit"](http://stackoverflow.com/documentation/excel-vba/1107/vba-best-practices/3554/always-use-option-explicit#t=201607231503377559602) ).

Always using `Option Explicit` is highly recommended because it helps prevent typo/spelling errors and ensures variables/objects will stay their intended type.

```vb
Option Explicit

Sub Example()
    Dim a As Integer
    a = 2
    Debug.Print a
    'Outputs: 2

    Dim b As Long
    b = a + 2
    Debug.Print b
    'Outputs: 4

    Dim c As String
    c = "Hello, world!"
    Debug.Print c
    'Outputs: Hello, world!
End Sub

```

Multiple variables can be declared on a single line using commas as delimiters, but **each type must be declared individually**, or they will default to the `Variant` type.

```vb
Dim Str As String, IntOne, IntTwo As Integer, Lng As Long
Debug.Print TypeName(Str)    'Output: String
Debug.Print TypeName(IntOne) 'Output: Variant <--- !!!
Debug.Print TypeName(IntTwo) 'Output: Integer
Debug.Print TypeName(Lng)    'Output: Long

```

Variables can also be declared using Data Type Character suffixes ($ % & ! # @), however using these are increasingly discouraged.

```

Dim this$  'String
 Dim this%  'Integer
 Dim this&  'Long
 Dim this!  'Single
 Dim this#  'Double
 Dim this@  'Currency

```

### Other ways of declaring variables are:

- `Static` like: `Static CounterVariable as Integer`

> 
When you use the Static statement instead of a Dim statement, the declared variable will retain its value between calls.


- `Public` like: `Public CounterVariable as Integer`

> 
Public variables can be used in any procedures in the project. If a public variable is declared in a standard module or a class module, it can also be used in any projects that reference the project where the public variable is declared.


- `Private` like: `Private CounterVariable as Integer`

> 
Private variables can be used only by procedures in the same module.


Source and more info:

[MSDN-Declaring Variables](https://msdn.microsoft.com/en-us/library/office/gg264241.aspx)

[Type Characters (Visual Basic)](https://docs.microsoft.com/en-us/dotnet/visual-basic/programming-guide/language-features/data-types/type-characters)



## Adding a new Object Library Reference


The procedure describes how to add an Object library reference, and afterwards how to declare new variables with reference to the new library class objects.

The example below shows how to add the **PowerPoint** library to the existing VB Project.
As can be seen, currently the PowerPoint Object library is not available.

[<img src="http://i.stack.imgur.com/0IwJy.jpg" alt="enter image description here" />](http://i.stack.imgur.com/0IwJy.jpg)

**Step 1**: Select Menu ****Tools**** **-->** ****References…****
[<img src="http://i.stack.imgur.com/yfb7J.jpg" alt="enter image description here" />](http://i.stack.imgur.com/yfb7J.jpg)

**Step 2**: Select the Reference you want to add. This example we scroll down to find “****Microsoft PowerPoint 14.0 Object Library****”, and then press “**OK**”.
[<img src="http://i.stack.imgur.com/vsKbO.jpg" alt="enter image description here" />](http://i.stack.imgur.com/vsKbO.jpg)

Note: PowerPoint 14.0 means that Office 2010 version is installed on the PC.

**Step 3**: in the VB Editor, once you press **Ctrl+Space** together, you get the autocomplete option of PowerPoint.
[<img src="http://i.stack.imgur.com/6DoDc.jpg" alt="enter image description here" />](http://i.stack.imgur.com/6DoDc.jpg)

After selecting `PowerPoint` and pressing `.`, another menu appears with all objects options related to the PowerPoint Object Library.
This example shows how to select the PowerPoint's object `Application`.
[<img src="http://i.stack.imgur.com/Av3V7.jpg" alt="enter image description here" />](http://i.stack.imgur.com/Av3V7.jpg)

**Step 4**: Now the user can declare more variables using the PowerPoint object library.

Declare a variable that is referencing the `Presentation` object of the PowerPoint object library.
[<img src="http://i.stack.imgur.com/dzCOc.jpg" alt="enter image description here" />](http://i.stack.imgur.com/dzCOc.jpg)

Declare another variable that is referencing the `Slide` object of the PowerPoint object library.
[<img src="http://i.stack.imgur.com/QARnI.jpg" alt="enter image description here" />](http://i.stack.imgur.com/QARnI.jpg)

Now the variables declaration section looks like in the screen-shot below, and the user can start using these variables in his code.
[<img src="http://i.stack.imgur.com/bfQff.jpg" alt="enter image description here" />](http://i.stack.imgur.com/bfQff.jpg)

Code version of this tutorial:

```vb
Option Explicit

Sub Export_toPPT()

Dim ppApp As PowerPoint.Application
Dim ppPres As PowerPoint.Presentation
Dim ppSlide As PowerPoint.Slide

' here write down everything you want to do with the PowerPoint Class and objects


End Sub

```



## Hello World


1. Open the Visual Basic Editor ( see  [Opening the Visual Basic Editor](http://stackoverflow.com/documentation/excel-vba/777/introduction-to-excel-vba/2801/opening-the-visual-basic-editor#t=201607252035449856939) )
1. Click Insert --> Module to add a new Module :

[<img src="http://i.stack.imgur.com/0KhKM.png" alt="enter image description here" />](http://i.stack.imgur.com/0KhKM.png)

1. Copy and Paste the following code in the new module :

```

 Sub hello()
    MsgBox "Hello World !"
  End Sub

```

To obtain :

[<img src="http://i.stack.imgur.com/wv7kE.png" alt="enter image description here" />](http://i.stack.imgur.com/wv7kE.png)

<li>
<p>Click on the green “play” arrow (or press F5)  in the Visual Basic toolbar to run the  program:
[<img src="http://i.stack.imgur.com/aFU8E.png" alt="enter image description here" />](http://i.stack.imgur.com/aFU8E.png)</p>
</li>
<li>
<p>Select the new created sub "hello" and click `Run` :
[<img src="http://i.stack.imgur.com/Mcj1X.png" alt="enter image description here" />](http://i.stack.imgur.com/Mcj1X.png)</p>
</li>
<li>
Done, your should see the following window:
</li>

[<img src="http://i.stack.imgur.com/j88GC.png" alt="enter image description here" />](http://i.stack.imgur.com/j88GC.png)



## Getting Started with the Excel Object Model


> 
This example intend to be a gentle introduction to the Excel Object Model **for beginners**.


1. Open the Visual Basic Editor (VBE)
1. Click View --> Immediate Window to open the Immediate Window (or <kbd>ctrl</kbd> + <kbd>G</kbd>):

[<img src="https://i.stack.imgur.com/I57Nk.png" alt="enter image description here" />](https://i.stack.imgur.com/I57Nk.png)

1. You should see the following Immediate Window at the bottom on VBE:

[<img src="https://i.stack.imgur.com/msMIR.png" alt="enter image description here" />](https://i.stack.imgur.com/msMIR.png)

This window allow you to directly test some VBA code.  So let's start, type in this console :

```vb
?Worksheets. 

```

VBE has intellisense and then it should open a tooltip as in the following figure :

[<img src="https://i.stack.imgur.com/f1i7c.png" alt="enter image description here" />](https://i.stack.imgur.com/f1i7c.png)

Select .Count in the list or directly type `.Cout` to obtain :

```vb
?Worksheets.Count

```


1. Then press Enter. The expression is evaluated and it should returns 1.  This indicates the number of Worksheet currently present in the workbook. The question mark (`?`) is an alias for Debug.Print.

Worksheets is an **Object** and Count is a **Method**. Excel has several Object (`Workbook`, `Worksheet`, `Range`, `Chart` ..) and each of one contains specific methods and properties. You can find the complete list of Object in the [Excel VBA reference](https://msdn.microsoft.com/en-us/library/ff194068.aspx). Worksheets Object is presented [here](https://msdn.microsoft.com/en-us/library/ff821537.aspx) .

> 
This Excel VBA reference should become your primary source of information regarding the Excel Object Model.


1. Now let's try another expression, type (without the `?` character):

```vb
Worksheets.Add().Name = "StackOveflow"

```


1. Press Enter. This should create a new worksheet called `StackOverflow.`:

[<img src="https://i.stack.imgur.com/7YbHr.png" alt="enter image description here" />](https://i.stack.imgur.com/7YbHr.png)

To understand this expression you need to read the Add function in the aforementioned Excel reference. You will find the following:

```vb
Add:  Creates a new worksheet, chart, or macro sheet. 
The new worksheet becomes the active sheet. 
Return Value: An Object value that represents the new worksheet, chart,
 or macro sheet.

```

So the `Worksheets.Add()` create a new worksheet and return it. Worksheet(**without s**) is itself a Object that [can be found](https://msdn.microsoft.com/en-us/library/ff194464.aspx) in the documentation and  `Name`  is one of its **property** (see [here](https://msdn.microsoft.com/en-us/library/ff841127.aspx)). It is defined as :

```vb
Worksheet.Name Property:  Returns or sets a String value that 
 represents the object name.

```

So, by investigating the different objects definitions we are able to understand this code `Worksheets.Add().Name = "StackOveflow"`.

`Add()` creates and add a new worksheet and return a **reference** to it, then we set its Name **property** to "StackOverflow"

Now let's be more formal, Excel contains several Objects. These Objects may be composed of one or several collection(s) of Excel objects of the same class.  It is the case for `WorkSheets` which is a collection of  `Worksheet` object. Each Object has some properties and methods that the programmer can interact with.

> 
The Excel Object model refers to the Excel **object hierarchy**


At the top of all objects is the `Application` object, it represents the Excel instance itself. Programming in VBA  requires a good understanding of this hierarchy because we always need a reference to an object to be able to call a Method or to Set/Get a property.

The (very simplified) Excel Object Model can be represented as,

```

                           Application
                             Workbooks
                             Workbook
                            Worksheets
                             Worksheet
                              Range

```

A more detail version for the Worksheet Object (as it is in Excel 2007) is shown below,

[<img src="https://i.stack.imgur.com/3yhD8.png" alt="enter image description here" />](https://i.stack.imgur.com/3yhD8.png)

> 
The full Excel Object Model can be found [here](https://msdn.microsoft.com/en-us/library/ff194068(v=office.15).aspx).


Finally some objects may have  `events` (ex: `Workbook.WindowActivate`) that are also part of the Excel Object Model.



#### Remarks


Microsoft Excel includes a comprehensive macro programming language called VBA. This programming language provides you with at least three additional resources:

1. Automatically drive Excel from code using Macros. For the most part, anything that the user can do by manipulating Excel from the user interface can be done by writing code in Excel VBA.
1. Create new, custom worksheet functions.
1. Interact Excel with other applications such as Microsoft Word, PowerPoint, Internet Explorer, Notepad, etc.

VBA stands for Visual Basic for Applications. It is a custom version of the venerable Visual Basic programming language that has powered Microsoft Excel's macros since the mid-1990s.

**IMPORTANT**<br />
Please ensure any examples or topics created within the excel-vba tag are **specific** and **relevant** to the use of VBA with Microsoft Excel. Any suggested topics or examples provided that are generic to the VBA language should be declined in order to prevent duplication of efforts.

<li>
on-topic examples:
<p>✓ **Creating and interacting with worksheet objects**<br />
✓ **The `WorksheetFunction` class and respective methods**<br />
✓ **Using the `xlDirection` enumeration to navigate a range**</p>
</li>

<li>
off-topic examples:
<p>✗ **How to create a 'for each' loop**<br />
✗ **`MsgBox` class and how to display a message**<br />
✗ **Using WinAPI in VBA**</p>
</li>

