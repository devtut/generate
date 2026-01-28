---
metaTitle: "Excel VBA - Debugging and Troubleshooting"
description: "Immediate Window, Debug.Print, Use Timer to Find Bottlenecks in Performance, Debugger Locals Window, Stop, Adding a Breakpoint to your code"
---

# Debugging and Troubleshooting



## Immediate Window


If you would like to test a line of macro code without needing to run an entire sub, you can type commands directly into the Immediate Window and hit `ENTER` to run the line.

For testing the output of a line, you can precede it with a question mark `?` to print directly to the Immediate Window.  Alternatively, you can also use the `print` command to have the output printed.

While in the Visual Basic Editor, press `CTRL + G` to open the Immediate Window. To rename your currently selected sheet to "ExampleSheet", type the following in the Immediate Window and hit `ENTER`

```

  ActiveSheet.Name = "ExampleSheet"

```

To print the currently selected sheet's name directly in the Immediate Window

```vb
? ActiveSheet.Name
ExampleSheet

```

This method can be very useful to test the functionality of built in or user defined functions before implementing them in code.  The example below demonstrates how the Immediate Window can be used to test the output of a function or series of functions to confirm an expected.

```vb
'In this example, the Immediate Window was used to confirm that a series of Left and Right 
'string methods would return the desired string

'expected output: "value"
print Left(Right("1111value1111",9),5) ' <---- written code here, ENTER pressed
value                                  ' <---- output

```

The Immediate Window can also be used to set or reset Application, Workbook, or other needed properties.  This can be useful if you have `Application.EnableEvents = False` in a subroutine that unexpectedly throws an error, causing it to close without resetting the value to `True` (which can cause frustrating and unexpected functionality.  In that case, the commands can be typed directly into the Immediate Window and run:

```vb
? Application.EnableEvents       ' <---- Testing the current state of "EnableEvents"
False                            ' <---- Output
Application.EnableEvents = True  ' <---- Resetting the property value to True
? Application.EnableEvents       ' <---- Testing the current state of "EnableEvents"
True                             ' <---- Output

```

For more advanced debugging techniques, a colon `:` can be used as a line separator. This can be used for multi-line expressions such as looping in the example below.

```vb
x = Split("a,b,c",","): For i = LBound(x,1) to UBound(x,1): Debug.Print x(i): Next i '<----Input this and press enter
a '<----Output
b '<----Output
c '<----Output

```



## Debug.Print


To print a listing of the Error Code descriptions to the Immediate Window, pass it to the `Debug.Print` function:

```vb
Private Sub ListErrCodes()
    Debug.Print "List Error Code Descriptions"
    For i = 0 To 65535
        e = Error(i)
        If e <> "Application-defined or object-defined error" Then Debug.Print i & ": " & e
    Next i
End Sub

```

You can show the Immediate Window by:

- Selecting **V**iew | **I**mmediate Window from the menu bar
- Using the keyboard shortcut **Ctrl-G**



## Use Timer to Find Bottlenecks in Performance


The first step in optimizing for speed is finding the slowest sections of code. The `Timer` VBA function returns the number of seconds elapsed since midnight with a precision of 1/256th of a second (3.90625 milliseconds) on Windows based PCs. The VBA functions `Now` and `Time` are only accurate to a second.

```vb
Dim start As Double       ' Timer returns Single, but converting to Double to avoid 
start = Timer             ' scientific notation like 3.90625E-03 in the Immediate window
' ... part of the code
Debug.Print Timer - start; "seconds in part 1" 

start = Timer
' ... another part of the code
Debug.Print Timer - start; "seconds in part 2"

```



## Debugger Locals Window


The Locals window provides easy access to the current value of variables and objects within the scope of the function or subroutine you are running. It is an essential tool to debugging your code and stepping through changes in order to find issues. It also allows you to explore properties you might not have known existed.

Take the following example,

```vb
Option Explicit
Sub LocalsWindowExample()
    Dim findMeInLocals As Integer
    Dim findMEInLocals2 As Range
    
    findMeInLocals = 1
    Set findMEInLocals2 = ActiveWorkbook.Sheets(1).Range("A1")
End Sub

```

In the VBA Editor, click View  --> Locals Window

[<img src="http://i.stack.imgur.com/rT6Wf.png" alt="Where locals menu is at on the window" />](http://i.stack.imgur.com/rT6Wf.png)

Then by stepping through the code using F8 after clicking inside the subroutine, we have stopped before getting to assigning findMeinLocals. Below you can see the value is 0 --- and this is what would be used if you never assigned it a value. The range object is 'Nothing'.

[<img src="http://i.stack.imgur.com/B8vZt.png" alt="At this step the variables are nothing..." />](http://i.stack.imgur.com/B8vZt.png)

[<img src="http://i.stack.imgur.com/8smT0.png" alt="Showing the variables are nothing." />](http://i.stack.imgur.com/8smT0.png)

If we stop right before the subroutine ends, we can see the final values of the variables.

[<img src="http://i.stack.imgur.com/VE12u.png" alt="Set a debug point... if you want" />](http://i.stack.imgur.com/VE12u.png)

We can see findMeInLocals with a value of 1 and type of Integer, and FindMeInLocals2 with a type of Range/Range. If we click the + sign we can expand the object and see its properties, such as count or column.

[<img src="http://i.stack.imgur.com/m7Rdu.png" alt="enter image description here" />](http://i.stack.imgur.com/m7Rdu.png)



## Stop


The Stop command will pause the execution when called. From there, the process can be resumed or be executed step by step.

```vb
Sub Test()
    Dim TestVar as String
    TestVar = "Hello World"
    Stop                    'Sub will be executed to this point and then wait for the user
    MsgBox TestVar
End Sub

```



## Adding a Breakpoint to your code


You can easily add a breakpoint to your code by clicking on the grey column to the left of the line of your VBA code where you want execution to stop. A red dot appears in the column and the breakpoint code is also highlighted in red.

You can add multiple breakpoints throughout your code and resuming execution is achieved by pressing the "play" icon in your menu bar.  Not all code can be a breakpoint as variable definition lines, the first or last line of a procedure and comment lines cannot be selected as a breakpoint.

[<img src="http://i.stack.imgur.com/AJUpD.jpg" alt="enter image description here" />](http://i.stack.imgur.com/AJUpD.jpg)



#### Syntax


- Debug.Print(string)
- Stop() / Stop

