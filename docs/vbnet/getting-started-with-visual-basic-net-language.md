---
metaTitle: "Visual Basic .NET - Getting started with Visual Basic .NET Language"
description: "Hello World, Hello World on a Textbox upon Clicking of a Button, Region, Creating a simple Calculator to get familiar with the interface and code."
---

# Getting started with Visual Basic .NET Language



## Hello World


First, install a version of [Microsoft Visual Studio](https://www.visualstudio.com/downloads/download-visual-studio-vs), including the free Community edition. Then, create a Visual Basic Console Application project of type **Console Application**, and the following code will print the string `'Hello World'` to the Console:

```vb
Module Module1

    Sub Main()
        Console.WriteLine("Hello World")
    End Sub

End Module

```

Then, save and press <kbd>F5</kbd> on the keyboard (or go to the **Debug** menu, then click **Run without Debug** or **Run**) to compile and run the program. `'Hello World'` should appear in the console window.

[<img src="http://i.stack.imgur.com/rZcqG.png" alt="Output window, showing the Hello World." />](http://i.stack.imgur.com/rZcqG.png)



## Hello World on a Textbox upon Clicking of a Button


Drag 1 textbox and 1 button

[<img src="http://i.stack.imgur.com/sgFeW.jpg" alt="enter image description here" />](http://i.stack.imgur.com/sgFeW.jpg)

Double click the button1 and you will be transferred to the `Button1_Click event`

```vb
Public Class Form1
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

    End Sub
End Class

```

Type the name of the object that you want to target, in our case it is the `textbox1`. `.Text` is the property that we want to use if we want to put a text on it.

`Property Textbox.Text, gets or sets the current text in the TextBox`. Now, we have `Textbox1.Text`

We need to set the value of that `Textbox1.Text` so we will use the `=` sign. The value that we want to put in the `Textbox1.Text` is `Hello World`. Overall, this is the total code for putting a value of `Hello World` to the `Textbox1.Text`

```vb
TextBox1.Text = "Hello World"

```

Adding that code to the `clicked event` of `button1`

```vb
Public Class Form1
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        TextBox1.Text = "Hello World"
    End Sub
End Class

```

[<img src="http://i.stack.imgur.com/axKMb.jpg" alt="enter image description here" />](http://i.stack.imgur.com/axKMb.jpg)



## Region


For the sake of readability, which will be useful for beginners when reading VB code as well for full time developers to maintain the code, we can use "Region" to set a region of the same set of events, functions, or variables:

```vb
#Region "Events"
    Protected Sub txtPrice_TextChanged(...) Handles txtPrice.TextChanged
        'Do the ops here...
    End Sub

    Protected Sub txtTotal_TextChanged(...) Handles txtTotal.TextChanged
        'Do the ops here...
    End Sub

    'Some other events....

#End Region

```

This region block could be collapsed to gain some visual help when the code row goes to 1000+. It is also save your scroll efforts.

[<img src="https://i.stack.imgur.com/GXRx8.png" alt="enter image description here" />](https://i.stack.imgur.com/GXRx8.png)

Tested on VS 2005, 2008 2010, 2015 and 2017.



## Creating a simple Calculator to get familiar with the interface and code.


1. Once you have installed Visual Studio from [https://www.visualstudio.com/downloads/](https://www.visualstudio.com/downloads/), start a new project.

<li>
[<img src="https://i.stack.imgur.com/AEVuZ.png" alt="Interface" />](https://i.stack.imgur.com/AEVuZ.png)
</li>
<li>
Select 'Windows Forms Application' from Visual Basic Tab. You can rename it here if you need to.
</li>
<li>
Once you click 'OK', you will see this window:
</li>

[<img src="https://i.stack.imgur.com/hd4h6.png" alt="VB.Net editor" />](https://i.stack.imgur.com/hd4h6.png)

<li>
Click on the 'Toolbox' tab on the left. The toolbar has 'auto-hide' option enabled by default. To disable this option, click the small symbol between the 'down arrow' symbol and the 'x' symbol, on the top-right corner of Toolbox window.
</li>
<li>
Get yourself familiar with the tools provided in the box. I have made a calculator interface by using buttons and a Textbox.
</li>

[<img src="https://i.stack.imgur.com/lJykr.png" alt="Calculator" />](https://i.stack.imgur.com/lJykr.png)

<li>
Click on the **Properties** tab (It is on the right side of the editor). You can change the **Text** property of a button, and the textbox to rename them. **Font** property can be used to alter the font of the controls.
</li>
<li>
To write the specific action for an event(eg. clicking on a button), double click on the control. Code window will open.
</li>

[<img src="https://i.stack.imgur.com/op2kd.png" alt="Sample Code" />](https://i.stack.imgur.com/op2kd.png)

1. VB.Net is a powerful language designed for fast development. High encapsulation and abstraction is cost for it. You do not need to add **semicolon** to indicate the end of a statement, there are no brackets, and most of the time, it auto-corrects the case of the alphabets.
<li>Code provided in the picture should be simple to understand.
**Dim** is the keyword used to initialize a variable, and **new** allocates memory.
Anything you type in the textbox is of type **string** by default. Casting is required to use the value as a different type.</li>

Enjoy your first creation in VB.Net!



#### Remarks


Visual Basic .NET is the official successor to Microsoft's original Visual Basic programming language. Visual Basic [.NET] appears to have similarities to Python with the lack of semicolons and brackets, but shares with C++ the basic structure of functions. Curly braces are absent in VB .NET, but instead are replaced with phrases like `End If`, `Next`, and `End Sub`.

