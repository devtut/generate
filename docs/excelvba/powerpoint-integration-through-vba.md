---
metaTitle: "Excel VBA - PowerPoint Integration Through VBA"
description: "The Basics: Launching PowerPoint from VBA"
---

# PowerPoint Integration Through VBA



## The Basics: Launching PowerPoint from VBA


While there are many parameters that can be changed and variations that can be added depending on the desired functionality, this example lays out the basic framework for launching PowerPoint.

> 
**Note:** This code requires that the PowerPoint reference has been added to the active VBA Project.  See the [References](http://stackoverflow.com/documentation/excel-vba/777/introduction-to-excel-vba/6489/adding-a-new-object-library-reference#t=201607220850379525533) Documentation entry to learn how to enable the reference.


First, define variables for the Application, Presentation, and Slide Objects.  While this can be done with late binding, it is always best to use early binding when applicable.

```vb
Dim PPApp As PowerPoint.Application
Dim PPPres As PowerPoint.Presentation
Dim PPSlide As PowerPoint.Slide

```

Next, open or create a new instance of the PowerPoint application.  Here, the `On Error Resume Next` call is used to avoid an error being thrown by `GetObject` if PowerPoint has not yet been opened.  See the [Error Handling](http://stackoverflow.com/documentation/excel-vba/1107/vba-best-practices/5411/error-handling#t=201607220853263117281) example of the Best Practices Topic for a more detailed explanation.

```vb
'Open PPT if not running, otherwise select active instance
On Error Resume Next
Set PPApp = GetObject(, "PowerPoint.Application")
On Error GoTo ErrHandler
If PPApp Is Nothing Then
    'Open PowerPoint
    Set PPApp = CreateObject("PowerPoint.Application")
    PPApp.Visible = True
End If

```

Once the application has been launched, a new presentation and subsequently contained slide is generated for use.

```vb
'Generate new Presentation and slide for graphic creation
Set PPPres = PPApp.Presentations.Add
Set PPSlide = PPPres.Slides.Add(1, ppLayoutBlank)

'Here, the slide type is set to the 4:3 shape with slide numbers enabled and the window 
'maximized on the screen.  These properties can, of course, be altered as needed

PPApp.ActiveWindow.ViewType = ppViewSlide
PPPres.PageSetup.SlideOrientation = msoOrientationHorizontal
PPPres.PageSetup.SlideSize = ppSlideSizeOnScreen
PPPres.SlideMaster.HeadersFooters.SlideNumber.Visible = msoTrue
PPApp.ActiveWindow.WindowState = ppWindowMaximized

```

Upon completion of this code, a new PowerPoint window with a blank slide will be open.  By using the object variables, shapes, text, graphics, and excel ranges can be added as desired



#### Remarks


This section demonstrates a variety of ways to interact with PowerPoint through VBA.  From showing data on slides to creating charts, PowerPoint is a very powerful tool when used in conjunction with Excel.  Thus, this section seeks to demonstrate the various ways VBA can be used to automate this interaction.

