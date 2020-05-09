---
metaTitle: "Xamarin - Getting started with Xamarin.iOS"
description: "Get Started in Visual Studio, Get Started in Xamarin Studio, Hello, World"
---

# Getting started with Xamarin.iOS



## Get Started in Visual Studio


1. Browse to **File > New > Project** to bring you up the New Project dialog.
<li>Navigate to Visual C# > iOS > iPhone and select Single View App:
[<img src="http://i.stack.imgur.com/FLWsn.png" alt="enter image description here" />](http://i.stack.imgur.com/FLWsn.png)</li>
1. Give your app a **Name** and press **OK** to create your project.
<li>Select the Mac Agent icon from the toolbar, as illustrated below:
[<img src="http://i.stack.imgur.com/nH2rm.png" alt="enter image description here" />](http://i.stack.imgur.com/nH2rm.png)</li>
<li>Select the Mac that will build your application from the list (make sure you Mac is set up to receive the connection!), and press **Connect**:
[<img src="http://i.stack.imgur.com/EeLxo.png" alt="enter image description here" />](http://i.stack.imgur.com/EeLxo.png)</li>
1. To run your application, select the **Debug | iPhoneSimulator** configuration, and press the Play button:[<img src="http://i.stack.imgur.com/DPD9M.png" alt="enter image description here" />](http://i.stack.imgur.com/DPD9M.png)
<li>This will launch the iOS Simulator on the Mac, and will display your empty application:
[<img src="http://i.stack.imgur.com/j6cAT.png" alt="enter image description here" />](http://i.stack.imgur.com/j6cAT.png)</li>



## Get Started in Xamarin Studio


<li>Browse to **File > New > Solution** to bring you up the new project
dialog</li>
1. Select **Single View App** and press **Next**
1. Configure your app by setting your app name and organization ID, and press **Next**: [<img src="http://i.stack.imgur.com/fpEWw.png" alt="enter image description here" />](http://i.stack.imgur.com/fpEWw.png)

1. Set your Project name and Solution name, or leave as the default name. Click **Create** to create your project.
<li>To run your application, select the Debug | iPhone 6s iOS 9.x configuration, and press the **Play** button:
[<img src="http://i.stack.imgur.com/vACh6.png" alt="enter image description here" />](http://i.stack.imgur.com/vACh6.png)</li>
<li>This will launch the iOS Simulator, and will display your empty application:
[<img src="http://i.stack.imgur.com/g0XIw.png" alt="enter image description here" />](http://i.stack.imgur.com/g0XIw.png)</li>



## Hello, World


1. Double click on the **Main.Storyboard** file.
1. Set **View As** to iPhone 6:[<img src="http://i.stack.imgur.com/70TIT.png" alt="enter image description here" />](http://i.stack.imgur.com/70TIT.png)
1. Drag a label and a button from the Toolbox to the design surface so that it looks like the image below:[<img src="http://i.stack.imgur.com/tXE46.png" alt="enter image description here" />](http://i.stack.imgur.com/tXE46.png)
1. In the Properties pad, give the label and button the following properties:

|nothing|Name|Title
|---|---|---|---|---|---|---|---|---|---
|Label|lblClicks|[blank]
|Button|clickMe|Click Me!

1. Add the following code to the **ViewDidLoad** method inside the **ViewController** class:

```cs
clickMe.TouchUpInside += (sender, e) =>
{
    totalClicks++;
    if (totalClicks == 1)
    {
        lblClicks.Text = totalClicks + " Click";
    }
    
   else {
       lblClicks.Text = totalClicks + " Clicks";
   }
};

```


1. Run the application



#### Remarks


Xamarin.iOS allows you to create native iOS applications using the same UI controls you would in Objective-C and Xcode, but with the flexibility and elegance of a modern language (C#), the power of the .NET Base Class Library (BCL), and two first-class IDEs - Xamarin Studio and Visual Studio - at your fingertips.

For more information on installing Xamarin.iOS on your Mac or Windows machine, refer to the [Getting Started](http://developer.xamarin.com/guides/ios/getting_started/installation/) guides on the Xamarin developer center

