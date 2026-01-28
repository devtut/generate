---
metaTitle: "Xamarin - Getting started with Xamarin.Forms"
description: "Installation (Visual Studio), Hello World Xamarin Forms: Visual Studio"
---

# Getting started with Xamarin.Forms



## Installation (Visual Studio)


Xamarin.Forms is a cross-platform natively backed UI toolkit abstraction that allows developers to easily create user interfaces that can be shared across Android, iOS, Windows, and Windows Phone. The user interfaces are rendered using the native controls of the target platform, allowing Xamarin.Forms applications to retain the appropriate look and feel for each platform.

### Xamarin Plugin for Visual Studio

To get started with Xamarin.Forms for Visual Studio you need to have the Xamarin plugin itself. The easiest way to have it installed is to download and install the latest Visual Studio.

If you already have the latest Visual Studio installed, go to Control Panel > Programs and Features, right click on Visual Studio, and click Change. When the installer opens, click on Modify, and select the cross-platform mobile development tools:

[<img src="http://i.stack.imgur.com/SgkVJ.png" alt="enter image description here" />](http://i.stack.imgur.com/SgkVJ.png)

You can also select to install the Android SDK:

[<img src="http://i.stack.imgur.com/zgfGZ.png" alt="enter image description here" />](http://i.stack.imgur.com/zgfGZ.png)

Uncheck it if you already have the SDK installed. You will be able to setup Xamarin to use existing Android SDK later.

### Xamarin.Forms

Xamarin.Forms is a set of libraries for your Portable Class library and native assemblies. The Xamarin.Forms library itself is available as a NuGet package.  To add it to your project just use the regular `Install-Package` command of the Package Manager Console:

`Install-Package Xamarin.Forms`

for all of your initial assemblies (for example MyProject, MyProject.Droid and MyProject.iOS).

The easiest way to get started with Xamarin.Forms is to create an empty project in Visual Studio:

[<img src="http://i.stack.imgur.com/W7MZV.png" alt="enter image description here" />](http://i.stack.imgur.com/W7MZV.png)

As you can see there are 2 available options to create the blank app -- Portable and Shared. I recommend you to get started with Portable one because it's the most commonly used in the real world (differences and more explanation to be added).

After creating the project make sure you're using the latest Xamarin.Forms version as your initial template may contain the old one. Use your Package Manager Console or Manage NuGet Packages option to upgrade to the latest Xamarin.Forms (remember it's just a NuGet package).

While the Visual Studio Xamarin.Forms templates will create an iOS platform project for you, you will need to connect Xamarin to a Mac build host to be able to run these projects on the iOS Simulator or physical devices.



## Hello World Xamarin Forms: Visual Studio


After successfully installing Xamarin as described in the first example, it's time to launch the first sample application.

### Step 1: Creating a new Project.

In Visual Studio, choose New -> Project -> Visual C# -> Cross-Platform -> Blank App (Xamarin.Forms Portable)

Name the app "Hello World" and select the location to create the project and click OK. This will create a solution for you which contains three projects:

1. HelloWorld (this is where your logic and views is placed, i.e. the portable project)
1. HelloWorld.Droid (the Android project)
1. HelloWorld.iOS (the iOS project)

[<img src="https://i.stack.imgur.com/VFpzj.png" alt="Creating a new project" />](https://i.stack.imgur.com/VFpzj.png)

### Step 2: Investigating the sample

Having created the solution, a sample application will be ready to be deployed. Open the `App.cs` located in the root of the portable project and investigate the code. As seen below, the `Content`s of the sample is a `StackLayout` which contains a `Label`:

```cs
using Xamarin.Forms;

namespace Hello_World
{
    public class App : Application
    {
        public App()
        {
            // The root page of your application
            MainPage = new ContentPage
            {
                Content = new StackLayout
                {
                    VerticalOptions = LayoutOptions.Center,
                    Children = {
                        new Label {
                            HorizontalTextAlignment = TextAlignment.Center,
                            Text = "Welcome to Xamarin Forms!"
                        }
                    }
                }
            };
        }
        protected override void OnStart()
        {
            // Handle when your app starts
        }
        protected override void OnSleep()
        {
            // Handle when your app sleeps
        }
        protected override void OnResume()
        {
            // Handle when your app resumes
        }
    }
}

```

### Step 3: Launching the application

Now simply right-click the project you want to start (`HelloWorld.Droid` or `HelloWorld.iOS`) and click `Set as StartUp Project`. Then, in the Visual Studio toolbar, click the `Start` button (the green triangular button that resembles a Play button) to launch the application on the targeted simulator/emulator.



#### Remarks


Xamarin.Forms makes it possible to create iOS, Android, and Windows apps with large amounts of shared code, including UI code or XAML UI markup. App pages and views are mapped to native controls on each platform, but can be customized to provide platform-specific UI or to access platform-specific features.

