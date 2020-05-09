---
metaTitle: "Xamarin - AppSettings Reader in Xamarin.Forms"
description: "Reading app.config file in a Xamarin.Forms Xaml project"
---

# AppSettings Reader in Xamarin.Forms



## Reading app.config file in a Xamarin.Forms Xaml project


While each mobile platforms do offer their own settings management api, there are no built in ways to read settings from a good old .net style app.config xml file;
This is due to a bunch of good reasons, notably the .net framework configuration management api being on the heavyweight side, and each platform having their own file system api.

So we built a simple [PCLAppConfig](https://www.nuget.org/packages/PCLAppConfig) library, nicely nuget packaged for your immediate consumption.

This library makes use of the lovely [PCLStorage](https://www.nuget.org/packages/pclstorage) library

This example assumes you are developing a Xamarin.Forms Xaml project, where you would need to access settings from your shared viewmodel.

1. Initialize ConfigurationManager.AppSettings on each of your platform project, just after the 'Xamarin.Forms.Forms.Init' statement, as per below:

iOS (AppDelegate.cs)

```cs
global::Xamarin.Forms.Forms.Init();
ConfigurationManager.Initialise(PCLAppConfig.FileSystemStream.PortableStream.Current);
LoadApplication(new App());

```

Android (MainActivity.cs)

```cs
global::Xamarin.Forms.Forms.Init(this, bundle);
ConfigurationManager.Initialise(PCLAppConfig.FileSystemStream.PortableStream.Current);
LoadApplication(new App());

```

UWP / Windows 8.1 / WP 8.1 (App.xaml.cs)

```cs
Xamarin.Forms.Forms.Init(e);
ConfigurationManager.Initialise(PCLAppConfig.FileSystemStream.PortableStream.Current);

```


1. Add an app.config file to your shared PCL project, and add your appSettings entries, as you would do with any app.config file

```cs
<configuration>
    <appSettings>
        <add key="config.text" value="hello from app.settings!" />
    </appSettings>
</configuration>

```


<li>
<p>Add this PCL app.config file **as a linked file** on all your platform
projects. For android, make sure to set the build action to
**'AndroidAsset'**, for UWP set the build action to **'Content'**</p>
</li>
<li>
<p>Access your setting:
`ConfigurationManager.AppSettings["config.text"];`</p>
</li>

