---
metaTitle: ".NET Framework - NuGet packaging system"
description: "Uninstalling a package from one project in a solution, Installing a specific version of a package, Installing the NuGet Package Manager, Adding a package source feed (MyGet, Klondike, ect), Managing Packages through the UI, Managing Packages through the console, Updating a package, Uninstalling a package, uninstall a specific version of package, Using different (local) Nuget package sources using UI"
---

# NuGet packaging system



## Uninstalling a package from one project in a solution


```dotnet
PM> Uninstall-Package -ProjectName MyProjectB EntityFramework

```



## Installing a specific version of a package


```dotnet
PM> Install-Package EntityFramework -Version 6.1.2  

```



## Installing the NuGet Package Manager


In order to be able to manage your projects' packages, you need the NuGet Package Manager. This is a Visual Studio Extension, explained in the official docs: [Installing and Updating NuGet Client](https://docs.nuget.org/consume/installing-nuget).

Starting with Visual Studio 2012, NuGet is included in every edition, and can be used from: Tools -> NuGet Package Manager -> Package Manager Console.

You do so through the Tools menu of Visual Studio, clicking Extensions and Updates:

[<img src="http://i.stack.imgur.com/zTzgp.png" alt="enter image description here" />](http://i.stack.imgur.com/zTzgp.png)

This installs both the GUI:

- Available through clicking "Manage NuGet Packages..." on a project or its References folder

And the Package Manager Console:

- Tools -> NuGet Package Manager -> Package Manager Console.



## Adding a package source feed (MyGet, Klondike, ect)


```dotnet
nuget sources add -name feedname -source http://sourcefeedurl

```



## Managing Packages through the UI


When you right-click a project (or its References folder), you can click the "Manage NuGet Packages..." option. This shows the [Package Manager Dialog](https://docs.nuget.org/consume/package-manager-dialog).

[<img src="http://i.stack.imgur.com/Fi0Uq.png" alt="enter image description here" />](http://i.stack.imgur.com/Fi0Uq.png)



## Managing Packages through the console


Click the menus Tools -> NuGet Package Manager -> Package Manager Console to show the console in your IDE. [Official documentation here](https://docs.nuget.org/consume/package-manager-console-powershell-reference).

Here you can issue, amongst others, `install-package` commands which installs the entered package into the currently selected "Default project":

```dotnet
Install-Package Elmah

```

You can also provide the project to install the package to, overriding the selected project in the "Default project" dropdown:

```dotnet
Install-Package Elmah -ProjectName MyFirstWebsite

```



## Updating a package


To update a package use the following command:

```dotnet
PM> Update-Package EntityFramework

```

where EntityFramework is the name of the package to be updated. Note that update will run for all projects, and so is different from `Install-Package EntityFramework` which would install to "Default project" only.

You can also specify a single project explicitly:

```dotnet
PM> Update-Package EntityFramework -ProjectName MyFirstWebsite

```



## Uninstalling a package


```dotnet
PM> Uninstall-Package EntityFramework  

```



## uninstall a specific version of package


```dotnet
PM> uninstall-Package EntityFramework -Version 6.1.2

```



## Using different (local) Nuget package sources using UI


It is common for company to set up it's own nuget server for distribution of packages across different teams.

1. Go to Solution Explorer and click <kbd>Right Mouse</kbd> button then choose `Manage NuGet Packages for Solution`

[<img src="http://i.stack.imgur.com/PhB3d.png" alt="enter image description here" />](http://i.stack.imgur.com/PhB3d.png)

1. In window that opens click on `Settings`

[<img src="http://i.stack.imgur.com/8vKM6.png" alt="enter image description here" />](http://i.stack.imgur.com/8vKM6.png)

1. Click on `+` in top right corner then add name and url that points to your local nuget server.

[<img src="http://i.stack.imgur.com/h85QG.png" alt="enter image description here" />](http://i.stack.imgur.com/h85QG.png)



#### Remarks


[NuGet.org](https://www.nuget.org/):

> 
NuGet is the package manager for the Microsoft development platform including .NET. The NuGet client tools provide the ability to produce and consume packages. The NuGet Gallery is the central package repository used by all package authors and consumers.


Images in examples courtesy of [NuGet.org](https://www.nuget.org/).

