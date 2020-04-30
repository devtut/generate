---
metaTitle: "AssemblyInfo.cs Examples"
description: "Global and local AssemblyInfo, [AssemblyVersion], [AssemblyTitle], [AssemblyProduct], Automated versioning, Common fields, [InternalsVisibleTo], Reading Assembly Attributes, [AssemblyConfiguration], [AssemblyKeyFile]"
---

# AssemblyInfo.cs Examples



## Global and local AssemblyInfo


Having a global allows for better DRYness, you need only put values that are different into AssemblyInfo.cs for projects that have variance.   This use assumes your product has more than one visual studio project.

GlobalAssemblyInfo.cs

```cs
using System.Reflection;
using System.Runtime.InteropServices;
//using Stackoverflow domain as a made up example    

// It is common, and mostly good, to use one GlobalAssemblyInfo.cs that is added 
// as a link to many projects of the same product, details below
// Change these attribute values in local assembly info to modify the information.
[assembly: AssemblyProduct("Stackoverflow Q&A")]
[assembly: AssemblyCompany("Stackoverflow")]
[assembly: AssemblyCopyright("Copyright © Stackoverflow 2016")]

// The following GUID is for the ID of the typelib if this project is exposed to COM
[assembly: Guid("4e4f2d33-aaab-48ea-a63d-1f0a8e3c935f")]
[assembly: ComVisible(false)] //not going to expose ;)

// Version information for an assembly consists of the following four values:
// roughly translated from I reckon it is for SO, note that they most likely 
// dynamically generate this file
//      Major Version  - Year 6 being 2016
//      Minor Version  - The month
//      Day Number     - Day of month
//      Revision       - Build number
// You can specify all the values or you can default the Build and Revision Numbers 
// by using the '*' as shown below: [assembly: AssemblyVersion("year.month.day.*")]
[assembly: AssemblyVersion("2016.7.00.00")] 
[assembly: AssemblyFileVersion("2016.7.27.3839")]

```

AssemblyInfo.cs - one for each project

```cs
//then the following might be put into a separate Assembly file per project, e.g.
[assembly: AssemblyTitle("Stackoveflow.Redis")]

```

You can add the GlobalAssemblyInfo.cs to the local project using the [following procedure](https://stackoverflow.com/questions/62353/what-are-the-best-practices-for-using-assembly-attributes):

1. Select Add/Existing Item... in the context menu of the project
1. Select GlobalAssemblyInfo.cs
<li>Expand the Add-Button by clicking on
that little down-arrow on the right hand</li>
<li>Select "Add As Link" in the
buttons drop down list</li>



## [AssemblyVersion]


This attribute applies a version to the assembly.

```cs
[assembly: AssemblyVersion("1.0.*")]

```

The `*` character is used to auto-increment a portion of the version automatically every time you compile (often used for the "build" number)



## [AssemblyTitle]


This attribute is used to give a name to this particular assembly.

```cs
[assembly: AssemblyTitle("MyProduct")]

```



## [AssemblyProduct]


This attribute is used to describe the product that this particular assembly is for. Multiple assemblies can be components of the same product, in which case they can all share the same value for this attribute.

```cs
[assembly: AssemblyProduct("MyProduct")]

```



## Automated versioning


Your code in source control has version numbers either by default (SVN ids or Git SHA1 hashes) or explicitly (Git tags). Rather than manually updating versions in AssemblyInfo.cs you can use a build time process to write the version from your source control system into your AssemblyInfo.cs files and thus onto your assemblies.

The [GitVersionTask](https://www.nuget.org/packages/GitVersionTask/) or [SemVer.Git.Fody](https://www.nuget.org/packages/SemVer.Git.Fody/) NuGet packages are examples of the above. To use GitVersionTask, for instance, after installing the package in your project remove the `Assembly*Version` attributes from your AssemblyInfo.cs files. This puts GitVersionTask in charge of versioning your assemblies.

Note that Semantic Versioning is increasingly the **de facto** standard so these methods recommend using source control tags that follow SemVer.



## Common fields


It's good practice to complete your AssemblyInfo's default fields. The information may be picked up by installers and will then appear when using Programs and Features (Windows 10) to uninstall or change a program.

The minimum should be:

- AssemblyTitle - usually the namespace, **i.e.** MyCompany.MySolution.MyProject
- AssemblyCompany - the legal entities full name
- AssemblyProduct - marketing may have a view here
- AssemblyCopyright - keep it up to date as it looks scruffy otherwise

'AssemblyTitle' becomes the 'File description' when examining the DLL's Properties Details tab.



## [InternalsVisibleTo]


If you want to make `internal` classes or functions of an assembly accessable from another assembly you declare this by `InternalsVisibleTo` and the assembly name that is allowed to access.

In this example code in the assembly `MyAssembly.UnitTests` is allowed to call `internal` elements from `MyAssembly`.

```cs
[assembly: InternalsVisibleTo("MyAssembly.UnitTests")]

```

This is especially useful for unit testing to prevent unnecessary `public` declarations.



## Reading Assembly Attributes


Using .NET's rich reflection APIs, you can gain access to an assembly's metadata. For example, you can get `this` assembly's title attribute with the following code

```cs
using System.Linq;
using System.Reflection;

...

Assembly assembly = typeof(this).Assembly;
var titleAttribute = assembly.GetCustomAttributes<AssemblyTitleAttribute>().FirstOrDefault();

Console.WriteLine($"This assembly title is {titleAttribute?.Title}");

```



## [AssemblyConfiguration]


AssemblyConfiguration: The AssemblyConfiguration attribute must have the configuration that was used to build the assembly.
Use conditional compilation to properly include different assembly configurations.
Use the block similar to the example below. Add as many different configurations as you commonly use.

```cs
#if (DEBUG)

[assembly: AssemblyConfiguration("Debug")]

#else

[assembly: AssemblyConfiguration("Release")]

#endif

```



## [AssemblyKeyFile]


Whenever we want our assembly to install in GAC then it is must to have a strong name. For strong naming assembly we have to create a public key.
To generate the `.snk` file.

To create a strong name key file

> 
<ol>
- Developers command prompt for VS2015 (with administrator Access)
- At the command prompt, type cd C:\Directory_Name and press ENTER.
- At the command prompt, type sn -k KeyFileName.snk, and then press ENTER.
</ol>


once the keyFileName.snk is created at specified directory then give refernce in your project . give `AssemblyKeyFileAttribute` attribute the path to `snk` file to generate the key when we build our class library.

> 
Properties  -> AssemblyInfo.cs


```cs
[assembly: AssemblyKeyFile(@"c:\Directory_Name\KeyFileName.snk")]

```

Thi will create a strong name assembly after build. After creating your strong name assembly you can then install it in GAC

Happy Coding :)



#### Remarks


The filename `AssemblyInfo.cs` is used by convention as the source file where developers place metadata attributes that describe the entire assembly they are building.

