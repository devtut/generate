---
metaTitle: "Xamarin - BDD Unit Testing in Xamarin.Forms"
description: "Simple Specflow to test commands and navigation with NUnit Test Runner, Advanced Usage for MVVM"
---

# BDD Unit Testing in Xamarin.Forms



## Simple Specflow to test commands and navigation with NUnit Test Runner


### Why do we need this?

The current way to do unit testing in Xamarin.Forms is via a platform runner, so your test will have to run within an ios, android, windows or mac UI environment
: [Running Tests in the IDE](https://developer.xamarin.com/guides/testcloud/uitest/working-with/running-tests-in-ide/)

Xamarin also provides awesome UI testing with the [Xamarin.TestCloud](https://www.xamarin.com/test-cloud) offering, but when wanting to implement BDD dev practices, and have the ability to test ViewModels and Commands, while running cheaply on a unit test runners locally or on a build server, there is not built in way.

I developed a library that allows to use Specflow with Xamarin.Forms to easily implement your features from your Scenarios definitions up to the ViewModel, independently of any MVVM framework used for the App (such as  [XLabs](https://github.com/XLabs/Xamarin-Forms-Labs), [MVVMCross](https://github.com/MvvmCross/MvvmCross), [Prism](https://github.com/PrismLibrary/Prism))

If you are new to BDD, check [Specflow](http://www.specflow.org/getting-started/) out.

### Usage:

<li>
If you don't have it yet, install the specflow visual studio extension from here (or from you visual studio IDE): [https://visualstudiogallery.msdn.microsoft.com/c74211e7-cb6e-4dfa-855d-df0ad4a37dd6](https://visualstudiogallery.msdn.microsoft.com/c74211e7-cb6e-4dfa-855d-df0ad4a37dd6)
</li>
<li>
Add a Class library to your Xamarin.Forms project. That's your test project.
</li>
<li>
Add SpecFlow.Xamarin.Forms package from [nuget](https://www.nuget.org/packages/SpecFlow.Xamarin.Forms) to your test projects.
</li>
<li>
Add a class to you test project that inherits 'TestApp', and register your views/viewmodels pairs as well as adding any DI registration, as per below:
</li>

- Add a SetupHook class to your test project, in order to add you Specflow hooks. You will need to bootstrap the test application as per below, providing the class you created above, and the your app initial viewmodel:

- You will need to add a catch block to your xamarin.forms views codebehind in order to ignore xamarin.forms framework forcing you to run the app ui (something we dont want to do):

<li>
Add a specflow feature to your project (using the vs specflow templates shipped with the vs specflow extension)
</li>
<li>
Create/Generate a step class that inherits TestStepBase, passing the scenarioContext parameter to the base.
</li>
<li>
Use the navigation services and helpers to navigate, execute commands, and test your view models:
</li>



## Advanced Usage for MVVM


To add to the first example, in order to test navigation statements that occurs within the application, we need to provide the ViewModel with a hook to the Navigation. To achieve this:

- Add the package SpecFlow.Xamarin.Forms.IViewModel from [nuget](https://www.nuget.org/packages/SpecFlow.Xamarin.Forms.IViewModel) to your PCL Xamarin.Forms project
- Implement the IViewModel interface in your ViewModel. This will simply expose the Xamarin.Forms INavigation property:
- `public class MainViewModel : INotifyPropertyChanged, IViewModel.IViewModel { public INavigation Navigation { get; set; }`
- The test framework will pick that up and manage internal navigation
- You can use any MVVM frameworks for you application (such as  [XLabs](https://github.com/XLabs/Xamarin-Forms-Labs), [MVVMCross](https://github.com/MvvmCross/MvvmCross), [Prism](https://github.com/PrismLibrary/Prism) to name a few. As long as the IViewModel interface is implemented in your ViewModel, the framework will pick it up.



#### Remarks


<li>The DI Container/resolver we use internally in this library is
Autofac.</li>
- The testing framework is NUnit 3x.
<li>You should be able to use this library with any Xamarin.Forms
framework</li>
- Source and example project available [here](https://github.com/mrbrl/SpecFlow.Xamarin.Forms)

