---
metaTitle: "Xamarin - CarouselView - Pre-release version"
description: "Import CarouselView, Import CarouselView into a XAML Page"
---

# CarouselView - Pre-release version



## Import CarouselView


The easiest way to import CarouselView is to use the NuGet-Packages Manager in Xamarin / Visual studio:
[<img src="http://i.stack.imgur.com/AVZ5o.png" alt="enter image description here" />](http://i.stack.imgur.com/AVZ5o.png)

To use pre-release packages, make sure you enable the 'Show pre-release packages' checkbox at the left corner.

Each sub-project (.iOS/.droid./.WinPhone) must import this package.



## Import CarouselView into a XAML Page


### The basics

In the heading of ContentPage, insert following line:

```cs
xmlns:cv="clr-namespace:Xamarin.Forms;assembly=Xamarin.Forms.CarouselView"

```

Between the <ContentPage.Content> tags place the CarouselView:

```cs
<cv:CarouselView x:Name="DemoCarouselView">
</cv:CarouselView>

```

x:Name will give your CarouselView a name, which can be used in the C# code behind file.
This is the basics you need to do for integrating CarouselView into a view.
The given examples will not show you anything because the CarouselView is empty.

### Creating bindable source

As example of an ItemSource, I will be using a ObservableCollection of strings.

```cs
public ObservableCollection<TechGiant> TechGiants { get; set; }

```

TechGiant is a class that will host names of Technology Giants

```cs
public class TechGiant
{
    public string Name { get; set; }

    public TechGiant(string Name)
    {
        this.Name = Name;
    }
}

```

After the InitializeComponent of your page, create and fill the ObservableCollection

```cs
TechGiants = new ObservableCollection<TechGiant>();
TechGiants.Add(new TechGiant("Xamarin"));
TechGiants.Add(new TechGiant("Microsoft"));
TechGiants.Add(new TechGiant("Apple"));
TechGiants.Add(new TechGiant("Google"));

```

At last, set TechGiants to be the ItemSource of the DemoCarouselView

```cs
DemoCarouselView.ItemsSource = TechGiants;

```

### DataTemplates

In the XAML - file, give the CarouselView a DataTemplate:

```cs
<cv:CarouselView.ItemTemplate>
</cv:CarouselView.ItemTemplate>

```

Define a DataTemplate. In this case, this will be a Label with text bind to the itemsource and a green background:

```cs
<DataTemplate>
    <Label Text="{Binding Name}" BackgroundColor="Green"/>
</DataTemplate>

```

That's it! Run the program and see the result!



#### Remarks


CarouselView is a Xamarin Control which can contains any kind of View.
This pre-release control can only be used in Xamarin Forms projects.

In the example provided by [James Montemagno](https://blog.xamarin.com/flip-through-items-with-xamarin-forms-carouselview/), on the blog of Xamarin, CarouselView is used to display images.

At this moment CarouselView is not integrated in Xamarin.Forms. To use this in your project(s), you will have to add the NuGet-Package (see example above).

