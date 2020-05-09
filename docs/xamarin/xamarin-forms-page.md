---
metaTitle: "Xamarin - Xamarin.Forms Page"
description: "TabbedPage, ContentPage, MasterDetailPage"
---

# Xamarin.Forms Page



## TabbedPage


A TabbedPage is similar to a NavigationPage in that it allows for and manages simple
navigation between several child Page objects. The difference is that generally speaking, each
platform displays some sort of bar at the top or bottom of the screen that displays most, if not
all, of the available child Page objects.
In Xamarin.Forms applications, a TabbedPage is generally useful when you have a small
predefined number of pages that users can navigate between, such as a menu or a simple
wizard that can be positioned at the top or bottom of the screen.

**XAML**

[<img src="http://i.stack.imgur.com/BbYAQ.png" alt="enter image description here" />](http://i.stack.imgur.com/BbYAQ.png)

**Code**

```cs
var page1 = new ContentPage {
Title = "Tab1",
Content = new Label {
Text = "I'm the Tab1 Page",
HorizontalOptions = LayoutOptions.Center,
VerticalOptions = LayoutOptions.Center
}
};
var page2 = new ContentPage {
Title = "Tab2",
Content = new Label {
Text = "I'm the Tab2 Page",
HorizontalOptions = LayoutOptions.Center,
66
VerticalOptions = LayoutOptions.Center
}
};
var tabbedPage = new TabbedPage {
Children = { page1, page2 }
};

```

[<img src="http://i.stack.imgur.com/UTaHv.png" alt="enter image description here" />](http://i.stack.imgur.com/UTaHv.png)



## ContentPage


ContentPage: Displays a single View.

**XAML**

```cs
<?xml version="1.0" encoding="utf-8" ?>
<ContentPage xmlns="http://xamarin.com/schemas/2014/forms"
xmlns:x="http://schemas.microsoft.com/winfx/2009/xaml"
x:Class="XamlBasics.SampleXaml">
<Label Text="This is a simple ContentPage"
HorizontalOptions="Center"
VerticalOptions="Center" />
</ContentPage>

```

**Code**

```cs
var label = new Label {
Text = "This is a simple ContentPage",
HorizontalOptions = LayoutOptions.Center,
VerticalOptions = LayoutOptions.Center
};
var contentPage = new ContentPage {
Content = label
};

```

[<img src="http://i.stack.imgur.com/bPwVY.png" alt="enter image description here" />](http://i.stack.imgur.com/bPwVY.png)



## MasterDetailPage


MasterDetailPage: Manages two separate Pages (panes) of information.

**XAML**

```cs
<?xml version="1.0" encoding="utf-8" ?>
<MasterDetailPage xmlns="http://xamarin.com/schemas/2014/forms"
xmlns:x="http://schemas.microsoft.com/winfx/2009/xaml"
x:Class="XamlBasics.SampleXaml">
<MasterDetailPage.Master>
<ContentPage Title = "Master" BackgroundColor = "Silver">
<Label Text="This is the Master page."
TextColor = "Black"
HorizontalOptions="Center"
VerticalOptions="Center" />
</ContentPage>
</MasterDetailPage.Master>
<MasterDetailPage.Detail>
<ContentPage>
<Label Text="This is the Detail page."
HorizontalOptions="Center"
VerticalOptions="Center" />
</ContentPage>
</MasterDetailPage.Detail>
</MasterDetailPage>

```

**Code**

```cs
var masterDetailPage = new MasterDetailPage {
Master = new ContentPage {
Content = new Label {
Title = "Master",
BackgroundColor = Color.Silver,

TextColor = Color.Black,
Text = "This is the Master page.",
HorizontalOptions = LayoutOptions.Center,
VerticalOptions = LayoutOptions.Center
}
},
Detail = new ContentPage {
Content = new Label {
Title = "Detail",
Text = "This is the Detail page.",
HorizontalOptions = LayoutOptions.Center,
VerticalOptions = LayoutOptions.Center
}
}
};

```

[<img src="http://i.stack.imgur.com/J8SLX.png" alt="enter image description here" />](http://i.stack.imgur.com/J8SLX.png)

