---
metaTitle: "Xamarin - Custom Fonts in Styles"
description: "Accessing custom Fonts in Syles"
---

# Custom Fonts in Styles



## Accessing custom Fonts in Syles


Xamarin.Forms provide great mechanism for styling your cross-platforms application with global styles.

In mobile world your application must be pretty and stand out from the other applications.
One of this characters is Custom Fonts used in application.

With power support of XAML Styling in Xamarin.Forms just created base style for all labels with yours custom fonts.

To include custom fonts into you iOS and Android project follow the guide in [Using custom fonts on iOS and Android with Xamarin.Forms](https://blog.verslu.is/xamarin/xamarin-forms-xamarin/using-custom-fonts-on-ios-and-android-with-xamarin-forms/) post written by Gerald.

Declare Style in App.xaml file resource section. This make all styles globally visible.

From Gerald post above we need to use StyleId property but it isn't bindable property, so to using it in Style Setter we need to create Attachable Property for it:

```cs
public static class FontHelper
{
    public static readonly BindableProperty StyleIdProperty =
        BindableProperty.CreateAttached(
            propertyName: nameof(Label.StyleId),
            returnType: typeof(String),
            declaringType: typeof(FontHelper),
            defaultValue: default(String),
            propertyChanged: OnItemTappedChanged);

    public static String GetStyleId(BindableObject bindable) => (String)bindable.GetValue(StyleIdProperty);

    public static void SetStyleId(BindableObject bindable, String value) => bindable.SetValue(StyleIdProperty, value);


    public static void OnItemTappedChanged(BindableObject bindable, object oldValue, object newValue)
    {
        var control = bindable as Element;
        if (control != null)
        {
            control.StyleId = GetStyleId(control);
        }
    }
}

```

Then add style in App.xaml resource:

```cs
<?xml version="1.0" encoding="utf-8" ?>
<Application xmlns="http://xamarin.com/schemas/2014/forms"
             xmlns:x="http://schemas.microsoft.com/winfx/2009/xaml"
             xmlns:h="clr-namespace:My.Helpers"
             x:Class="My.App">

  <Application.Resources>

    <ResourceDictionary>
        <Style x:Key="LabelStyle" TargetType="Label">
            <Setter Property="FontFamily" Value="Metric Bold" />
            <Setter Property="h:FontHelper.StyleId" Value="Metric-Bold" />
        </Style>
    </ResourceDictionary>

  </Application.Resources>

</Application>

```

According to post above we need to create Custom Renderer for Label which inherits from LabelRenderer On Android platform.

```cs
internal class LabelExRenderer : LabelRenderer
{
    protected override void OnElementChanged(ElementChangedEventArgs<Label> e)
    {
        base.OnElementChanged(e);
        if (!String.IsNullOrEmpty(e.NewElement?.StyleId))
        {
            var font = Typeface.CreateFromAsset(Forms.Context.ApplicationContext.Assets, e.NewElement.StyleId + ".ttf");
            Control.Typeface = font;
        }
    }
}

```

For iOS platform no custom renderers required.

Now you can obtain style in your`s page markup:

For specific label

```cs
<Label Text="Some text" Style={StaticResource LabelStyle} />

```

Or apply style to all labels on the page by creating Style Based on LabesStyle

```cs
<!-- language: xaml -->

<?xml version="1.0" encoding="utf-8" ?>
<ContentPage xmlns="http://xamarin.com/schemas/2014/forms"
             xmlns:x="http://schemas.microsoft.com/winfx/2009/xaml"
             x:Class="My.MainPage">

  <ContentPage.Resources>

    <ResourceDictionary>
        <Style TargetType="Label" BasedOn={StaticResource LabelStyle}>
        </Style>
    </ResourceDictionary>

  </ContentPage.Resources>

  <Label Text="Some text" />      

</ContentPage>

```



#### Remarks


Resources to look at:

- [Xamarin Styles](https://developer.xamarin.com/guides/xamarin-forms/user-interface/styles/)
- [Using custom fonts on iOS and Android with Xamarin.Forms](https://blog.verslu.is/xamarin/xamarin-forms-xamarin/using-custom-fonts-on-ios-and-android-with-xamarin-forms/)
- [Custom Renderers](https://developer.xamarin.com/guides/xamarin-forms/custom-renderer/)
- [Resource Dictionaries](https://developer.xamarin.com/guides/xamarin-forms/xaml/resource-dictionaries/)
- [Attached Properties](https://developer.xamarin.com/guides/xamarin-forms/xaml/attached-properties/)

