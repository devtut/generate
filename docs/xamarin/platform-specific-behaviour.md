---
metaTitle: "Xamarin - Platform-specific behaviour"
description: "Removing icon in navigation header in Anroid, Make label's font size smaller in iOS"
---

# Platform-specific behaviour



## Removing icon in navigation header in Anroid


[<img src="http://i.stack.imgur.com/9kvuD.png" alt="enter image description here" />](http://i.stack.imgur.com/9kvuD.png)
**Using a small transparent image called empty.png**

```cs
public class MyPage : ContentPage
{
    public Page()
    {
        if (Device.OS == TargetPlatform.Android)
            NavigationPage.SetTitleIcon(this, "empty.png");
    }
}

```



## Make label's font size smaller in iOS


```cs
Label label = new Label
{
    Text = "text"
};
if(Device.OS == TargetPlatform.iOS)
{
    label.FontSize = label.FontSize - 2;
}

```



#### Remarks


Target Platforms

```cs
if(Device.OS == TargetPlatform.Android)
{

}
else if (Device.OS == TargetPlatform.iOS)
{

}
else if (Device.OS == TargetPlatform.WinPhone)
{

}
else if (Device.OS == TargetPlatform.Windows)
{

}
else if (Device.OS == TargetPlatform.Other)
{

}

```

