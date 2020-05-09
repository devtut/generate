---
metaTitle: "Xamarin - Using ListViews"
description: "Pull to Refresh in XAML and Code behind"
---

# Using ListViews


This documentation details how to use the different components of the Xamarin Forms ListView



## Pull to Refresh in XAML and Code behind


To enable Pull to Refresh in a `ListView` in Xamarin, you first need to specify that it is `PullToRefresh` enabled and then specify the name of the command you want to invoke upon the `ListView` being pulled:

```cs
<ListView x:Name="itemListView" IsPullToRefreshEnabled="True" RefreshCommand="Refresh">

```

The same can be achieved in code behind:

```cs
itemListView.IsPullToRefreshEnabled = true;
itemListView.RefreshCommand = Refresh;

```

Then, you must specify what the `Refresh` Command does in your code behind:

```cs
public ICommand Refresh 
{
    get
    {
        itemListView.IsRefreshing = true; //This turns on the activity
                                          //Indicator for the ListView
        //Then add your code to execute when the ListView is pulled
        itemListView.IsRefreshing = false;
    }
}

```

