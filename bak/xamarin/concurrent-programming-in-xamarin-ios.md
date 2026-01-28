---
metaTitle: "Xamarin - Concurrent Programming in Xamarin.iOS"
description: "Manipulating UI from background threads, Using Async and await"
---

# Concurrent Programming in Xamarin.iOS



## Manipulating UI from background threads


Background threads cannot modify the UI; almost all UIKit methods must be called on the main thread.

From a subclass of `NSObject` (including any `UIViewController` or `UIView`):

```cs
InvokeOnMainThread(() =>
{
    // Call UI methods here
});

```

From a standard C# class:

```cs
UIApplication.SharedApplication.InvokeOnMainThread(() =>
{
    // Call UI methods here
});

```

`InvokeOnMainThread` waits for your code running on the main thread to execute before continuing. If you don't need to wait, use `BeginInvokeOnMainThread`.



## Using Async and await


You can use async methods to handle asynchronous executions. For example POST and GET requests. Let say below is your get data method.

```cs
Task<List> GetDataFromServer(int type);

```

You can call that method as shown below

```cs
var result = await GetDataFromServer(1);

```

However, in real day practice this method will be in a service layer interface. There for best way to do that is create a separate method to call this and update UI shown below.

```cs
//Calling from viewDidLoad
void async ViewDidLoad()
{
    await GetDataListFromServer(1);
    //Do Something else
}

//New method call to handle the async task
private async Task GetArchivedListFromServer(int type)
{
    var result = await GetDataFromServer(type);
    DataList.AddRange(result.toList());
    tableView.ReloadData();
}

```

In the above code snippet, GetDataListFromServer method will get called and it will send the web request. Nevertheless, it will not block the UI thread till it gets the response from the server. It will move down the line after `await GetDataListFromServer(1)`. However, inside the `private async Task GetArchivedListFromServer(int type)` method, it will wait till it gets the response from the server in order to execute the lines after `var result = await GetDataFromServer(type);`.

