---
metaTitle: "Xamarin - Adding UIRefreshControl to a table view"
description: "Addind a simple UIRefreshControl to a UIScrollView"
---

# Adding UIRefreshControl to a table view




## Addind a simple UIRefreshControl to a UIScrollView


We assume a fully working `UIScrollview` named `_scrollView`;

Note that `UITableView`, `UICollectionView` are also scrollviews, hence the following examples would work on those UI elements.

First, creation & allocation

```cs
UIRefreshControl refreshControl = new UIRefreshControl();

```

Second, connecting the refresh event to a method. There are different ways to do that.

### Style 1:

```cs
refreshControl.ValueChanged += (object sender, EventArgs e) => MyMethodCall();

```

### Style 2:

```cs
refreshControl.ValueChanged += (object sender, EventArgs e) =>
{
    //Write code here
};

```

### Style 3:

```cs
refreshControl.ValueChanged += HandleRefreshValueChanged;

void HandleRefreshValueChanged(object sender, EventArgs e)
{
    //Write code here
}

```

Third and last, adding the refresh control itself to our scrollview.

```cs
_scrollView.AddSubview(refreshControl);

```

