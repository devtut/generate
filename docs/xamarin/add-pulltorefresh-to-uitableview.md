---
metaTitle: "Xamarin - Add PullToRefresh to UITableView"
description: "Adding UIRefreshControl to UITableView"
---

# Add PullToRefresh to UITableView



## Adding UIRefreshControl to UITableView


```cs
public override async void ViewDidLoad(){  
base.ViewDidLoad();  
// Perform any additional setup after loading the view, typically from a nib.  

Title = "Pull to Refresh Sample";  
table = new UITableView(new CGRect(0, 20, View.Bounds.Width, View.Bounds.Height - 20));  
//table.AutoresizingMask = UIViewAutoresizing.All;  
tableItems = new List<TableItem>();  
tableItems.Add(new TableItem("Vegetables") { ImageName = "Vegetables.jpg" });  
tableItems.Add(new TableItem("Fruits") { ImageName = "Fruits.jpg" });  
tableItems.Add(new TableItem("Flower Buds") { ImageName = "Flower Buds.jpg" });  
tableItems.Add(new TableItem("Legumes") { ImageName = "Legumes.jpg" });  
tableItems.Add(new TableItem("Tubers") { ImageName = "Tubers.jpg" });  
tableSource = new TableSource(tableItems);  
table.Source = tableSource;  

await RefreshAsync();  

AddRefreshControl();  

Add(table);  
table.Add(RefreshControl);
}

async Task RefreshAsync()  
{  
// only activate the refresh control if the feature is available  
if (useRefreshControl)  
    RefreshControl.BeginRefreshing();  

if (useRefreshControl)  
    RefreshControl.EndRefreshing();  

    table.ReloadData();  
}  

#region * iOS Specific Code  
// This method will add the UIRefreshControl to the table view if  
// it is available, ie, we are running on iOS 6+  
void AddRefreshControl()  
{  
if (UIDevice.CurrentDevice.CheckSystemVersion(6, 0))  
{  
    // the refresh control is available, let's add it  
    RefreshControl = new UIRefreshControl();  
    RefreshControl.ValueChanged += async (sender, e) =>  
    {  
        tableItems.Add(new TableItem("Bulbs") { ImageName = "Bulbs.jpg" });  
        await RefreshAsync();  
    };  
    useRefreshControl = true;  
   }    
}  
#endregion  

```



#### Remarks


Object References:

UITableView table;<br />
TableSource tableSource;<br />
bool useRefreshControl = false;<br />
UIRefreshControl RefreshControl;<br />
List tableItems;

TableSource and TableItem are user-defined classes

For the complete sample you can fork:
[https://github.com/adiiaditya/Xamarin.iOS-Samples/tree/master/PullToRefresh](https://github.com/adiiaditya/Xamarin.iOS-Samples/tree/master/PullToRefresh)

