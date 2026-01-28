---
metaTitle: "Xamarin - Add Search Bar to UITableView"
description: "Add UISearchBar to UITableView"
---

# Add Search Bar to UITableView



## Add UISearchBar to UITableView


```cs
public override void ViewDidLoad()  
{  
    base.ViewDidLoad();  
    // Perform any additional setup after loading the view, typically from a nib.  

    //Declare the search bar and add it to the header of the table  
    searchBar = new UISearchBar();  
    searchBar.SizeToFit();  
    searchBar.AutocorrectionType = UITextAutocorrectionType.No;  
    searchBar.AutocapitalizationType = UITextAutocapitalizationType.None;  
    searchBar.TextChanged += (sender, e) =>  
    {  
        //this is the method that is called when the user searches  
        searchTable();  
    };  

    Title = "SearchBarWithTableView Sample";  
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
    table.TableHeaderView = searchBar;  
    Add(table);  
}  

private void searchTable()  
{  
    //perform the search, and refresh the table with the results  
    tableSource.PerformSearch(searchBar.Text);  
    table.ReloadData();  
}  

```

The TableSource class will look like this:

```cs
public class TableSource : UITableViewSource  
{  
    private List<TableItem> tableItems = new List<TableItem>();  
    private List<TableItem> searchItems = new List<TableItem>();  
    protected string cellIdentifier = "TableCell";  

    public TableSource(List<TableItem> items)  
    {  
        this.tableItems = items;  
        this.searchItems = items;  
    }  

    public override nint RowsInSection(UITableView tableview, nint section)  
    {  
        return searchItems.Count;  
    }  

    public override UITableViewCell GetCell(UITableView tableView, NSIndexPath indexPath)  
    {  
        // request a recycled cell to save memory  
        UITableViewCell cell = tableView.DequeueReusableCell(cellIdentifier);  


        var cellStyle = UITableViewCellStyle.Default;  

        // if there are no cells to reuse, create a new one  
        if (cell == null)  
        {  
            cell = new UITableViewCell(cellStyle, cellIdentifier);  
        }  

        cell.TextLabel.Text = searchItems[indexPath.Row].Title;  
        cell.ImageView.Image = UIImage.FromFile("Images/" + searchItems[indexPath.Row].ImageName);  

        return cell;  
    }  

    public override nint NumberOfSections(UITableView tableView)  
    {  
        return 1;  
    }  

    public void PerformSearch(string searchText)  
    {  
        searchText = searchText.ToLower();  
        this.searchItems = tableItems.Where(x => x.Title.ToLower().Contains(searchText)).ToList();  
    }  
}  

```



#### Remarks


Object References:

UITableView table;<br />
TableSource tableSource;<br />
List tableItems;<br />
UISearchBar searchBar;

To fork the complete sample:
[https://github.com/adiiaditya/Xamarin.iOS-Samples/tree/master/SearchBarWithTableView](https://github.com/adiiaditya/Xamarin.iOS-Samples/tree/master/SearchBarWithTableView)

