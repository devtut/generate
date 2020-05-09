---
metaTitle: "Xamarin - Calculating variable row height in GetHeightForRow"
description: "Using GetHeightForRow"
---

# Calculating variable row height in GetHeightForRow



## Using GetHeightForRow


To set a custom row height, override `UITableViewSource.GetHeightForRow(UITableView,NSIndexPath)`:

```cs
public class ColorTableDataSource : UITableViewSource
{
    List<DomainClass> Model { get; set; }

    public override nfloat GetHeightForRow(UITableView tableView, NSIndexPath indexPath)
    {
        var height = Model[indexPath.Row % Model.Count].Height;
        return height;
    }
    
    //...etc ...
}

```

The domain class for the table (in this case, it has 1 of 3 random colors and 1 of 3 random heights):

```cs
public class DomainClass
{
    static Random rand = new Random(0);
    public UIColor Color { get; protected set; }
    public float Height { get; protected set; }

    static UIColor[] Colors = new UIColor[]
    {
        UIColor.Red,
        UIColor.Green,
        UIColor.Blue,
        UIColor.Yellow
    };

    public DomainClass()
    {
        Color = Colors[rand.Next(Colors.Length)];
        switch (rand.Next(3))
        {
            case 0:
                Height = 24.0f;
                break;
            case 1:
                Height = 44.0f;
                break;
            case 2:
                Height = 64.0f;
                break;
            default:
                throw new ArgumentOutOfRangeException();
        }
    }

    public override string ToString()
    {
        return string.Format("[DomainClass: Color={0}, Height={1}]", Color, Height);
    }
}

```

Which looks like:

[<img src="http://i.stack.imgur.com/Xyqqk.png" alt="Variable table row heights" />](http://i.stack.imgur.com/Xyqqk.png)

Here is a complete program to demonstrate the technique:

```cs
using System;
using System.Collections.Generic;
using System.Linq;

using Foundation;
using UIKit;
using System.Drawing;

namespace SingleFileTableViewSolution
{
  //Model
  public class DomainClass
  {
    static Random rand = new Random(0);
    public UIColor Color { get; protected set; }
    public float Height { get; protected set; }

    static UIColor[] Colors = new UIColor[]
    {
        UIColor.Red,
        UIColor.Green,
        UIColor.Blue,
        UIColor.Yellow
    };

    public DomainClass()
    {
        Color = Colors[rand.Next(Colors.Length)];
        switch (rand.Next(3))
        {
            case 0:
                Height = 24.0f;
                break;
            case 1:
                Height = 44.0f;
                break;
            case 2:
                Height = 64.0f;
                break;
            default:
                throw new ArgumentOutOfRangeException();
        }
    }

    public override string ToString()
    {
        return string.Format("[DomainClass: Color={0}, Height={1}]", Color, Height);
    }
  }

  //Table Source
  public class ColorTableDataSource : UITableViewSource
  {
    List<DomainClass> Model { get; set; }

    public ColorTableDataSource(List<DomainClass> model)
    {
        this.Model = model;
    }

    public override nint RowsInSection(UITableView tableView, nint section)
    {
        return 10000;
    }

    public override UITableViewCell GetCell(UITableView tableView, NSIndexPath indexPath)
    {
        var cell = tableView.DequeueReusableCell(ColoredTableCell.ID, indexPath);
        cell.ContentView.BackgroundColor = Model[indexPath.Row % Model.Count].Color;

        return cell;
    }

    public override nfloat GetHeightForRow(UITableView tableView, NSIndexPath indexPath)
    {
        var height = Model[indexPath.Row % Model.Count].Height;
        return height;
    }

    public override void CellDisplayingEnded(UITableView tableView, UITableViewCell cell, NSIndexPath indexPath)
    {
        Console.WriteLine("CellDisplayingEnded on {0}", indexPath);
    }
  }

  public class ColoredTableCell : UITableViewCell
  {
    public static readonly NSString ID = new NSString("ColoredTableCell");
    public static int ClassCount = 0;
    public int myId = 0;

    public ColoredTableCell()
    {
    }

    public ColoredTableCell(IntPtr handle) : base(handle)
    {
        Console.WriteLine("New ColoredTableCell allocated {0} {1}", handle.ToInt64(), ClassCount++);
        myId = handle.ToInt32();
    }

    ~ColoredTableCell()
    {
        Console.WriteLine("ColoredTableCell {0} being finalized.", myId);
    }
  }

  public class ColorTableController : UITableViewController
  {
    String title;

    public ColorTableController(String title, UITableViewSource source) : base()
    {
        this.title = title;
        this.TableView.Source = source;

        this.TableView.RegisterClassForCellReuse(typeof(ColoredTableCell), ColoredTableCell.ID);
        this.TableView.TableHeaderView = new UIView(new RectangleF(0, 0, 500, 200));
        this.TableView.TableHeaderView.BackgroundColor = UIColor.Brown;
    }

    public override void DidReceiveMemoryWarning()
    {
        // Releases the view if it doesn't have a superview.
        base.DidReceiveMemoryWarning();
    }

    public override void ViewDidLoad()
    {
        base.ViewDidLoad();

        Title = title;
    }
  }

  [Register("AppDelegate")]
  public class AppDelegate : UIApplicationDelegate
  {
    UIWindow window;
    ColorTableController viewController;

    public override bool FinishedLaunching(UIApplication app, NSDictionary options)
    {
        var models = new List<DomainClass>();
        for (int i = 0; i < 20; i++)
        {
            models.Add(new DomainClass());
        }

        var tableController = new ColorTableController("My Table", new ColorTableDataSource(models));

        window = new UIWindow(UIScreen.MainScreen.Bounds);
        window.RootViewController = tableController;

        window.MakeKeyAndVisible();

        return true;
    }
  }

  public class Application
  {
    static void Main(string[] args)
    {
        UIApplication.Main(args, null, "AppDelegate");
    }
  }
}

```



#### Remarks


Calculating row heights might be expensive and scrolling performance can suffer if you have larger amounts of data. In that scenario, override `UITableViewSource.EstimatedHeight(UITableView, NSIndexPath)` to quickly provide a number sufficient for rapid scrolling, e.g.,:

```cs
public override nfloat EstimatedHeight(UITableView tableView, NSIndexPath indexPath) 
{ 
    return 44.0f; 
}

```

