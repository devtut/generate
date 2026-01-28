---
metaTitle: "Xamarin - Create and use custom prototype table cells in xamarin.iOS using storyboard"
description: "Create custom cell using Storyboard"
---

# Create and use custom prototype table cells in xamarin.iOS using storyboard



## Create custom cell using Storyboard


Open Storyboard where you have your ViewController with TableView:

Add prototype cell (if there is no cell added before):

Customize cell as you want (in my case there is custom UIImage and Label):

[<img src="http://i.stack.imgur.com/Mmo9Q.png" alt="enter image description here" />](http://i.stack.imgur.com/Mmo9Q.png)

[<img src="http://i.stack.imgur.com/37WHY.png" alt="enter image description here" />](http://i.stack.imgur.com/37WHY.png)

Remember to set height of the cell. To do it select your whole TableView and from the Properties window select "Layout" tab. On the top of the properties window you should see "row height" - put the appropriate value:

[<img src="http://i.stack.imgur.com/3cttW.png" alt="enter image description here" />](http://i.stack.imgur.com/3cttW.png)

Now select prototype cell once again. In the Properties window type the name of the class (it will create code-behind class for it). In my case this is "FriendsCustomTableViewCell". After that provide "Identifier" for your cell. As you can see my is "FriendCell". Last thing to set is "Style" property set to custom. "Name" field should be empty. Once you click "enter" after typing "Class" code-behind file will be automatically created:

[<img src="http://i.stack.imgur.com/x6drt.png" alt="enter image description here" />](http://i.stack.imgur.com/x6drt.png)

[<img src="http://i.stack.imgur.com/YoJOm.png" alt="enter image description here" />](http://i.stack.imgur.com/YoJOm.png)

Now code behind for the cell should look like below:

```cs
public partial class FriendsCustomTableViewCell : UITableViewCell
{
    public FriendsCustomTableViewCell (IntPtr handle) : base (handle)
    {
    }

    public FriendsCustomTableViewCell(NSString cellId, string friendName, UIImage friendPhoto) : base (UITableViewCellStyle.Default, cellId)
    {
        FriendNameLabel.Text = friendName;
        FriendPhotoImageView.Image = friendPhoto;
    }

    //This methods is to update cell data when reuse:
    public void UpdateCellData(string friendName, UIImage friendPhoto)
    {
        FriendNameLabel.Text = friendName;
        FriendPhotoImageView.Image = friendPhoto;
    }
}

```

In UITableViewSource you have to declare cellIdentifier at the top of the class (in my case it is "FriendCell") and in "GetCell" method you have to cast cells and set data for them:

```cs
string cellIdentifier = "FriendCell";

public override UITableViewCell GetCell(UITableView tableView, NSIndexPath indexPath)
{
    FriendsCustomTableViewCell cell = (FriendsCustomTableViewCell) tableView.DequeueReusableCell(cellIdentifier);
    Friend friend = _friends[indexPath.Row];

    //---- if there are no cells to reuse, create a new one
    if (cell == null)
    { cell = new FriendsCustomTableViewCell(new NSString(cellIdentifier), friend.FriendName, new UIImage(NSData.FromArray(friend.FriendPhoto))); }

    cell.UpdateCellData(friend.UserName, new UIImage(NSData.FromArray(friend.FriendPhoto)));

    return cell;
}

```

