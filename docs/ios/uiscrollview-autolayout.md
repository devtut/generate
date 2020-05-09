---
metaTitle: "iOS - UIScrollView AutoLayout"
description: "ScrollableController, UIScrollView dynamic content size through Storyboard"
---

# UIScrollView AutoLayout



## ScrollableController


When using Autolayout with a `UIScrollView`, it does NOT resize properly depending on the size of its contents or subviews.

In order to get a `UIScrollView` to automatically scroll when its contents become too large to fit in the visible area, we need to add a `ContentView` and some constraints that allow the `UIScrollView` to determine the size of its contents AND its width and height in its parent view.

```swift
import Foundation
import UIKit

class ScrollableController : UIViewController {
    
    private var scrollView: UIScrollView!
    private var contentView: UIView!
    
    override func viewDidLoad() {
        super.viewDidLoad()
        
        //Setup
        self.initControls()
        self.setTheme()
        self.layoutScrollView()
        self.layoutContentView()
        
        //Add child views
        self.addChildViews()
    }
    
    func initControls() {
        self.scrollView = UIScrollView()
        self.contentView = UIView()
    }
    
    func setTheme() {
        self.scrollView.backgroundColor = UIColor.blue()
        self.contentView.backgroundColor = UIColor.orange()
    }
    
    func layoutScrollView() {
        self.view.addSubview(self.scrollView)
        
        let views: NSDictionary = ["scrollView": self.scrollView]
        var constraints = Array<String>()
        
        //Constrain the scrollView to our controller's self.view.
        constraints.append("H:|-0-[scrollView]-0-|")
        constraints.append("V:|-0-[scrollView]-0-|")
        
        for constraint in constraints {
            self.view.addConstraints(NSLayoutConstraint.constraints(withVisualFormat: constraint, options: NSLayoutFormatOptions(rawValue: 0), metrics: nil, views: views as! [String : AnyObject]))
        }
        
        self.scrollView.translatesAutoresizingMaskIntoConstraints = false
    }
    
    func layoutContentView() {
        self.scrollView.addSubview(self.contentView)
        
        let views: NSDictionary = ["contentView": self.contentView, "view": self.view]
        var constraints = Array<String>()
        
        //Constrain the contentView to the scrollView.
        constraints.append("H:|-0-[contentView]-0-|")
        constraints.append("V:|-0-[contentView]-0-|")
        
        for constraint in constraints {
            self.scrollView.addConstraints(NSLayoutConstraint.constraints(withVisualFormat: constraint, options: NSLayoutFormatOptions(rawValue: 0), metrics: nil, views: views as! [String : AnyObject]))
        }
        
        //Disable Horizontal Scrolling by making the contentView EqualWidth with our controller's self.view (ScrollView's parentView).
        self.view.addConstraints(NSLayoutConstraint.constraints(withVisualFormat: "H:[contentView(==view)]", options: NSLayoutFormatOptions(rawValue: 0), metrics: nil, views: views as! [String : AnyObject]))
        
        self.contentView.translatesAutoresizingMaskIntoConstraints = false
    }
    
    func addChildViews() {
        //Init
        let greenView = UIView()
        let whiteView = UIView()
        
        //Theme
        greenView.backgroundColor = UIColor.green()
        whiteView.backgroundColor = UIColor.orange()
        
        //Layout -- Child views are added to the 'ContentView'
        self.contentView.addSubview(greenView)
        self.contentView.addSubview(whiteView)
        
        let views: NSDictionary = ["greenView": greenView, "whiteView": whiteView];
        var constraints = Array<String>()
        
        //Constrain the greenView to the contentView with a height of 400 and 15 spacing all around.
        constraints.append("H:|-15-[greenView]-15-|")
        constraints.append("V:|-15-[greenView(400)]")
        
        //Constrain the whiteView below the greenView with 15 spacing all around and a height of 500.
        constraints.append("H:|-15-[whiteView]-15-|")
        constraints.append("V:[greenView]-15-[whiteView(500)]-15-|")
        
        for constraint in constraints {
            self.contentView.addConstraints(NSLayoutConstraint.constraints(withVisualFormat: constraint, options: NSLayoutFormatOptions(rawValue: 0), metrics: nil, views: views as! [String : AnyObject]))
        }
        
        greenView.translatesAutoresizingMaskIntoConstraints = false
        whiteView.translatesAutoresizingMaskIntoConstraints = false
    }
}

```

Now we can see that the greenView (400 height) + the whiteView (500 height) is larger than our screen. This will cause the ScrollView's contentSize to grow to fit BOTH views, allowing it to scroll vertically.

We disabled horizontal scrolling using the `EqualWidth` constraint on the `contentView` and `self.view`

[<img src="http://i.stack.imgur.com/a9kai.png" alt="enter image description here" />](http://i.stack.imgur.com/a9kai.png)



## UIScrollView dynamic content size through Storyboard


While using scrollviews in storyboard it's better to calculate content size according to number of views present in scrollview rather than giving content size  programatically with static value.

Here are the steps to get content size dynamically.

Step 1 :

Add Scrollview to view in storyboard and add leading, trailing, top and bottom constraints (All values are zero).

Step 2 :

Don't add directly views which you need on directly scrollview, First add one view to scrollview (that will be our content view for all UI elements). Add below constraints to this view.

<li>
Leading, trailing, top and bottom constraints (All values are zero).
</li>
<li>
Add equal height, equal width to Main view (i.e. which contains scrollview). For equal height set priority to low. (This is the important step for setting content size).
</li>
<li>
Height of this content view will be according to the number of views added to the view. let say if you added last view is one label and his Y position is 420 and height is 20 then your content view will be 440.
</li>

Step 3 : Add constraints to all of views which you added within content view as per your requirement.

[<img src="https://i.stack.imgur.com/e6GKu.png" alt="enter image description here" />](https://i.stack.imgur.com/e6GKu.png)

[<img src="https://i.stack.imgur.com/acGwM.png" alt="enter image description here" />](https://i.stack.imgur.com/acGwM.png)

