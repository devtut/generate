---
metaTitle: "Xamarin - Auto Layout in Xamarin.iOS"
description: "Adding Constraints With iOS 9+ Layout Anchors, Adding Constraints Using Visual Format Language (VFL), Using Cirrious.FluentLayout, Adding Constraints with Masonry"
---

# Auto Layout in Xamarin.iOS



## Adding Constraints With iOS 9+ Layout Anchors


```cs
// Since the anchor system simply returns constraints, you still need to add them somewhere.
View.AddConstraints(
    new[] {
        someLabel.TopAnchor.ConstraintEqualTo(TopLayoutGuide.GetBottomAnchor()),
        anotherLabel.TopAnchor.ConstraintEqualTo(someLabel.BottomAnchor, 6),
        oneMoreLabel.TopAnchor.ConstraintEqualTo(anotherLabel.BottomAnchor, 6),
        oneMoreLabel.BottomAnchor.ConstraintGreaterThanOrEqualTo(BottomLayoutGuide.GetTopAnchor(), -10),
    }
);

```



## Adding Constraints Using Visual Format Language (VFL)


```cs
// Using Visual Format Language requires a special look-up dictionary of names<->views.
var views = new NSDictionary(
    nameof(someLabel), someLabel,
    nameof(anotherLabel), anotherLabel,
    nameof(oneMoreLabel), oneMoreLabel
);
// It can also take a look-up dictionary for metrics (such as size values).
// Since we are hard-coding those values in this example, we can give it a `null` or empty dictionary.
var metrics = (NSDictionary)null;

// Add the vertical constraints to stack everything together.
// `V:` = vertical
// `|…|` = constrain to super view (`View` for this example)
// `-10-` = connection with a gap of 10 pixels (could also be a named parameter from the metrics dictionary)
// `-[viewName]-` = connection with a control by name looked up in views dictionary (using C# 6 `nameof` for refactoring support)
var verticalConstraints = NSLayoutConstraint.FromVisualFormat(
    $"V:|-20-[{nameof(someLabel)}]-6-[{nameof(anotherLabel)}]-6-[{nameof(oneMoreLabel)}]->=10-|",
    NSLayoutFormatOptions.AlignAllCenterX,
    metrics,
    views
);
View.AddConstraints(verticalConstraints);

```

You may find some constraint types, like [aspect ratios](https://developer.apple.com/library/prerelease/content/documentation/UserExperience/Conceptual/AutolayoutPG/VisualFormatLanguage.html), cannot be conveyed in Visual Format Language (VFL) syntax and must call the appropriate methods directly.



## Using Cirrious.FluentLayout


Using NuGet

```cs
Install-Package Cirrious.FluentLayout

```

An expanded example based on the starter example at the [GitHub](https://github.com/FluentLayout/Cirrious.FluentLayout) Page, a simple first name, last name labels and fields all stacked one on top of the other:

```cs
public override void ViewDidLoad()
{
    //create our labels and fields
    var firstNameLabel = new UILabel();
    var lastNameLabel = new UILabel();
    var firstNameField = new UITextField();
    var lastNameField = new UITextField();

    //add them to the View
    View.AddSubviews(firstNameLabel, lastNameLabel, firstNameField, lastNameField);
    
    //create constants that we can tweak if we do not like the final layout
    const int vSmallMargin = 5;
    const int vMargin = 20;
    const int hMargin = 10;

    //add our constraints
    View.SubviewsDoNotTranslateAutoresizingMaskIntoConstraints();        
    View.AddConstraints(
        firstNameLabel.WithSameTop(View).Plus(vMargin),
        firstNameLabel.AtLeftOf(View).Plus(hMargin),
        firstNameLabel.WithSameWidthOf(View),

        firstNameField.WithSameWidth(firstNameLabel),
        firstNameField.WithSameLeft(firstNameLabel),
        firstNameField.Below(firstNameLabel).Plus(vSmallMargin),

        lastNameLabel.Below(firstNameField).Plus(vMargin),
        lastNameLabel.WithSameLeft(firstNameField),
        lastNameLabel.WithSameWidth(firstNameField),

        lastNameField.Below(lastNameLabel).Plus(vSmallMargin),
        lastNameField.WithSameWidth(lastNameLabel),
        lastNameField.WithSameLeft(lastNameLabel));
}

```



## Adding Constraints with Masonry


Masonry is a library for objective-c but xamarin have created a binding for it and created it as a nuget package [https://www.nuget.org/packages/Masonry/](https://www.nuget.org/packages/Masonry/).

Nuget install

```cs
Install-Package Masonry

```

This centers a button 100 points below the centre point of the containing view and sets a width between 200 and 400 points

```cs
this.loginBtn.MakeConstraints(make =>
{
    make.Width.GreaterThanOrEqualTo(new NSNumber(200));
    make.Width.LessThanOrEqualTo(new NSNumber(400));
    make.Center.EqualTo(this.View).CenterOffset(new CGPoint(0, 100));
});

```

This sets a scaled image 100 points above the centre point of the containing view then sets the width to the width of the containing view with a muliplier of 0.5 which means 50% of the width. It then sets the height to the width multiplied by the aspect ratio which causes the image to scale but maintain its correct aspect ratio

```cs
this.logo.MakeConstraints(make =>
{
    make.Center.EqualTo(this.View).CenterOffset(new CGPoint(0, -100));
    make.Width.EqualTo(this.View).MultipliedBy(0.5f);
    make.Height.EqualTo(this.logo.Width()).MultipliedBy(0.71f);
});

```

