---
metaTitle: "iOS - UIScrollView with StackView child"
description: "A complex StackView inside Scrollview Example, Preventing ambiguous layout, Scrolling to content inside nested StackViews"
---

# UIScrollView with StackView child




## A complex StackView inside Scrollview Example


Here follows a example of what can be done with nested StackViews, giving the user the impression of a continuous scrolling experience using complex user interface elements or alignments.

[<img src="https://i.stack.imgur.com/ukGYH.png" alt="A complex StackView inside Scrollview Example" />](https://i.stack.imgur.com/ukGYH.png)



## Preventing ambiguous layout


A frequent question about StackViews inside Scrollviews comes from ambiguous with/heigh alerts on interface builder. As [this answer](http://stackoverflow.com/questions/19036228/uiscrollview-scrollable-content-size-ambiguity) explained, it is necessary to:

1. Add in the UIScrollView a UIView (the contentScrollView);
1. In this contentScrollView, set top, bottom, left and right margins to 0
1. Set also align center horizontally and vertically;



## Scrolling to content inside nested StackViews


The big gotcha about scrolling is to determine the offset necessary to present (for instance) a **Textfield inside a StackView with is inside the ScrollView**.

If you try to get **the position of `Textfield.frame.minY` can be 0**, because the minY frame is only considering the distance between the element and the top of the StackView. So you have to **consider all other parent stackviews/views.**

A good workaround for this is:

1 - Implement the ScrollView Extension

```swift
extension UIScrollView {

    func scrollToShowView(view: UIView){
        var offset = view.frame.minY
        var superview = view.superview
        while((superview  != nil)){
            offset += (superview?.frame.minY)!
            superview = superview?.superview
        }
        
        offset -= 100 //optional margin added on offset
        
        self.contentOffset = CGPoint.init(x: 0, y: offset)
    }

}

```

This will consider all parent view and sum the necessary offset for the scrollview present the necessary view on screen (for example a Textfield which cannot stay behind the user keyboard)

Usage example:

```swift
func textViewDidBeginEditing(_ textView: UITextView) {
    self.contentScrollView.scrollToShowView(view: textView)
}

```

