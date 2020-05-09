---
metaTitle: "iOS - AirPrint tutorial in iOS"
description: "AirPrint printing Banner Text"
---

# AirPrint tutorial in iOS



## AirPrint printing Banner Text


**Objective-C**

Add the delegate and a text-formatter to the `ViewController.h` file

```swift
@interface ViewController : UIViewController <UIPrintInteractionControllerDelegate> {
    UISimpleTextPrintFormatter *_textFormatter;
}

```

In the `ViewController.m` file define the following constants

```swift
#define DefaultFontSize 48
#define PaddingFactor 0.1f

```

The function which prints the text is as follows:-

```swift
-(IBAction)print:(id)sender;
{
    /* Get the UIPrintInteractionController, which is a shared object */
    UIPrintInteractionController *controller = [UIPrintInteractionController sharedPrintController];
    if(!controller){
        NSLog(@"Couldn't get shared UIPrintInteractionController!");
        return;
    }
    
    /* Set this object as delegate so you can  use the printInteractionController:cutLengthForPaper: delegate */
    controller.delegate = self;
        
    UIPrintInfo *printInfo = [UIPrintInfo printInfo];
    printInfo.outputType = UIPrintInfoOutputGeneral;

    /* Use landscape orientation for a banner so the text  print along the long side of the paper. */
    printInfo.orientation = UIPrintInfoOrientationLandscape;

    printInfo.jobName = self.textField.text;
    controller.printInfo = printInfo;
    
    /* Create the UISimpleTextPrintFormatter with the text supplied by the user in the text field */
    _textFormatter = [[UISimpleTextPrintFormatter alloc] initWithText:self.textField.text];
    
    /* Set the text formatter's color and font properties based on what the user chose */
    _textFormatter.color = [self chosenColor];
    _textFormatter.font = [self chosenFontWithSize:DefaultFontSize];
    
    /* Set this UISimpleTextPrintFormatter on the controller */
    controller.printFormatter = _textFormatter;
    
    /* Set up a completion handler block.  If the print job has an error before spooling, this is where it's handled. */
    void (^completionHandler)(UIPrintInteractionController *, BOOL, NSError *) = ^(UIPrintInteractionController *printController, BOOL completed, NSError *error) {
        if(completed && error)
            NSLog( @"Printing failed due to error in domain %@ with error code %lu. Localized description: %@, and failure reason: %@", error.domain, (long)error.code, error.localizedDescription, error.localizedFailureReason );
    };

    if (UI_USER_INTERFACE_IDIOM() == UIUserInterfaceIdiomPad)
        [controller presentFromRect:self.printButton.frame inView:self.view animated:YES completionHandler:completionHandler];
    else
        [controller presentAnimated:YES completionHandler:completionHandler];  // iPhone
}

```

The delegate function which sets up the print page:-

```swift
- (CGFloat)printInteractionController:(UIPrintInteractionController *)printInteractionController cutLengthForPaper:(UIPrintPaper *)paper {

    /* Create a font with arbitrary size so that you can calculate the approximate
        font points per screen point for the height of the text. */
    UIFont *font = _textFormatter.font;
    CGSize size = [self.textField.text sizeWithAttributes:@{NSFontAttributeName: font}];
    
    float approximateFontPointPerScreenPoint = font.pointSize / size.height;
    
    /* Create a new font using a size  that will fill the width of the paper */
    font = [self chosenFontWithSize: paper.printableRect.size.width * approximateFontPointPerScreenPoint];
    
    /* Calculate the height and width of the text with the final font size */
    CGSize finalTextSize = [self.textField.text sizeWithAttributes:@{NSFontAttributeName: font}];
    
    /* Set the UISimpleTextFormatter font to the font with the size calculated */
    _textFormatter.font = font;
    
    /* Calculate the margins of the roll. Roll printers may have unprintable areas
        before and after the cut.  We must add this to our cut length to ensure the
        printable area has enough room for our text. */
    CGFloat lengthOfMargins = paper.paperSize.height - paper.printableRect.size.height;

    /* The cut length is the width of the text, plus margins, plus some padding */
    return finalTextSize.width + lengthOfMargins + paper.printableRect.size.width * PaddingFactor;
}

```

