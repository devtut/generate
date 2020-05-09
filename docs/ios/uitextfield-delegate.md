---
metaTitle: "iOS - UITextField Delegate"
description: "UITextField - Restrict textfield to certain characters, Actions when a user has started/ended interacting with a textfield, Find Next Tag & Manage Keyboard"
---

# UITextField Delegate




## UITextField - Restrict textfield to certain characters


If you want to perform a user input validation of your textfield use the following code snippet:

```swift
// MARK: - UITextFieldDelegate

let allowedCharacters = CharacterSet(charactersIn:"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvxyz").inverted    

func textField(_ textField: UITextField, shouldChangeCharactersIn range: NSRange, replacementString string: String) -> Bool {
    
    let components = string.components(separatedBy: allowedCharacters)
    let filtered = components.joined(separator: "")
    
    if string == filtered {
        
        return true

    } else {
        
        return false
    }
}

```

**Objective-C**

```swift
#define ACCEPTABLE_CHARACTERS @"0123456789 ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

- (BOOL)textField:(UITextField *)textField shouldChangeCharactersInRange:(NSRange)range replacementString:(NSString *)string  
{
      NSCharacterSet *cs = [[NSCharacterSet characterSetWithCharactersInString:ACCEPTABLE_CHARACTERS] invertedSet];

      NSString *filtered = [[string componentsSeparatedByCharactersInSet:cs] componentsJoinedByString:@""];

      return [string isEqualToString:filtered];
}

```

In addition you can also use character sets provided by apple to perform validation:

Take a look at [https://developer.apple.com/reference/foundation/nscharacterset](https://developer.apple.com/reference/foundation/nscharacterset)

```swift
let allowedCharacters = CharacterSet.alphanumerics.inverted
let allowedCharacters = CharacterSet.capitalizedLetters.inverted

```



## Actions when a user has started/ended interacting with a textfield


For Swift 3.1:

In the first example one can see how you would intercept the user interacting with a textfield while writing. Similarly, there are methods in the [UITextFieldDelegate](https://developer.apple.com/reference/uikit/uitextfielddelegate) that are called when a user has started and ended his interaction with a TextField.

To be able to access these methods, you need to conform to the [UITextFieldDelegate](https://developer.apple.com/reference/uikit/uitextfielddelegate) protocol, and for each textfield you want to be notified about, assign the parent class as the delegate:

```swift
class SomeClass: UITextFieldDelegate {
    
    @IBOutlet var textField: UITextField!

    override func viewDidLoad() {
        super.viewDidLoad()
        textField.delegate = self
    }

}

```

Now you will be able to implement all the UITextFieldDelegate methods.

To be notified when a user has started editing a textfield, you can implement [textFieldDidBeginEditing(_:)](https://developer.apple.com/reference/uikit/uitextfielddelegate/1619590-textfielddidbeginediting) method like so:

```swift
func textFieldDidBeginEditing(_ textField: UITextField) {
    // now you can perform some action 
    // if you have multiple textfields in a class, 
    // you can compare them here to handle each one separately
    if textField == emailTextField {
        // e.g. validate email 
    } 
    else if textField == passwordTextField {
        // e.g. validate password 
    } 
}

```

Similarly, being notified if a user has ended interaction with a textfield, you can use the [textFieldDidEndEditing(_:)](https://developer.apple.com/reference/uikit/uitextfielddelegate/1619591-textfielddidendediting) method like so:

```swift
func textFieldDidEndEditing(_ textField: UITextField) {
    // now you can perform some action 
    // if you have multiple textfields in a class, 
    // you can compare them here to handle each one separately
    if textField == emailTextField {
        // e.g. validate email 
    } 
    else if textField == passwordTextField {
        // e.g. validate password 
    } 
}

```

If you want to have control over whether a TextField should begin/end editing, the [textFieldShouldBeginEditing(_:)](https://developer.apple.com/reference/uikit/uitextfielddelegate/1619601-textfieldshouldbeginediting) and [textFieldShouldEndEditing(_:)](https://developer.apple.com/reference/uikit/uitextfielddelegate/1619592-textfieldshouldendediting) methods can be used by return true/false based on your needed logic.



## Find Next Tag & Manage Keyboard


The text field calls different delegate methods (only if delegates are set)One of delegate method called by textfield is  ***- (BOOL)textFieldShouldReturn:(UITextField **)textField****

This method is called whenever users taps the return button.By using this method,
we can implement any custom behaviour.

For Example,

> 
<p>In the below example ,next responder will be find out on the basis of
tag and manage the keyboard. Here 20 is the constant,As tag assigned
to textfield are like this 50,70,90 etc.</p>


**Here on finding a new textfield object as responder,it will make current text field as new responder and open keyboard accordingly.**

```

- (BOOL)textFieldShouldReturn:(UITextField *)textField {

                NSInteger nextTag = textField.tag+20;
                // Try to find next responder
                UIResponder *nextResponder = [textField.superview viewWithTag:nextTag];
                if (nextResponder)
                {
                    // Found next responder, so set it.
                    [nextResponder becomeFirstResponder];
                }
                else
                {
                    // Not found, so remove keyboard.
                    [textField resignFirstResponder];
                }
                return YES;
            }

```

