---
metaTitle: "Xamarin - Alerts"
description: "Display an Alert, Display a login alert, Display an Action Sheet, Display Modal Alert Dialog"
---

# Alerts



## Display an Alert


For Alerts since iOS 8, you would use a `UIAlertController` but for versions before, you would have used a `UIAlertView`, which is now deprecated.

```cs
var alert = UIAlertController.Create(title, message, UIAlertControllerStyle.Alert);
alert.AddAction(UIAlertAction.Create(otherTitle, UIAlertActionStyle.Destructive, (action) => {
    // otherTitle();
}));
alert.AddAction(UIAlertAction.Create(cancelTitle, UIAlertActionStyle.Cancel, null));
this.PresentViewController(alert, true, null);

```

```cs
var alert = new UIAlertView (title, message, null, cancelTitle, otherTitle);
alert.Clicked += (object sender, UIButtonEventArgs e) => {
    if(e.ButtonIndex == 1)
       // otherTitle();
};
alert.Show ();

```



## Display a login alert


The following code is for iOS 8 and lower for creating a login alert.

```cs
// Create the UIAlertView
var loginAlertView = new UIAlertView(title, message, null, cancelTitle, okTitle);

// Setting the AlertViewStyle to UIAlertViewStyle.LoginAndPasswordInput
loginAlertView.AlertViewStyle = UIAlertViewStyle.LoginAndPasswordInput;

// Getting the fields Username and Password
var usernameTextField = loginAlertView.GetTextField(0);
var passwordTextField = loginAlertView.GetTextField(1);

// Setting a placeholder
usernameTextField.Placeholder = "user@stackoverflow.com";
passwordTextField.Placeholder = "Password";

// Adding the button click handler.
loginAlertView.Clicked += (alertViewSender, buttonArguments) =>
{
    
    // Check if cancel button is pressed
    if (buttonArguments.ButtonIndex == loginAlertView.CancelButtonIndex)
    {
        // code
    }
    
    // In our case loginAlertView.FirstOtherButtonIndex is equal to the OK button
    if (buttonArguments.ButtonIndex == loginAlertView.FirstOtherButtonIndex)
    {
        // code
    }
};

// Show the login alert dialog
loginAlertView.Show();

```



## Display an Action Sheet


The `UIAlertController` available since iOS8 allows you to use the same alert object for either Action sheets or more classic alerts. The only difference is the `UIAlertControllerStyle` passed as a parameter when creating.

This line changes from an AlertView to an ActionSheet, compared to some other examples available here :

```cs
var alert = UIAlertController.Create(title, message, UIAlertControllerStyle.ActionSheet);

```

The way you add actions to the controller is still the same :

```cs
alert.AddAction(UIAlertAction.Create(otherTitle, UIAlertActionStyle.Destructive, (action) => {
    // ExecuteSomeAction();
}));
alert.AddAction(UIAlertAction.Create(cancelTitle, UIAlertActionStyle.Cancel, null));

//Add additional actions if necessary

```

Note that if you have a parameterless void method, you can use it as the last parameter of the `.AddAction()`.

For example, let's assume I want the code of `private void DoStuff(){...}` to be executed when I press "OK" :

```cs
UIAlertAction action = UIAlertAction.Create("OK", UIAlertActionStyle.Cancel, DoStuff);
alert.AddAction(action);

```

Notice I'm not using the () after DoStuff in the creation of the action.

The way you present the controller is done the same way as any other controller :

```cs
this.PresentViewController(alert, true, null);

```



## Display Modal Alert Dialog


It was common practice to use `NSRunLoop` to show modal `UIAlertView` to block code execution until user input is processed in iOS; until Apple released the iOS7, it broke few existing apps. Fortunately, there is a better way of implementing it with C#’s async/await.

Here’s the new code taking advantage of async/await pattern to show modal UIAlertView:

```cs
Task ShowModalAletViewAsync (string title, string message, params string[] buttons)
{
    var alertView = new UIAlertView (title, message, null, null, buttons);
    alertView.Show ();
    var tsc = new TaskCompletionSource ();

    alertView.Clicked += (sender, buttonArgs) => {
        Console.WriteLine ("User clicked on {0}", buttonArgs.ButtonIndex);
        tsc.TrySetResult(buttonArgs.ButtonIndex);
    };
    return tsc.Task;
}

//Usage
async Task PromptUser() {
    var result = await ShowModalAletViewAsync 
               ("Alert", "Do you want to continue?", "Yes", "No"); //process the result
}

```

