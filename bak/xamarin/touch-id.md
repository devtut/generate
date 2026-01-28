---
metaTitle: "Xamarin - Touch ID"
description: "Add Touch ID to your App, Using Keychain"
---

# Touch ID



## Add Touch ID to your App


First, establish if the device is capable of accepting Touch ID input.

```cs
if (context.CanEvaluatePolicy(LAPolicy.DeviceOwnerAuthenticationWithBiometrics, out AuthError))

```

If it does then we can display the Touch ID UI by using:

```cs
context.EvaluatePolicy(LAPolicy.DeviceOwnerAuthenticationWithBiometrics, myReason, replyHandler);

```

There are three pieces of information we have to pass into `EvaluatePolicy` – the policy itself, a string explaining why authentication is necessary, and a reply handler. The reply handler tells the application what it should do in the case of a successful, or unsuccessful, authentication.

One of the caveats of Local Authentication is that it must be run on the foreground, so make sure to use `InvokeOnMainThread` for the reply handler:

```cs
var replyHandler = new LAContextReplyHandler((success, error) =>
{
    this.InvokeOnMainThread(() =>
    {
        if (success)
        {
            Console.WriteLine("You logged in!");
            PerformSegue("AuthenticationSegue", this);
        }
        else {
            //Show fallback mechanism here
        }
    });
});

```

To determine whether the database of authorized fingerprints has been modified you can check the opaque structure (NSData) returned by `context.EvaluatedPolicyDomainState`. Your app will need to store and compare the policy state to detect changes. One thing to note which Apple states:

> 
However, the nature of the change cannot be determined from this data.


```cs
if (context.CanEvaluatePolicy(LAPolicy.DeviceOwnerAuthenticationWithBiometrics, out AuthError))
{
    var policyState = context.EvaluatedPolicyDomainState;

    var replyHandler = new LAContextReplyHandler((success, error) =>
    {

        this.InvokeOnMainThread(() =>
        {
            if (success)
            {
                Console.WriteLine("You logged in!");
                PerformSegue("AuthenticationSegue", this);
            }
            else {
                //Show fallback mechanism here
            }
        });

    });
    context.EvaluatePolicy(LAPolicy.DeviceOwnerAuthenticationWithBiometrics, myReason, replyHandler);
};

```

**Button Example**

```cs
partial void AuthenticateMe(UIButton sender)
{
    var context = new LAContext();
    //Describes an authentication context 
    //that allows apps to request user authentication using Touch ID.
    NSError AuthError;
    //create the reference for error should it occur during the authentication.
    var myReason = new NSString("To add a new chore");
    //this is the string displayed at the window for touch id

    if (context.CanEvaluatePolicy(LAPolicy.DeviceOwnerAuthenticationWithBiometrics, out AuthError)) 
    // check if the device have touchId capabilities.
    {
        var replyHandler = new LAContextReplyHandler((success, error) =>
        {

            this.InvokeOnMainThread(() =>
            {
                if (success)
                {
                    Console.WriteLine("You logged in!");
                    PerformSegue("AuthenticationSegue", this);
                }
                else {
                    //Show fallback mechanism here
                }
            });

        });
        context.EvaluatePolicy(LAPolicy.DeviceOwnerAuthenticationWithBiometrics, myReason, replyHandler);//send touch id request
    };
}

```



## Using Keychain


Working Source - [https://github.com/benhysell/V.TouchIdExample](https://github.com/benhysell/V.TouchIdExample)

Long form description - [http://benjaminhysell.com/archive/2014/11/authentication-in-xamarin-ios-with-touch-id-or-passcode/](http://benjaminhysell.com/archive/2014/11/authentication-in-xamarin-ios-with-touch-id-or-passcode/)

```cs
//Simple View with a switch to enable / disable Touch ID and 
//a button to invoke authentication 

/// <summary>
/// Enable/Disable Touch ID
/// </summary>
/// <param name="sender">Sender.</param>
partial void TouchIdEnableDisable(UISwitch sender)
{
    if (sender.On)
    {
        //enable Touch ID          
        //set our record
        //note what you fill in here doesn't matter, just needs to be 
        //consistent across all uses of the record
        var secRecord = new SecRecord(SecKind.GenericPassword)
        {
            Label = "Keychain Item",
            Description = "fake item for keychain access",
            Account = "Account",
            Service = "com.yourcompany.touchIdExample",
            Comment = "Your comment here",
            ValueData = NSData.FromString("my-secret-password"),
            Generic = NSData.FromString("foo")
        };

        secRecord.AccessControl = new SecAccessControl(SecAccessible.WhenPasscodeSetThisDeviceOnly, SecAccessControlCreateFlags.UserPresence);
        SecKeyChain.Add(secRecord);
                                
        authenticateButton.Enabled = true;
    }
    else
    {
        //disable Touch ID
        var record = new SecRecord(SecKind.GenericPassword)
        {
            Service = "com.yourcompany.touchIdExample",
            UseOperationPrompt = "Authenticate to Remove Touch ID / Passcode from Test App"
        };

        SecStatusCode result;

        //query one last time to ensure they can remove it
        SecKeyChain.QueryAsRecord(record, out result);
        if (SecStatusCode.Success == result || SecStatusCode.ItemNotFound == result)
        {
            //remove the record
            SecKeyChain.Remove(record);
            authenticateButton.Enabled = false;
        }
        else
        {
            //could not authenticate, leave switch on
            sender.On = true;
         }                
    }
}

/// <summary>
/// Show Touch ID to user and evaluate authentication
/// </summary>
/// <param name="sender">Sender.</param>
partial void AuthenticateUser(UIButton sender)
{
    var rec = new SecRecord(SecKind.GenericPassword)
    {
        Service = "com.yourcompany.touchIdExample",
        UseOperationPrompt = "Authenticate to access Test App"
    };
    SecStatusCode res;
    SecKeyChain.QueryAsRecord(rec, out res);
    if (SecStatusCode.Success == res || SecStatusCode.ItemNotFound == res)
    {
        //Success!!  
        //add your code here to continue into your application
        AuthenticatedLabel.Hidden = false;
    }
    else
    {
        //Failure
        AuthenticatedLabel.Hidden = true;
    }
}

```



#### Parameters


|Column|Column
|---|---|---|---|---|---|---|---|---|---
|Cell|Cell



#### Remarks


First, establish if the device is capable of accepting Touch ID input.

```cs
if (context.CanEvaluatePolicy(LAPolicy.DeviceOwnerAuthenticationWithBiometrics, out AuthError))

```

If it does then we can display the Touch ID UI by using:

```cs
context.EvaluatePolicy(LAPolicy.DeviceOwnerAuthenticationWithBiometrics, myReason, replyHandler);

```

There are three pieces of information we have to pass into `EvaluatePolicy` – the policy itself, a string explaining why authentication is necessary, and a reply handler. The reply handler tells the application what it should do in the case of a successful, or unsuccessful, authentication.

One of the caveats of Local Authentication is that it must be run on the foreground, so make sure to use `InvokeOnMainThread` for the reply handler:

```cs
var replyHandler = new LAContextReplyHandler((success, error) =>
    {

        this.InvokeOnMainThread(() =>
        {
            if (success)
            {
                Console.WriteLine("You logged in!");
                PerformSegue("AuthenticationSegue", this);
            }
            else {
                //Show fallback mechanism here
            }
        });

    });

```

To determine whether the database of authorized fingerprints has been modified you can check the opaque structure (NSData) returned by `context.EvaluatedPolicyDomainState`. Your app will need to store and compare the policy state to detect changes. One thing to note which Apple states:

> 
However, the nature of the change cannot be determined from this data.


```cs
if (context.CanEvaluatePolicy(LAPolicy.DeviceOwnerAuthenticationWithBiometrics, out AuthError))
{
    var policyState = context.EvaluatedPolicyDomainState;

    var replyHandler = new LAContextReplyHandler((success, error) =>
    {

        this.InvokeOnMainThread(() =>
        {
            if (success)
            {
                Console.WriteLine("You logged in!");
                PerformSegue("AuthenticationSegue", this);
            }
            else {
                //Show fallback mechanism here
            }
        });

    });
    context.EvaluatePolicy(LAPolicy.DeviceOwnerAuthenticationWithBiometrics, myReason, replyHandler);
};

```

