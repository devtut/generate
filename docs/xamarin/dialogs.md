---
metaTitle: "Xamarin - Dialogs"
description: "AlertDialog, Simple Alert Dialog Example"
---

# Dialogs




## AlertDialog


```cs
// 1. Instantiate an AlertDialog.Builder with its constructor
// the parameter this is the context (usually your activity)
AlertDialog.Builder builder = new AlertDialog.Builder(this);

// 2. Chain together various setter methods to set the dialog characteristics 
builder.SetMessage(Resource.String.dialog_message)
       .SetTitle(Resource.String.dialog_title);

// 3. Get the AlertDialog from create()
AlertDialog dialog = builder.Create();

dialog.Show();

```



## Simple Alert Dialog Example


We shall create a simple Alert Dialog in Xamarin.Android

Now considering you have gone through the [getting started guide](http://stackoverflow.com/documentation/xamarin.android/topics) from the documentation.

> 
You must be having the project structure like this:


[<img src="http://i.stack.imgur.com/rRaYy.png" alt="Project Structure" />](http://i.stack.imgur.com/rRaYy.png)

> 
Your Main Activity must be looking like this:


```

public class MainActivity : Activity
 {
 int count = 1;
 
 protected override void OnCreate(Bundle bundle)
 {
 base.OnCreate(bundle);
 
 // Set our view from the "main" layout resource
 SetContentView(Resource.Layout.Main);
 
 // Get our button from the layout resource,
 // and attach an event to it
 Button button = FindViewById<Button>(Resource.Id.MyButton);
 
 button.Click += delegate { button.Text = string.Format("{0} clicks!", count++); };
 }
 }

```

Now What we shall do is, instead of adding one to the counter on button click, we shall ask user if he wants to add or substract one in a simple Alert Dialog

And on Click of the Positive or the negative button we will take the action.

```

button.Click += delegate {
 AlertDialog.Builder alert = new AlertDialog.Builder(this);
 alert.SetTitle("Specify Action");
 alert.SetMessage("Do you want to add or substract?");

 alert.SetPositiveButton("Add", (senderAlert, args) =>
 {
 count++;
 button.Text = string.Format("{0} clicks!", count);
  });

  alert.SetNegativeButton("Substract", (senderAlert, args) =>
  {
  count--;
  button.Text = string.Format("{0} clicks!", count);
  });

  Dialog dialog = alert.Create();
      dialog.Show();
 };

```

> 
screenshot:


[<img src="http://i.stack.imgur.com/0DJFx.png" alt="enter image description here" />](http://i.stack.imgur.com/0DJFx.png)



#### Parameters


|commonly used Public Method|Use
|---|---|---|---|---|---|---|---|---|---
|SetTitle(String)|Sets Title for the dialog
|SetIcon(Drawable)|Set Icon for the alert dialog
|SetMessage(string)|Set the message to display.
|SetNegativeButton(String, EventHandler)|Set a listener to be invoked when the negative button of the dialog is pressed.
|SetPositiveButton(String, EventHandler)|Set a listener to be invoked when the positive button of the dialog is pressed.
|SetNeutralButton(String, EventHandler)|Set a listener to be invoked when the neutral button of the dialog is pressed.
|SetOnCancelListener(IDialogInterfaceOnCancelListener)|Sets the callback that will be called if the dialog is canceled.
|SetOnDismissListener(IDialogInterfaceOnDismissListener)|Sets the callback that will be called when the dialog is dismissed for any reason.
|Show()|Creates a AlertDialog with the arguments supplied to this builder and Dialog.Show's the dialog.



#### Remarks


**Requirements**

Namespace: Android.App

Assembly: Mono.Android (in Mono.Android.dll)

Assembly Versions: 0.0.0.0

**Public Constructors**

AlertDialog.Builder(Context) :-

Constructor using a context for this builder and the AlertDialog it creates.

AlertDialog.Builder(Context, Int32) :-

Constructor using a context and theme for this builder and the AlertDialog it creates.

**Using Material Design AlertDialog**

In order to use the modern AlertDialog:

1. Install Support v7 AppCompat library from the NuGet packages
<li>Replace AlertDialog with Android.Support.V7.App.AlertDialog or add the
following statement at the top to make your dialog shine.</li>

```

       using AlertDialog = Android.Support.V7.App.AlertDialog;

```

