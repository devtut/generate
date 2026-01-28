---
metaTitle: "Android - Snackbar"
description: "Creating a simple Snackbar, Custom Snack Bar, Snackbar with Callback, Custom Snackbar, Snackbar vs Toasts: Which one should I use?, Custom Snackbar (no need view)"
---

# Snackbar




## Creating a simple Snackbar


Creating a `Snackbar` can be done as follows:

```java
Snackbar.make(view, "Text to display", Snackbar.LENGTH_LONG).show();

```

The `view` is used to find a suitable parent to use to display the `Snackbar`. Typically this would be a `CoordinatorLayout` that you've defined in your XML, which enables adding functionality such as swipe to dismiss and automatically moving of other widgets (e.g. `FloatingActionButton`). If there's no `CoordinatorLayout` then the window decor's content view is used.

Very often we also add an action to the `Snackbar`. A common use case would be an "Undo" action.

```java
Snackbar.make(view, "Text to display", Snackbar.LENGTH_LONG)
        .setAction("UNDO", new View.OnClickListener() {
            @Override
               public void onClick(View view) {
                   // put your logic here

               }
         })
        .show();

```

You can create a `Snackbar` and show it later:

```java
Snackbar snackbar = Snackbar.make(view, "Text to display", Snackbar.LENGTH_LONG); 
snackbar.show();

```

If you want to change the color of the `Snackbar`'s text:

```java
Snackbar snackbar = Snackbar.make(view, "Text to display", Snackbar.LENGTH_LONG);
View view = snackbar .getView();
TextView textView = (TextView) view.findViewById(android.support.design.R.id.snackbar_text);
textView.setTextColor(Color.parseColor("#FF4500"));
snackbar.show();

```

By default `Snackbar` dismisses on it's right swipe.This example demonstrates how to [dismiss the snackBar on it's left swipe](http://stackoverflow.com/a/41790613/3732887).



## Custom Snack Bar


Function to customize snackbar

```java
public static Snackbar makeText(Context context, String message, int duration) {
        Activity activity = (Activity) context;
        View layout;
        Snackbar snackbar = Snackbar
                .make(activity.findViewById(android.R.id.content), message, duration);
        layout = snackbar.getView();
        //setting background color
        layout.setBackgroundColor(context.getResources().getColor(R.color.orange));
        android.widget.TextView text = (android.widget.TextView) layout.findViewById(android.support.design.R.id.snackbar_text);
        //setting font color
        text.setTextColor(context.getResources().getColor(R.color.white));
        Typeface font = null;
        //Setting font 
        font = Typeface.createFromAsset(context.getAssets(), "DroidSansFallbackanmol256.ttf");
        text.setTypeface(font);
        return snackbar;

    }

```

Call the function from fragment or activity 

```

SnackBar.makeText(MyActivity.this, "Please Locate your address at Map", Snackbar.LENGTH_SHORT).show();

```



## Snackbar with Callback


You can use Snackbar.Callback to listen if the snackbar was dismissed by user or timeout.

```java
Snackbar.make(getView(), "Hi snackbar!", Snackbar.LENGTH_LONG).setCallback( new Snackbar.Callback() {
                @Override
                public void onDismissed(Snackbar snackbar, int event) {
                    switch(event) {
                        case Snackbar.Callback.DISMISS_EVENT_ACTION:
                            Toast.makeText(getActivity(), "Clicked the action", Toast.LENGTH_LONG).show();
                            break;
                        case Snackbar.Callback.DISMISS_EVENT_TIMEOUT:
                            Toast.makeText(getActivity(), "Time out", Toast.LENGTH_LONG).show();
                            break;
                    }
                }

                @Override
                public void onShown(Snackbar snackbar) {
                    Toast.makeText(getActivity(), "This is my annoying step-brother", Toast.LENGTH_LONG).show();
                }
            }).setAction("Go!", new View.OnClickListener() {
                @Override
                public void onClick(View v) {

                }
            }).show();

```



## Custom Snackbar


This example shows a white Snackbar with custom Undo icon.

```java
Snackbar customBar = Snackbar.make(view , "Text to be displayed", Snackbar.LENGTH_LONG);
customBar.setAction("UNDO", new View.OnClickListener() {
    @Override
    public void onClick(View view) {
    //Put the logic for undo button here

    }
});

View sbView = customBar.getView();
//Changing background to White
sbView.setBackgroundColor(Color.WHITE));

TextView snackText = (TextView) sbView.findViewById(android.support.design.R.id.snackbar_text); 
if (snackText!=null) {
    //Changing text color to Black
   snackText.setTextColor(Color.BLACK);
}

TextView actionText = (TextView) sbView.findViewById(android.support.design.R.id.snackbar_action);
if (actionText!=null) {
    // Setting custom Undo icon
    actionText.setCompoundDrawablesRelativeWithIntrinsicBounds(R.drawable.custom_undo, 0, 0, 0);
}
customBar.show();

```



## Snackbar vs Toasts: Which one should I use?


Toasts are generally used when we want to display an information to the user regarding some action that has successfully (or not) happened and this action does not require the user to take any other action. Like when a message has been sent, for example:

```java
Toast.makeText(this, "Message Sent!", Toast.LENGTH_SHORT).show();

```

Snackbars are also used to display an information. But this time, we can give the user an opportunity to take an action. For example, let's say the user deleted a picture by mistake and he wants to get it back. We can provide a Snackbar with the "Undo" action. Like this:

```java
Snackbar.make(getCurrentFocus(), "Picture Deleted", Snackbar.LENGTH_SHORT)
        .setAction("Undo", new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                //Return his picture
            }
        })
        .show();

```

Conclusion: Toasts are used when we don't need user interaction. Snackbars are used to allow users to take another action or undo a previous one.



## Custom Snackbar (no need view)


Creating an Snackbar without the need pass view to Snackbar, all layout create in android in android.R.id.content.

```java
public class CustomSnackBar {

    public static final int STATE_ERROR = 0;
    public static final int STATE_WARNING = 1;
    public static final int STATE_SUCCESS = 2;
    public static final int VIEW_PARENT = android.R.id.content;

    public CustomSnackBar(View view, String message, int actionType) {
        super();

        Snackbar snackbar = Snackbar.make(view, message, Snackbar.LENGTH_LONG);
        View sbView = snackbar.getView();
        TextView textView = (TextView) sbView.findViewById(android.support.design.R.id.snackbar_text);
        textView.setTextColor(Color.parseColor("#ffffff"));
        textView.setTextSize(TypedValue.COMPLEX_UNIT_SP, 14);
        textView.setGravity(View.TEXT_ALIGNMENT_CENTER);
        textView.setLayoutDirection(View.LAYOUT_DIRECTION_RTL);

        switch (actionType) {
            case STATE_ERROR:
                snackbar.getView().setBackgroundColor(Color.parseColor("#F12B2B"));
                break;
            case STATE_WARNING:
                snackbar.getView().setBackgroundColor(Color.parseColor("#000000"));
                break;
            case STATE_SUCCESS:
                snackbar.getView().setBackgroundColor(Color.parseColor("#7ED321"));
                break;
        }
        snackbar.show();
    }
}

```

for call class

new CustomSnackBar(findViewById(CustomSnackBar.VIEW_PARENT),"message", CustomSnackBar.STATE_ERROR);



#### Syntax


- Snackbar make (View view, CharSequence text, int duration)
- Snackbar make (View view, int resId, int duration)



#### Parameters


|Parameter|Description
|---|---|---|---|---|---|---|---|---|---
|view|View: The view to find a parent from.
|text|CharSequence: The text to show. Can be formatted text.
|resId|int: The resource id of the string resource to use. Can be formatted text.
|duration|int: How long to display the message. This can be LENGTH_SHORT, LENGTH_LONG or LENGTH_INDEFINITE



#### Remarks


[Snackbar](http://developer.android.com/reference/android/support/design/widget/Snackbar.html) provides lightweight feedback about an operation. It displays a brief message at the bottom of the screen on mobile and at the bottom left on larger devices.   Snackbars appear above all other elements on the screen and only one can be displayed at a time.

They automatically disappear after a timeout or after user interaction elsewhere on the screen, particularly after interactions that summon a new surface or activity. Snackbar can be swiped off screen.

Before using `SnackBar` you must add the design support library dependency in the `build.gradle` file:

```java
dependencies {
    compile 'com.android.support:design:25.3.1'
}

```

### Official Documentation

[https://developer.android.com/reference/android/support/design/widget/Snackbar.html](https://developer.android.com/reference/android/support/design/widget/Snackbar.html)

