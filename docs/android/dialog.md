---
metaTitle: "Android - Dialog"
description: "Adding Material Design AlertDialog to your app using Appcompat, A Basic Alert Dialog, Alert Dialog , DatePickerDialog, DatePicker, ListView in AlertDialog, Custom Alert Dialog with EditText, Alert Dialog with Multi-line Title, Date Picker within DialogFragment, Fullscreen Custom Dialog with no background and no title"
---

# Dialog




## Adding Material Design AlertDialog to your app using Appcompat


`AlertDialog` is a subclass of `Dialog` that can display one, two or three buttons. If you only want to display a String in this dialog box, use the `setMessage()` method.

The `AlertDialog` from `android.app` package displays differently on different Android OS Versions.

The Android V7 Appcompat library provides an `AlertDialog` implementation which will display with Material Design on all supported Android OS versions, as shown below:

[<img src="https://i.stack.imgur.com/7mLMs.png" alt="Material AlertDialog" />](https://i.stack.imgur.com/7mLMs.png)

First you need to add the V7 Appcompat library to your project. you can do this in the app level build.gradle file:

```java
dependencies {
    compile 'com.android.support:appcompat-v7:24.2.1'
    //........
}

```

Be sure to import the correct class:

```java
import android.support.v7.app.AlertDialog;

```

Then Create AlertDialog like this:

```java
AlertDialog.Builder builder = new AlertDialog.Builder(this);
builder.setTitle("Are you sure?");
builder.setMessage("You'll lose all photos and media!");
builder.setPositiveButton("ERASE", null);
builder.setNegativeButton("CANCEL", null);
builder.show();

```



## A Basic Alert Dialog


```

   AlertDialog.Builder builder = new AlertDialog.Builder(context);
    //Set Title
    builder.setTitle("Reset...")
            //Set Message
            .setMessage("Are you sure?")
            //Set the icon of the dialog
            .setIcon(drawable)
            //Set the positive button, in this case, OK, which will dismiss the dialog and do everything in the onClick method
            .setPositiveButton(android.R.string.ok, new DialogInterface.OnClickListener() {
                @Override
                public void onClick(DialogInterface dialogInterface, int i) {
                    // Reset
                }
            });
    AlertDialog dialog = builder.create();
    //Now, any time you can call on:
    dialog.show();
    //So you can show the dialog.

```

Now this code will achieve this: <br>
[<img src="https://i.stack.imgur.com/Pdcb7.jpg" alt="Dialog Image" />](https://i.stack.imgur.com/Pdcb7.jpg)
<br>
(**Image source: WikiHow**)



## Alert Dialog 


```java
AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(
                MainActivity.this);

        alertDialogBuilder.setTitle("Title Dialog");
        alertDialogBuilder
                .setMessage("Message Dialog")
                .setCancelable(true)
                .setPositiveButton("Yes",
                        new DialogInterface.OnClickListener() {

                            public void onClick(DialogInterface dialog, int arg1) {
                                // Handle Positive Button
         
                            }
                        })
                .setNegativeButton("No",
                        new DialogInterface.OnClickListener() {

                            public void onClick(DialogInterface dialog, int arg1) {
                                // Handle Negative Button
                                dialog.cancel();
                            }
                        });

        AlertDialog alertDialog = alertDialogBuilder.create();
        alertDialog.show();

```



## DatePickerDialog


`DatePickerDialog` is the simplest way to use `DatePicker`, because you can show dialog anywhere in your app. You don't have to implement your own layout with `DatePicker` widget.

How to show dialog:

```java
DatePickerDialog datePickerDialog = new DatePickerDialog(context, listener, year, month, day);
datePickerDialog.show();

```

You can get `DataPicker` widget from dialog above, to get access to more functions, and for example set minimum date in milliseconds:

```java
DatePicker datePicker = datePickerDialog.getDatePicker();
datePicker.setMinDate(System.currentTimeMillis());

```



## DatePicker


`DatePicker` allows user to pick date.
When we create new instance of `DatePicker`, we can set initial date. If we don't set initial date, current date will be set by default.

We can show `DatePicker` to user by using `DatePickerDialog` or by creating our own layout with `DatePicker` widget.

Also we can limit range of date, which user can pick.

> 
By setting minimum date in milliseconds


```java
//In this case user can pick date only from future
datePicker.setMinDate(System.currentTimeMillis());

```

> 
By setting maximum date in milliseconds


```java
//In this case user can pick date only, before following week.
datePicker.setMaxDate(System.currentTimeMillis() + TimeUnit.DAYS.toMillis(7));

```

To receive information, about which date was picked by user, we have to use `Listener`.

If we are using `DatePickerDialog`, we can set `OnDateSetListener` in constructor when we are creating new instance of `DatePickerDialog`:

### Sample use of `DatePickerDialog`

```java
public class SampleActivity extends AppCompatActivity implements DatePickerDialog.OnDateSetListener {

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        ...
    }

    private void showDatePicker() {
        //We need calendar to set current date as initial date in DatePickerDialog.
        Calendar calendar = new GregorianCalendar(Locale.getDefault());
        int year = calendar.get(Calendar.YEAR);
        int month = calendar.get(Calendar.MONTH);
        int day = calendar.get(Calendar.DAY_OF_MONTH);

        DatePickerDialog datePickerDialog = new DatePickerDialog(this, this, year, month, day);
        datePickerDialog.show();
    }

    @Override
    public void onDateSet(DatePicker datePicker, int year, int month, int day) {

    }
}

```

Otherwise, if we are creating our own layout with `DatePicker` widget, we also have to create our own listener as it was shown in [other example](http://stackoverflow.com/documentation/android/2836/date-pickers/9614/date-picker-within-dialogfragment#t=2016072402344308997)



## ListView in AlertDialog


We can always use `ListView` or `RecyclerView` for selection from list of items, but if we have small amount of choices and among those choices we want user to select one, we can use `AlertDialog.Builder setAdapter`.

```

   private void showDialog()
    {
        AlertDialog.Builder builder = new AlertDialog.Builder(this);
        builder.setTitle("Choose any item");

        final List<String> lables = new ArrayList<>();
        lables.add("Item 1");
        lables.add("Item 2");
        lables.add("Item 3");
        lables.add("Item 4");

        ArrayAdapter<String> dataAdapter = new ArrayAdapter<String>(this,
            android.R.layout.simple_dropdown_item_1line, lables);
        builder.setAdapter(dataAdapter, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                Toast.makeText(MainActivity.this,"You have selected " + lables.get(which),Toast.LENGTH_LONG).show();
            }
        });
        AlertDialog dialog = builder.create();
        dialog.show();
    }

```

Perhaps, if we don't need any particular `ListView`, we can use a basic way:

```java
AlertDialog.Builder builder = new AlertDialog.Builder(this);
builder.setTitle("Select an item")
       .setItems(R.array.your_array, new DialogInterface.OnClickListener() {
           public void onClick(DialogInterface dialog, int which) {
               // The 'which' argument contains the index position of the selected item
               Log.v(TAG, "Selected item on position " + which);
           }
        });
builder.create().show();

```



## Custom Alert Dialog with EditText


```java
void alertDialogDemo() {
        // get alert_dialog.xml view
        LayoutInflater li = LayoutInflater.from(getApplicationContext());
        View promptsView = li.inflate(R.layout.alert_dialog, null);

        AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(
                getApplicationContext());

        // set alert_dialog.xml to alertdialog builder
        alertDialogBuilder.setView(promptsView);

        final EditText userInput = (EditText) promptsView.findViewById(R.id.etUserInput);

        // set dialog message
        alertDialogBuilder
                .setCancelable(false)
                .setPositiveButton("OK", new DialogInterface.OnClickListener() {
                    public void onClick(DialogInterface dialog, int id) {
                        // get user input and set it to result
                        // edit text
                        Toast.makeText(getApplicationContext(), "Entered: "+userInput.getText().toString(), Toast.LENGTH_LONG).show();
                    }
                })
                .setNegativeButton("Cancel",
                        new DialogInterface.OnClickListener() {
                            public void onClick(DialogInterface dialog, int id) {
                                dialog.cancel();
                            }
                        });

        // create alert dialog
        AlertDialog alertDialog = alertDialogBuilder.create();

        // show it
        alertDialog.show();
    }

```

Xml file: res/layout/alert_dialog.xml



```java
<TextView
    android:id="@+id/textView1"
    android:layout_width="wrap_content"
    android:layout_height="wrap_content"
    android:text="Type Your Message : "
    android:textAppearance="?android:attr/textAppearanceLarge" />

<EditText
    android:id="@+id/etUserInput"
    android:layout_width="match_parent"
    android:layout_height="wrap_content" >

    <requestFocus />

</EditText>

```

[<img src="https://i.stack.imgur.com/wIN7M.png" alt="enter image description here" />](https://i.stack.imgur.com/wIN7M.png)



## Alert Dialog with Multi-line Title


The setCustomTitle() method of AlertDialog.Builder lets you specify an arbitrary view to be used for the dialog title. One common use for this method is to build an alert dialog that has a long title.

```java
AlertDialog.Builder builder = new AlertDialog.Builder(context, Theme_Material_Light_Dialog);
builder.setCustomTitle(inflate(context, R.layout.my_dialog_title, null))
       .setView(inflate(context, R.layout.my_dialog, null))
       .setPositiveButton("OK", null);

Dialog dialog = builder.create();
dialog.show();

```

my_dialog_title.xml:

```java
<?xml version="1.0" encoding="utf-8"?>
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
              android:layout_width="match_parent"
              android:layout_height="match_parent"
              android:padding="16dp">

    <TextView
        style="@android:style/TextAppearance.Small"
        android:layout_width="match_parent"
        android:layout_height="wrap_content"
        android:text="Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur
tincidunt condimentum tristique. Vestibulum ante ante, pretium porttitor
iaculis vitae, congue ut sem. Curabitur ac feugiat ligula. Nulla
tincidunt est eu sapien iaculis rhoncus. Mauris eu risus sed justo
pharetra semper faucibus vel velit."
        android:textStyle="bold"/>

</LinearLayout>

```

my_dialog.xml:

```java
<?xml version="1.0" encoding="utf-8"?>
<ScrollView
    xmlns:android="http://schemas.android.com/apk/res/android"
    android:layout_width="match_parent"
    android:layout_height="match_parent">

    <LinearLayout
        android:layout_width="match_parent"
        android:layout_height="wrap_content"
        android:orientation="vertical"
        android:padding="16dp"
        android:scrollbars="vertical">

        <TextView
            style="@android:style/TextAppearance.Small"
            android:layout_width="match_parent"
            android:layout_height="wrap_content"
            android:paddingBottom="10dp"
            android:text="Hello world!"/>

        <TextView
            style="@android:style/TextAppearance.Small"
            android:layout_width="match_parent"
            android:layout_height="wrap_content"
            android:paddingBottom="10dp"
            android:text="Hello world again!"/>

        <TextView
            style="@android:style/TextAppearance.Small"
            android:layout_width="match_parent"
            android:layout_height="wrap_content"
            android:paddingBottom="10dp"
            android:text="Hello world again!"/>

        <TextView

            style="@android:style/TextAppearance.Small"
            android:layout_width="match_parent"
            android:layout_height="wrap_content"
            android:paddingBottom="10dp"
            android:text="Hello world again!"/>

    </LinearLayout>
</ScrollView>

```

[<img src="https://i.stack.imgur.com/zirHh.png" alt="The alert dialog as rendered:" />](https://i.stack.imgur.com/zirHh.png)



## Date Picker within DialogFragment


xml of the Dialog:

```java
<?xml version="1.0" encoding="utf-8"?>
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
    android:orientation="vertical" android:layout_width="match_parent"
    android:layout_height="match_parent">

    <DatePicker
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:id="@+id/datePicker"
        android:layout_gravity="center_horizontal"
        android:calendarViewShown="false"/>

    <Button
        android:layout_width="match_parent"
        android:layout_height="wrap_content"
        android:text="ACCEPT"
        android:id="@+id/buttonAccept" />

</LinearLayout>

```

Dialog Class:

```java
public class ChooseDate extends DialogFragment implements View.OnClickListener {

    private DatePicker datePicker;
    private Button acceptButton;

    private boolean isDateSetted = false;
    private int year;
    private int month;
    private int day;

    private DateListener listener;

    public interface DateListener {
        onDateSelected(int year, int month, int day);
    }

    public ChooseDate(){}

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View rootView = inflater.inflate(R.layout.dialog_year_picker, container);

        getDialog().setTitle(getResources().getString("TITLE"));

        datePicker = (DatePicker) rootView.findViewById(R.id.datePicker);
        acceptButton = (Button) rootView.findViewById(R.id.buttonAccept);
        acceptButton.setOnClickListener(this);

        if (isDateSetted) {
            datePicker.updateDate(year, month, day);
        }

        return rootView;
    }

    @Override
    public void onClick(View v) {
        switch(v.getId()){
            case R.id.buttonAccept:
                int year = datePicker.getYear();
                int month = datePicker.getMonth() + 1; // months start in 0
                int day = datePicker.getDayOfMonth();
                
                listener.onDateSelected(year, month, day);
                break;
        }
        this.dismiss();
    }

    @Override
    public void onAttach(Context context) {
        super.onAttach(context);
        listener = (DateListener) context;
    }

    public void setDate(int year, int month, int day) {

        this.year = year;
        this.month = month;
        this.day = day;
        this.isDateSetted = true;
    }

}

```

Activity calling the dialog:

```java
public class MainActivity extends AppCompatActivity implements ChooseDate.DateListener{

    private int year;
    private int month;
    private int day;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        
        private void showDateDialog();
    }

    private void showDateDialog(){
        ChooseDate pickDialog = new ChooseDate();
        // We could set a date
        //  pickDialog.setDate(23, 10, 2016);
        pickDialog.show(getFragmentManager(), "");
    }

    @Override
    onDateSelected(int year, int month, int day){
        this.day = day;
        this.month = month;
        this.year = year;
    }
}

```



## Fullscreen Custom Dialog with no background and no title


in `styles.xml` add your custom style:

```java
<?xml version="1.0" encoding="utf-8"?>
<resources>
    <style name="AppBaseTheme" parent="@android:style/Theme.Light.NoTitleBar.Fullscreen">
    </style>
</resources>

```

Create your custom layout for the dialog:
`fullscreen.xml`:

```java
<RelativeLayout xmlns:android="http://schemas.android.com/apk/res/android"
    android:layout_width="match_parent"
    android:layout_height="match_parent" >
    
</RelativeLayout>

```

Then in java file you can use it for an Activity or Dialog etc:

```java
import android.app.Activity;
import android.app.Dialog;
import android.os.Bundle;

public class FullscreenActivity extends Activity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        //You can set no content for the activity.
        Dialog mDialog = new Dialog(this, R.style.AppBaseTheme);
        mDialog.setContentView(R.layout.fullscreen);
        mDialog.show();
    }
}

```



#### Parameters


|Line|Description
|---|---|---|---|---|---|---|---|---|---
|show();|Shows the dialog
|setContentView(R.layout.yourlayout);|sets the ContentView of the dialog to your custom layout.
|dismiss()|Closes the dialog



#### Remarks


<li>
The dialog in the first example(Dialog) does not need to call `show()` when it is created as it is handled in the constructor
</li>
<li>
Alert Dialogs must be constructed through a new instance of the `AlertDialog.Builder()` class. Following the [Builder Pattern](https://en.wikipedia.org/wiki/Builder_pattern), all members of the AlertDialog.Builder can be method chained to 'build up' the dialog instance.
</li>
<li>
The Alert Dialog builder can directly `show()` the dialog -- you do not need to call `create()` then `show()` on the AlertDialog instance
</li>

