---
metaTitle: "Android - Date and Time Pickers"
description: "Material DatePicker, Date Picker Dialog"
---

# Date and Time Pickers



## Material DatePicker


add below dependencies to `build.gradle` file in dependency section. (this is an unOfficial library for date picker)

```

 compile 'com.wdullaer:materialdatetimepicker:2.3.0'

```

Now we have to open `DatePicker` on Button click event.

So create one Button on xml file like below.

```

    <Button
            android:id="@+id/dialog_bt_date"
            android:layout_below="@+id/resetButton"
            android:layout_width="wrap_content"
            android:layout_height="40dp"
            android:textColor="#FF000000"
            android:gravity="center"
            android:text="DATE"/>

```

and in MainActivity use this way.

```java
public class MainActivity extends AppCompatActivity implements DatePickerDialog.OnDateSetListener{

    Button button;
    Calendar calendar ;
    DatePickerDialog datePickerDialog ;
    int Year, Month, Day ;


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        calendar = Calendar.getInstance();

        Year = calendar.get(Calendar.YEAR) ;
        Month = calendar.get(Calendar.MONTH);
        Day = calendar.get(Calendar.DAY_OF_MONTH);


        Button dialog_bt_date = (Button)findViewById(R.id.dialog_bt_date);
        dialog_bt_date.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {

                datePickerDialog = DatePickerDialog.newInstance(MainActivity.this, Year, Month, Day);

                datePickerDialog.setThemeDark(false);

                datePickerDialog.showYearPickerFirst(false);

                datePickerDialog.setAccentColor(Color.parseColor("#0072BA"));

                datePickerDialog.setTitle("Select Date From DatePickerDialog");

                datePickerDialog.show(getFragmentManager(), "DatePickerDialog");

            }
        });
    }

    @Override
    public void onDateSet(DatePickerDialog view, int Year, int Month, int Day) {

        String date = "Selected Date : " + Day + "-" + Month + "-" + Year;

        Toast.makeText(MainActivity.this, date, Toast.LENGTH_LONG).show();
    }
    @Override
    public boolean onCreateOptionsMenu(Menu menu)
    {
        getMenuInflater().inflate(R.menu.abc_main_menu, menu);
        return true;
    }

}

```

Output :

[<img src="http://i.stack.imgur.com/N5p0D.png" alt="enter image description here" />](http://i.stack.imgur.com/N5p0D.png)



## Date Picker Dialog


It is a dialog which prompts user to select date using `DatePicker`. The dialog requires context, initial year, month and day to show the dialog with starting date.  When the user selects the date it callbacks via `DatePickerDialog.OnDateSetListener`.

```java
public void showDatePicker(Context context,int initialYear, int initialMonth, int initialDay) {
       DatePickerDialog datePickerDialog = new DatePickerDialog(context,
                new DatePickerDialog.OnDateSetListener() {
                    @Override
                    public void onDateSet(DatePicker datepicker,int year ,int month, int day) {
                    //this condition is necessary to work properly on all android versions
                    if(view.isShown()){
                            //You now have the selected year, month and day
                    } 

}
                }, initialYear, initialMonth , initialDay);

        //Call show() to simply show the dialog
        datePickerDialog.show();

    }

```

Please note that month is a int starting from 0 for January to 11 for December

