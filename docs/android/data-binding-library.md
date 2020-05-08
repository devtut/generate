---
metaTitle: "Android - Data Binding Library"
description: "Basic text field binding, Built-in two-way Data Binding, Custom event using lambda expression, Default value in Data Binding, Binding with an accessor method, Databinding in Dialog, Referencing classes, Databinding in Fragment, Data binding in RecyclerView Adapter, Click listener with Binding, DataBinding with custom variables(int,boolean), Pass widget as reference in BindingAdapter"
---

# Data Binding Library




## Basic text field binding


**Gradle (Module:app) Configuration**

```java
android {
    ....
    dataBinding {
        enabled = true
    }
}

```

**Data model**

```java
public class Item {
    public String name;
    public String description;

    public Item(String name, String description) {
        this.name = name;
        this.description = description;
    }
}

```

**Layout XML**

The first step is wrapping your layout in a `<layout>` tag, adding a `<data>` element, and adding a `<variable>` element for your data model.

Then you can bind XML attributes to fields in the data model using `@{model.fieldname}`, where `model` is the variable's name and `fieldname` is the field you want to access.

**item_detail_activity.xml:**

```java
<?xml version="1.0" encoding="utf-8"?>
<layout xmlns:android="http://schemas.android.com/apk/res/android">
   <data>
       <variable name="item" type="com.example.Item"/>
   </data>

   <LinearLayout
       android:orientation="vertical"
       android:layout_width="match_parent"
       android:layout_height="match_parent">

       <TextView
           android:layout_width="wrap_content"
           android:layout_height="wrap_content"
           android:text="@{item.name}"/>

       <TextView
           android:layout_width="wrap_content"
           android:layout_height="wrap_content"
           android:text="@{item.description}"/>

   </LinearLayout>
</layout>

```

For each XML layout file properly configured with bindings, the Android Gradle plugin generates a corresponding class : bindings.
Because we have a layout named **item_detail_activity**, the corresponding generated binding class is called `ItemDetailActivityBinding`.

This binding can then be used in an Activity like so:

```java
public class ItemDetailActivity extends Activity {
    @Override
    protected void onCreate(Bundle savedInstanceState) {
       super.onCreate(savedInstanceState);
       ItemDetailActivityBinding binding = DataBindingUtil.setContentView(this, R.layout.item_detail_activity);
       Item item = new Item("Example item", "This is an example item.");
       binding.setItem(item);
    }
}

```



## Built-in two-way Data Binding


Two-way Data-Binding supports the following attributes:

|Element|Properties
|---|---|---|---|---|---|---|---|---|---
|`AbsListView`|`android:selectedItemPosition`
|`CalendarView`|`android:date`
|`CompoundButton`|`android:checked`
|`DatePicker`|- `android:year`- `android:month`- `android:day`
|`EditText`|`android:text`
|`NumberPicker`|`android:value`
|`RadioGroup`|`android:checkedButton`
|`RatingBar`|`android:rating`
|`SeekBar`|`android:progress`
|`TabHost`|`android:currentTab`
|`TextView`|`android:text`
|`TimePicker`|- `android:hour`- `android:minute`
|`ToggleButton`|`android:checked`
|`Switch`|`android:checked`

**Usage**

```java
<layout ...>
    <data>
        <variable type="com.example.myapp.User" name="user"/>
    </data>
    <RelativeLayout ...>
        <EditText android:text="@={user.firstName}" .../>
    </RelativeLayout>
</layout>

```

Notice that the Binding expression `@={}` **has an additional `=`**, which is necessary for the **two-way Binding**. It is not possible to use methods in two-way Binding expressions.



## Custom event using lambda expression


**Define Interface**

```java
public interface ClickHandler {
    public void onButtonClick(User user);
}

```

**Create Model class**

```java
public class User {
    private String name;

    public User(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
}

```

**Layout XML**

```java
<layout xmlns:android="http://schemas.android.com/apk/res/android">

    <data>
        <variable
            name="handler"
            type="com.example.ClickHandler"/>

        <variable
            name="user"
            type="com.example.User"/>
    </data>

    <RelativeLayout
        android:layout_width="match_parent"
        android:layout_height="match_parent">

        <Button
            android:layout_width="wrap_content"
            android:layout_height="wrap_content"
            android:text="@{user.name}"
            android:onClick="@{() -> handler.onButtonClick(user)}"/>
    </RelativeLayout>
</layout>

```

**Activity code :**

```java
public class MainActivity extends Activity implements ClickHandler {

    private ActivityMainBinding binding;
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        binding = DataBindingUtil.setContentView(this,R.layout.activity_main);
        binding.setUser(new User("DataBinding User"));
        binding.setHandler(this);
    }

    @Override
    public void onButtonClick(User user) {
        Toast.makeText(MainActivity.this,"Welcome " + user.getName(),Toast.LENGTH_LONG).show();
    }
}

```

For some view listener which is not available in xml code but can be set in java code, it can be bind with custom binding.

**Custom class**

```java
public class BindingUtil {
    @BindingAdapter({"bind:autoAdapter"})
    public static void setAdapter(AutoCompleteTextView view, ArrayAdapter<String> pArrayAdapter) {
        view.setAdapter(pArrayAdapter);
    }
    @BindingAdapter({"bind:onKeyListener"})
    public static void setOnKeyListener(AutoCompleteTextView view , View.OnKeyListener pOnKeyListener)
    {
        view.setOnKeyListener(pOnKeyListener);
    }
}

```

**Handler class**

```java
public class Handler extends BaseObservable {
    private ArrayAdapter<String> roleAdapter;
       
    public ArrayAdapter<String> getRoleAdapter() {
        return roleAdapter;
    }
    public void setRoleAdapter(ArrayAdapter<String> pRoleAdapter) {
        roleAdapter = pRoleAdapter;
    }
}

```

**XML**

```java
<layout 
    xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:bind="http://schemas.android.com/tools" >

    <data>
        <variable
            name="handler"
            type="com.example.Handler" />
    </data>

    <LinearLayout
        android:layout_width="match_parent"
        android:layout_height="match_parent"
        android:orientation="vertical" >

        <AutoCompleteTextView
            android:layout_width="match_parent"
            android:layout_height="wrap_content"
            android:singleLine="true"
            bind:autoAdapter="@{handler.roleAdapter}" />

    </LinearLayout>
</layout>

```



## Default value in Data Binding


> 
<p>The Preview pane displays default values for data binding expressions
if provided.</p>


For example :

```java
android:layout_height="@{@dimen/main_layout_height, default=wrap_content}"

```

It will take `wrap_content` while designing and will act as a `wrap_content` in preview pane.

Another example is

```java
android:text="@{user.name, default=`Preview Text`}"

```

It will display `Preview Text` in preview pane but when you run it in device/emulator actual text binded to it will be displayed



## Binding with an accessor method


If your model has private methods, the databinding library still allows you to access them in your view without using the full name of the method.

**Data model**

```java
public class Item {
    private String name;

    public String getName() {
        return name;
    }
}

```

**Layout XML**

```java
<?xml version="1.0" encoding="utf-8"?>
<layout xmlns:android="http://schemas.android.com/apk/res/android">
   <data>
       <variable name="item" type="com.example.Item"/>
   </data>

   <LinearLayout
       android:orientation="vertical"
       android:layout_width="match_parent"
       android:layout_height="match_parent">

       <!-- Since the "name" field is private on our data model,
            this binding will utilize the public getName() method instead. -->
       <TextView
           android:layout_width="wrap_content"
           android:layout_height="wrap_content"
           android:text="@{item.name}"/>

   </LinearLayout>
</layout>

```



## Databinding in Dialog


```java
public void doSomething() {
    DialogTestBinding binding = DataBindingUtil
            .inflate(LayoutInflater.from(context), R.layout.dialog_test, null, false);

    Dialog dialog = new Dialog(context);
    dialog.setContentView(binding.getRoot());
    dialog.show();
}

```



## Referencing classes


**Data model**

```java
public class Item {
    private String name;

    public String getName() {
        return name;
    }
}

```

**Layout XML**

You must import referenced classes, just as you would in Java.

```java
<?xml version="1.0" encoding="utf-8"?>
<layout xmlns:android="http://schemas.android.com/apk/res/android">
   <data>
       <import type="android.view.View"/>
       <variable name="item" type="com.example.Item"/>
   </data>

   <LinearLayout
       android:orientation="vertical"
       android:layout_width="match_parent"
       android:layout_height="match_parent">

       <!-- We reference the View class to set the visibility of this TextView -->
       <TextView
           android:layout_width="wrap_content"
           android:layout_height="wrap_content"
           android:text="@{item.name}"
           android:visibility="@{item.name == null ? View.VISIBLE : View.GONE"/>

   </LinearLayout>
</layout>

```

**Note:** The package `java.lang.*` is imported automatically by the system. (The same is made by `JVM` for `Java`)



## Databinding in Fragment


**Data Model**

```java
public class Item {
    private String name;

    public String getName() {
        return name;
    }

    public void setName(String name){
        this.name = name;
    }

}

```

**Layout XML**

```java
<?xml version="1.0" encoding="utf-8"?>
<layout xmlns:android="http://schemas.android.com/apk/res/android">
   <data>
       <variable name="item" type="com.example.Item"/>
   </data>

   <LinearLayout
       android:orientation="vertical"
       android:layout_width="match_parent"
       android:layout_height="match_parent">

       <TextView
           android:layout_width="wrap_content"
           android:layout_height="wrap_content"
           android:text="@{item.name}"/>

   </LinearLayout>
</layout>

```

**Fragment**

```java
@Override
public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
    FragmentTest binding = DataBindingUtil.inflate(inflater, R.layout.fragment_test, container, false);
    Item item = new Item();
    item.setName("Thomas");
    binding.setItem(item);
    return binding.getRoot();
}

```



## Data binding in RecyclerView Adapter


It's also possible to use data binding within your `RecyclerView` Adapter.

### Data model

```java
public class Item {
    private String name;

    public String getName() {
        return name;
    }
}

```

### XML Layout

```java
<TextView
       android:layout_width="wrap_content"
       android:layout_height="wrap_content"
       android:text="@{item.name}"/>

```

### Adapter class

```java
public class ListItemAdapter extends RecyclerView.Adapter<RecyclerView.ViewHolder> {

    private Activity host;
    private List<Item> items;

    public ListItemAdapter(Activity activity, List<Item> items) {
        this.host = activity;
        this.items = items;
    }

    @Override
    public RecyclerView.ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        // inflate layout and retrieve binding
        ListItemBinding binding = DataBindingUtil.inflate(host.getLayoutInflater(),
                R.layout.list_item, parent, false);

        return new ItemViewHolder(binding);
    }

    @Override
    public void onBindViewHolder(RecyclerView.ViewHolder holder, int position) {
        Item item = items.get(position);

        ItemViewHolder itemViewHolder = (ItemViewHolder)holder;
        itemViewHolder.bindItem(item);
    }

    @Override
    public int getItemCount() {
        return items.size();
    }

    private static class ItemViewHolder extends RecyclerView.ViewHolder {
        ListItemBinding binding;

        ItemViewHolder(ListItemBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
        }

        void bindItem(Item item) {
            binding.setItem(item);
            binding.executePendingBindings();
        }
    }
}

```



## Click listener with Binding


**Create interface for clickHandler**

```java
public interface ClickHandler {
    public void onButtonClick(View v);
}

```

**Layout XML**

```java
<?xml version="1.0" encoding="utf-8"?>
<layout xmlns:android="http://schemas.android.com/apk/res/android">

    <data>
        <variable
            name="handler"
            type="com.example.ClickHandler"/>
    </data>

    <RelativeLayout
        android:layout_width="match_parent"
        android:layout_height="match_parent">

        <Button
            android:layout_width="wrap_content"
            android:layout_height="wrap_content"
            android:text="click me"
            android:onClick="@{handler.onButtonClick}"/>
    </RelativeLayout>
</layout>

```

**Handle event in your Activity**

```java
public class MainActivity extends Activity implements ClickHandler {

    private ActivityMainBinding binding;
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        binding = DataBindingUtil.setContentView(this,R.layout.activity_main);
        binding.setHandler(this);
    }

    @Override
    public void onButtonClick(View v) {
        Toast.makeText(context,"Button clicked",Toast.LENGTH_LONG).show();
    }
}

```



## DataBinding with custom variables(int,boolean)


Sometimes we need to perform basic operations like hide/show view based on single value, for that single variable we cannot create model or it is not good practice to create model for that. DataBinding supports basic datatypes to perform those oprations.

```java
<layout xmlns:android="http://schemas.android.com/apk/res/android">

    <data>

        <import type="android.view.View" />

        <variable
            name="selected"
            type="Boolean" />

    </data>

    <RelativeLayout
        android:layout_width="match_parent"
        android:layout_height="match_parent">

        <TextView
            android:layout_width="wrap_content"
            android:layout_height="wrap_content"
            android:text="Hello World"
            android:visibility="@{selected ? View.VISIBLE : View.GONE}" />

    </RelativeLayout>
</layout>

```

and set its value from java class.

```java
binding.setSelected(true);

```



## Pass widget as reference in BindingAdapter


Layout XML

```java
<?xml version="1.0" encoding="utf-8"?>
<layout xmlns:android="http://schemas.android.com/apk/res/android">
   <data>
   
   </data>

   <LinearLayout
       android:orientation="vertical"
       android:layout_width="match_parent"
       android:layout_height="match_parent">

       <ProgressBar
        android:id="@+id/progressBar"
        style="?android:attr/progressBarStyleSmall"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"/>

        <ImageView
        android:id="@+id/img"
        android:layout_width="match_parent"
        android:layout_height="100dp"
        app:imageUrl="@{url}"
        app:progressbar="@{progressBar}"/>

   </LinearLayout>
</layout>

```

BindingAdapter method

```java
@BindingAdapter({"imageUrl","progressbar"})
public static void loadImage(ImageView view, String imageUrl, ProgressBar progressBar){
    Glide.with(view.getContext()).load(imageUrl)
                        .listener(new RequestListener<String, GlideDrawable>() {
                    @Override
                    public boolean onException(Exception e, String model, Target<GlideDrawable> target, boolean isFirstResource) {
                        return false;
                    }

                    @Override
                    public boolean onResourceReady(GlideDrawable resource, String model, Target<GlideDrawable> target, boolean isFromMemoryCache, boolean isFirstResource) {
                        progressBar.setVisibility(View.GONE);
                        return false;
                    }
                }).into(view);
}

```



#### Remarks


**Setup**

Before using data binding, you must enable the plugin in your `build.gradle`.

```java
android {
    ....
    dataBinding {
        enabled = true
    }
}

```

**Note: Data binding was added to the Android Gradle plugin in version 1.5.0**

**Binding class names**

The data binding plugin generates a binding class name by converting your layout's file name to Pascal case and adding "Binding" to the end. Thus `item_detail_activity.xml` will generate a class named `ItemDetailActivityBinding`.

**Resources**

- [Official documentation](https://developer.android.com/topic/libraries/data-binding/index.html)

