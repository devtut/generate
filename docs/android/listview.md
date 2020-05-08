---
metaTitle: "Android - ListView"
description: "Custom ArrayAdapter, A basic ListView with an ArrayAdapter, Filtering with CursorAdapter"
---

# ListView


ListView is a viewgroup which groups several items from a data source like array or database and displays them in a scroll-able list. Data are bound with listview using an Adapter class.



## Custom ArrayAdapter


[By default](http://stackoverflow.com/documentation/android/4226/listview/22771/a-basic-listview-with-an-arrayadapter#t=201609130731516972147) the ArrayAdapter class creates a view for each array item by calling `toString()` on each item and placing the contents in a TextView.

To create a complex view for each item (for example, if you want an ImageView for each array item), extend the ArrayAdapter class and override the `getView()` method to return the type of View you want for each item.

For example:

```java
public class MyAdapter extends ArrayAdapter<YourClassData>{

    private LayoutInflater inflater;

    public MyAdapter (Context context, List<YourClassData> data){
        super(context, 0, data);
        inflater = LayoutInflater.from(context);
    }

    @Override
    public long getItemId(int position)
    {
        //It is just an example
        YourClassData data = (YourClassData) getItem(position);
        return data.ID;
    }

    @Override
    public View getView(int position, View view, ViewGroup parent)
    {
        ViewHolder viewHolder;
        if (view == null) {
            view = inflater.inflate(R.layout.custom_row_layout_design, null);
            // Do some initialization
        
            //Retrieve the view on the item layout and set the value.
            viewHolder = new ViewHolder(view);
            view.setTag(viewHolder);
         }
         else {
             viewHolder = (ViewHolder) view.getTag();
         }
         
        //Retrieve your object    
        YourClassData data = (YourClassData) getItem(position);
       
        viewHolder.txt.setTypeface(m_Font);    
        viewHolder.txt.setText(data.text);              
        viewHolder.img.setImageBitmap(BitmapFactory.decodeFile(data.imageAddr));
        
        return view;
    
    }

    private class ViewHolder
    {
         private final TextView txt;
         private final ImageView img;

         private ViewHolder(View view) 
         {
             txt = (TextView) view.findViewById(R.id.txt);
             img = (ImageView) view.findViewById(R.id.img);
         }
    }
}

```



## A basic ListView with an ArrayAdapter


By default the [`ArrayAdapter`](https://developer.android.com/reference/android/widget/ArrayAdapter.html) creates a view for each array item by calling `toString()` on each item and placing the contents in a `TextView`.

Example:

```java
ArrayAdapter<String> adapter = new ArrayAdapter<String>(this,
        android.R.layout.simple_list_item_1, myStringArray);

```

where `android.R.layout.simple_list_item_1` is the layout that contains a `TextView` for each string in the array.

Then simply call `setAdapter()` on your `ListView`:

```java
ListView listView = (ListView) findViewById(R.id.listview);
listView.setAdapter(adapter);

```

To use something other than TextViews for the array display, for instance, ImageViews, or to have some of data besides `toString()` results fill the views, override `getView(int, View, ViewGroup)` to return the type of view you want. [Check this example](http://stackoverflow.com/documentation/android/4226/listview/18297/custom-array-adapter#t=201609130742332734311).



## Filtering with CursorAdapter


```java
// Get the reference to your ListView
ListView listResults = (ListView) findViewById(R.id.listResults);

// Set its adapter
listResults.setAdapter(adapter);

// Enable filtering in ListView
listResults.setTextFilterEnabled(true);

// Prepare your adapter for filtering    
adapter.setFilterQueryProvider(new FilterQueryProvider() {
    @Override
    public Cursor runQuery(CharSequence constraint) {

        // in real life, do something more secure than concatenation
        // but it will depend on your schema
        // This is the query that will run on filtering
        String query = "SELECT _ID as _id, name FROM MYTABLE "
                       + "where name like '%" + constraint + "%' "
                       + "ORDER BY NAME ASC";
        return db.rawQuery(query, null);
     }
});

```

Let's say your query will run every time the user types in an `EditText`:

```

   EditText queryText = (EditText) findViewById(R.id.textQuery);
    queryText.addTextChangedListener(new TextWatcher() {
        @Override
        public void beforeTextChanged(final CharSequence s, final int start, final int count, final int after) {

        }

        @Override
        public void onTextChanged(final CharSequence s, final int start, final int before, final int count) {
            // This is the filter in action
            adapter.getFilter().filter(s.toString());
            // Don't forget to notify the adapter
            adapter.notifyDataSetChanged();
        }

        @Override
        public void afterTextChanged(final Editable s) {

        }
    });

```



#### Remarks


[`ListView`](https://developer.android.com/reference/android/widget/ListView.html) is a view group that displays a list of scrollable items.<br />
The list items are automatically inserted to the list using an [`Adapter`](https://developer.android.com/reference/android/widget/Adapter.html) that pulls content from a source such as an array or database query and converts each item result into a view that's placed into the list.

When the content for your layout is dynamic or not pre-determined, you can use a layout that subclasses [`AdapterView`](https://developer.android.com/reference/android/widget/AdapterView.html) to populate the layout with views at runtime. A subclass of the `AdapterView` class uses an [`Adapter`](https://developer.android.com/reference/android/widget/Adapter.html) to bind data to its layout.

Before using the `ListView` you should also checking the [`RecyclerView`](http://stackoverflow.com/documentation/android/169/recyclerview#t=201609130728062984881) examples.

