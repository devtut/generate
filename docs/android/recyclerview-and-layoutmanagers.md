---
metaTitle: "Android - RecyclerView and LayoutManagers"
description: "Adding header view to recyclerview with gridlayout manager, GridLayoutManager with dynamic span count, Simple list with LinearLayoutManager, StaggeredGridLayoutManager"
---

# RecyclerView and LayoutManagers



## Adding header view to recyclerview with gridlayout manager


To add a header to a recyclerview with a gridlayout, first the adapter needs to be told  that the header view is the first position - rather than the standard cell used for the content. Next, the layout manager must be told that the first position should have a span equal to the *span count of the entire list. *

Take a regular RecyclerView.Adapter class and configure it as follows:

```java
public class HeaderAdapter extends RecyclerView.Adapter<RecyclerView.ViewHolder> {

    private static final int ITEM_VIEW_TYPE_HEADER = 0;
    private static final int ITEM_VIEW_TYPE_ITEM = 1;

    private List<YourModel> mModelList;

    public HeaderAdapter (List<YourModel> modelList) {
        mModelList = modelList;
    }

    public boolean isHeader(int position) {
        return position == 0;
    }

    @Override
    public RecyclerView.ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        LayoutInflater inflater = LayoutInflater.from(parent.getContext());

        if (viewType == ITEM_VIEW_TYPE_HEADER) {
            View headerView = inflater.inflate(R.layout.header, parent, false);
            return new HeaderHolder(headerView);
        }

        View cellView = inflater.inflate(R.layout.gridcell, parent, false);
        return new ModelHolder(cellView);
    }

    @Override
    public int getItemViewType(int position) {
        return isHeader(position) ? ITEM_VIEW_TYPE_HEADER : ITEM_VIEW_TYPE_ITEM;
    }

    @Override
    public void onBindViewHolder(RecyclerView.ViewHolder h, int position) {
        if (isHeader(position)) {
            return;
        }

        final YourModel model = mModelList.get(position -1 ); // Subtract 1 for header

        ModelHolder holder = (ModelHolder) h;
        // populate your holder with data from your model as usual
    }

    @Override
    public int getItemCount() {
        return _categories.size() + 1; // add one for the header
    }
}

```

Then in the activity/fragment:

```java
final HeaderAdapter adapter = new HeaderAdapter (mModelList);
final GridLayoutManager manager = new GridLayoutManager(); 
manager.setSpanSizeLookup(new GridLayoutManager.SpanSizeLookup() {
    @Override
    public int getSpanSize(int position) {
        return adapter.isHeader(position) ? manager.getSpanCount() : 1;
    }
});
mRecyclerView.setLayoutManager(manager);
mRecyclerView.setAdapter(adapter);

```

The same approach can be used add a footer in addition to or instead of a header.

Source: [Chiu-Ki Chan's Square Island blog](http://blog.sqisland.com/2014/12/recyclerview-grid-with-header.html)



## GridLayoutManager with dynamic span count


When creating a recyclerview with a gridlayout layout manager you have to specify the span count in the constructor. Span count refers to the number of columns. This is fairly clunky and doesn't take into account larger screen sizes or screen orientation. One approach is to create multiple layouts for the various screen sizes. Another more dynamic approach can be seen below.

First we create a custom RecyclerView class as follows:

```java
public class AutofitRecyclerView extends RecyclerView {
    private GridLayoutManager manager;
    private int columnWidth = -1;

    public AutofitRecyclerView(Context context) {
        super(context);
        init(context, null);
    }

    public AutofitRecyclerView(Context context, AttributeSet attrs) {
        super(context, attrs);
        init(context, attrs);
    }

    public AutofitRecyclerView(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        init(context, attrs);
    }

    private void init(Context context, AttributeSet attrs) {
        if (attrs != null) {
            int[] attrsArray = {
                    android.R.attr.columnWidth
            };
            TypedArray array = context.obtainStyledAttributes(attrs, attrsArray);
            columnWidth = array.getDimensionPixelSize(0, -1);
            array.recycle();
        }

        manager = new GridLayoutManager(getContext(), 1);
        setLayoutManager(manager);
    }

    @Override
    protected void onMeasure(int widthSpec, int heightSpec) {
        super.onMeasure(widthSpec, heightSpec);
        if (columnWidth > 0) {
            int spanCount = Math.max(1, getMeasuredWidth() / columnWidth);
            manager.setSpanCount(spanCount);
        }
    }
}

```

This class determines how many columns can fit into the recyclerview. To use it you will need to put it into your layout.xml as follows:

```java
<?xml version="1.0" encoding="utf-8"?>
<com.path.to.your.class.autofitRecyclerView.AutofitRecyclerView
    xmlns:android="http://schemas.android.com/apk/res/android"
    android:id="@+id/auto_fit_recycler_view"
    android:layout_width="match_parent"
    android:layout_height="match_parent"
    android:columnWidth="200dp"
    android:clipToPadding="false"
    />

```

Notice that we use the columnWidth attribute. The recyclerview will need it to determine how many columns will fit into the available space.

In your activity/fragment you just get a reference to the recylerview and set an adapter to it (and any item decorations or animations that you want to add). **DO NOT SET A LAYOUT MANAGER**

```java
RecyclerView recyclerView = (RecyclerView) findViewById(R.id.auto_fit_recycler_view);
recyclerView.setAdapter(new MyAdapter());

```

(where MyAdapter is your adapter class)

You now have a recyclerview that will adjust the spancount (ie columns) to fit the screen size. As a final addition you might want to center the columns in the recyclerview (by default they are aligned to layout_start). You can do that by modifying the AutofitRecyclerView class a little. Start by creating an inner class in the recyclerview. This will be a class that extends from GridLayoutManager. It will add enough padding to the left and right in order to center the rows:

```java
public class AutofitRecyclerView extends RecyclerView {

    // etc see above

    private class CenteredGridLayoutManager extends GridLayoutManager {

        public CenteredGridLayoutManager(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
            super(context, attrs, defStyleAttr, defStyleRes);
        }

        public CenteredGridLayoutManager(Context context, int spanCount) {
            super(context, spanCount);
        }

        public CenteredGridLayoutManager(Context context, int spanCount, int orientation, boolean reverseLayout) {
            super(context, spanCount, orientation, reverseLayout);
        }

        @Override
        public int getPaddingLeft() {
            final int totalItemWidth = columnWidth * getSpanCount();
            if (totalItemWidth >= AutofitRecyclerView.this.getMeasuredWidth()) {
                return super.getPaddingLeft(); // do nothing
            } else {
                return Math.round((AutofitRecyclerView.this.getMeasuredWidth() / (1f + getSpanCount())) - (totalItemWidth / (1f + getSpanCount())));
            }
        }

        @Override
        public int getPaddingRight() {
            return getPaddingLeft();
        }
    }
}

```

Then when you set the LayoutManager in the AutofitRecyclerView use the CenteredGridLayoutManager as follows:

```java
private void init(Context context, AttributeSet attrs) {
    if (attrs != null) {
        int[] attrsArray = {
                android.R.attr.columnWidth
        };
        TypedArray array = context.obtainStyledAttributes(attrs, attrsArray);
        columnWidth = array.getDimensionPixelSize(0, -1);
        array.recycle();
    }

    manager = new CenteredGridLayoutManager(getContext(), 1);
    setLayoutManager(manager);
}

```

And that's it! You have a dynamic spancount, center aligned gridlayoutmanager based recyclerview.

Sources:

- [Chiu-Ki Chan's Square Island blog](http://blog.sqisland.com/2014/12/recyclerview-autofit-grid.html)
- [StackOverflow](http://stackoverflow.com/questions/29117916/android-centering-item-in-recyclerview/30118826)



## Simple list with LinearLayoutManager


This example adds a list of places with image and name by using an `ArrayList` of custom `Place` objects as dataset.

### Activity layout

The layout of the activity / fragment or where the RecyclerView is used only has to contain the RecyclerView. There is no ScrollView or a specific layout needed.

```java
<?xml version="1.0" encoding="utf-8"?>
<RelativeLayout xmlns:android="http://schemas.android.com/apk/res/android"
    android:layout_width="match_parent"
    android:layout_height="match_parent">

    <android.support.v7.widget.RecyclerView
        android:id="@+id/my_recycler_view"
        android:layout_width="match_parent"
        android:layout_height="match_parent" />

</RelativeLayout>

```

### Define the data model

You could use any class or primitive data type as a model, like `int`, `String`, `float[]` or `CustomObject`. The RecyclerView will refer to a `List` of this objects / primitives.

When a list item refers to different data types like text, numbers, images (as in this example with places), it is often a good idea to use a custom object.

```java
public class Place {
    // these fields will be shown in a list item
    private Bitmap image;
    private String name;

    // typical constructor
    public Place(Bitmap image, String name) {
        this.image = image;
        this.name = name;
    }

    // getters
    public Bitmap getImage() {
        return image;
    }
    public String getName() {
        return name;
    } 
}

```

### List item layout

You have to specify a xml layout file that will be used for each list item. In this example, an `ImageView` is used for the image and a `TextView` for the name. The `LinearLayout` positions the `ImageView` at the left and the `TextView` right to the image.

```java
<?xml version="1.0" encoding="utf-8"?>
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
    android:layout_width="match_parent"
    android:layout_height="wrap_content"
    android:gravity="center_vertical"
    android:orientation="horizontal"
    android:padding="8dp">

    <ImageView
        android:id="@+id/image"
        android:layout_width="36dp"
        android:layout_height="36dp"
        android:layout_marginEnd="8dp"
        android:layout_marginRight="8dp" />

    <TextView
        android:id="@+id/name"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content" />

</LinearLayout>

```

### Create a RecyclerView adapter and ViewHolder

Next, you have to inherit the `RecyclerView.Adapter` and the `RecyclerView.ViewHolder`. A usual class structure would be:

```java
public class PlaceListAdapter extends RecyclerView.Adapter<PlaceListAdapter.ViewHolder> {
    // ...

    public class ViewHolder extends RecyclerView.ViewHolder {
        // ...
    }
}

```

First, we implement the `ViewHolder`. It only inherits the default constructor and saves the needed views into some fields:

```java
public class ViewHolder extends RecyclerView.ViewHolder {
    private ImageView imageView;
    private TextView nameView;

    public ViewHolder(View itemView) {
        super(itemView);

        imageView = (ImageView) itemView.findViewById(R.id.image);
        nameView = (TextView) itemView.findViewById(R.id.name);
    }
}

```

The adapter's constructor sets the used dataset:

```java
public class PlaceListAdapter extends RecyclerView.Adapter<PlaceListAdapter.ViewHolder> {
    private List<Place> mPlaces;

    public PlaceListAdapter(List<Place> contacts) {
        mPlaces = contacts;
    }

    // ...
}

```

To use our custom list item layout, we override the method `onCreateViewHolder(...)`. In this example, the layout file is called `place_list_item.xml`.

```java
public class PlaceListAdapter extends RecyclerView.Adapter<PlaceListAdapter.ViewHolder> {
    // ...

    @Override
    public ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(
                R.layout.place_list_item,
                parent,
                false
        );
        return new ViewHolder(view);
    }

    // ...
}

```

In the `onBindViewHolder(...)`, we actually set the views' contents. We get the used model by finding it in the `List` at the given position and then set image and name on the `ViewHolder`'s views.

```java
public class PlaceListAdapter extends RecyclerView.Adapter<PlaceListAdapter.ViewHolder> {
    // ...

    @Override
    public void onBindViewHolder(PlaceListAdapter.ViewHolder viewHolder, int position) {
        Place place = mPlaces.get(position);

        viewHolder.nameView.setText(place.getName());
        viewHolder.imageView.setImageBitmap(place.getImage());
    }

    // ...
}

```

We also need to implement `getItemCount()`, which simply return the `List`'s size.

```java
public class PlaceListAdapter extends RecyclerView.Adapter<PlaceListAdapter.ViewHolder> {
    // ...

    @Override
    public int getItemCount() {
        return mPlaces.size();
    }

    // ...
}

```

### (Generate random data)

For this example, we'll generate some random places.

```java
@Override
protected void onCreate(Bundle savedInstanceState) {
    // ...

    List<Place> places = randomPlaces(5);

    // ...
}

private List<Place> randomPlaces(int amount) {
    List<Place> places = new ArrayList<>();
    for (int i = 0; i < amount; i++) {
        places.add(new Place(
                BitmapFactory.decodeResource(getResources(), Math.random() > 0.5 ?
                        R.drawable.ic_account_grey600_36dp :
                        R.drawable.ic_android_grey600_36dp
                ),
                "Place #" + (int) (Math.random() * 1000)
        ));
    }
    return places;
}

```

### Connect the RecyclerView with the PlaceListAdapter and the dataset

Connecting a `RecyclerView` with an adapter is very easy. You have to set the `LinearLayoutManager` as layout manager to achieve the list layout.

```java
@Override
protected void onCreate(Bundle savedInstanceState) {
    // ...

    RecyclerView recyclerView = (RecyclerView) findViewById(R.id.my_recycler_view);
    recyclerView.setAdapter(new PlaceListAdapter(places));
    recyclerView.setLayoutManager(new LinearLayoutManager(this));
}

```

### Done!



## StaggeredGridLayoutManager


1. Create your RecyclerView in your layout xml file:

```java
<android.support.v7.widget.RecyclerView
            android:id="@+id/recycleView"
            android:layout_width="match_parent"
            android:layout_height="match_parent" />

```


<li>
Create your Model class for holding your data:

```java
     public class PintrestItem {
     String url;
     public PintrestItem(String url,String name){
         this.url=url;
         this.name=name;
     }
     public String getUrl() {
         return url;
     }
 
    public String getName(){
        return name;
    }
     String name;
 }  

```


</li>
<li>
Create a layout file to hold RecyclerView items:

```java
<ImageView
    android:layout_width="match_parent"
    android:layout_height="wrap_content"
    android:adjustViewBounds="true"
    android:scaleType="centerCrop"
    android:id="@+id/imageView"/>
    <TextView
        android:layout_width="match_parent"
        android:layout_height="wrap_content"
        android:gravity="center"
        android:id="@+id/name"
        android:layout_gravity="center"
        android:textColor="@android:color/white"/>

```


</li>
<li>
Create the adapter class for the RecyclerView:

```java
 public class PintrestAdapter extends  RecyclerView.Adapter<PintrestAdapter.PintrestViewHolder>{
    private ArrayList<PintrestItem>images;
    Picasso picasso;
    Context context;
    public PintrestAdapter(ArrayList<PintrestItem>images,Context context){
        this.images=images;
        picasso=Picasso.with(context);
        this.context=context;

    }

    @Override
    public PintrestViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        View view= LayoutInflater.from(parent.getContext()).inflate(R.layout.pintrest_layout_item,parent,false);
        return new PintrestViewHolder(view);
    }

    @Override
    public void onBindViewHolder(PintrestViewHolder holder, int position) {
          picasso.load(images.get(position).getUrl()).into(holder.imageView);
          holder.tv.setText(images.get(position).getName());
    }

    @Override
    public int getItemCount() {
        return images.size();
    }

    public class PintrestViewHolder extends RecyclerView.ViewHolder{
        ImageView imageView;
        TextView tv;
        public PintrestViewHolder(View itemView) {
            super(itemView);
            imageView=(ImageView)itemView.findViewById(R.id.imageView);
            tv=(TextView)itemView.findViewById(R.id.name);
            
        }
    }
}

```


</li>

<li>
Instantiate the RecyclerView in your activity or fragment:

```java
RecyclerView recyclerView = (RecyclerView)findViewById(R.id.recyclerView);
//Create the instance of StaggeredGridLayoutManager with 2 rows i.e the span count and provide the orientation
StaggeredGridLayoutManager layoutManager=new new StaggeredGridLayoutManager(2, StaggeredGridLayoutManager.VERTICAL);
recyclerView.setLayoutManager(layoutManager);
// Create Dummy Data and Add to your List<PintrestItem>
List<PintrestItem>items=new ArrayList<PintrestItem>
items.add(new PintrestItem("url of image you want to show","imagename"));
items.add(new PintrestItem("url of image you want to show","imagename"));
items.add(new PintrestItem("url of image you want to show","imagename"));
recyclerView.setAdapter(new PintrestAdapter(items,getContext() );

```


</li>

Don't forgot to add the Picasso dependency in your build.gradle file:

```java
compile 'com.squareup.picasso:picasso:2.5.2'

```

